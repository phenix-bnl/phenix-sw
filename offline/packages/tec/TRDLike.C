#include "TRDLike.hh"

#include "RunToTime.hh"
#include "PHTimeStamp.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbTRDlike.hh"

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
using namespace std;

//_____________________________________________________________________________
TRDLike::TRDLike(const string &namein, size_t ind)
{
  nbins = 0;
  trdvar = TRDvar::ERR;
  if (!strcmp(namein.c_str(),"DE")) trdvar = TRDvar::DE;
  if (!strcmp(namein.c_str(),"TR")) trdvar = TRDvar::TR;
  if (!strcmp(namein.c_str(),"NHITS")) trdvar = TRDvar::NHITS;
  if (!strcmp(namein.c_str(),"NTR")) trdvar = TRDvar::NTR;
  if (!strcmp(namein.c_str(),"WTB")) trdvar = TRDvar::WTB;
  if (!strcmp(namein.c_str(),"WeightedTimeBin")) trdvar = TRDvar::WTB;
   if (!strcmp(namein.c_str(),"WTB")) trdvar = TRDvar::WTB;
 if (trdvar == TRDvar::ERR)
    std::cout << "TRDLike:: wrong variable name!!!" << endl;
  index = ind;
}

//_____________________________________________________________________________
TRDLike::TRDLike(const size_t var, size_t i)
{
  trdvar = var;
  index = i;
  nbins = 0;
}

//_____________________________________________________________________________
TRDLike::~TRDLike()
{
  likebin.clear();
}

//_____________________________________________________________________________
int
TRDLike::FetchFromFile()
{
  ostringstream filename;
  filename << "/afs/rhic.bnl.gov/phenix/users/slash/calibration/run5pp/tec/h" << Name() << index << ".dat";
  FetchFromFile(filename.str());
  return 0;
}

//_____________________________________________________________________________
string 
TRDLike::Name()
{
  string varname;
  switch (trdvar)
    {
    case TRDvar::DE :    varname = "DE";              break;
    case TRDvar::TR :    varname = "TR";              break;
    case TRDvar::NHITS : varname = "NHITS";           break;
    case TRDvar::NTR :   varname = "NTR";             break;
    case TRDvar::WTB :   varname = "WTB"; break;
    }
  return varname;
}

//_____________________________________________________________________________
int
TRDLike::FetchFromFile(const string &filename)
{
  likerange liketmp;
  ifstream fread(filename.c_str());
  if (!fread)
    {
      cout << filename << " doesn't exist" << endl;
      return 0;
    }
  while (fread >> liketmp.min >> liketmp.max >> liketmp.prob_e >> liketmp.prob_p)
    {
      likebin.push_back(liketmp);
      nbins ++;
    }
  fread.close();
  return 0;
}

//_____________________________________________________________________________
void
TRDLike::setBin(float min, float max, float pe, float pp)
{
  likerange liketmp;
  liketmp.min = min;
  liketmp.max = max;
  liketmp.prob_e = pe;
  liketmp.prob_p = pp;
  likebin.push_back(liketmp);
  nbins ++;
}

//_____________________________________________________________________________
float
TRDLike::prob_e(float val)
{
  for (size_t i=0; i<likebin.size(); i++)
      if (likebin[i].min <= val && likebin[i].max > val)
	return likebin[i].prob_e;
  return -999.;
}

//_____________________________________________________________________________
float
TRDLike::prob_p(float val)
{
  for (size_t i=0; i<likebin.size(); i++)
    if (likebin[i].min <= val && likebin[i].max > val)
      return likebin[i].prob_p;
  return -999.;
}

//_____________________________________________________________________________
float
TRDLike::likelihood(float val)
{
  for (size_t i=0; i<likebin.size(); i++)
    {
      if (likebin[i].min <= val && likebin[i].max > val)
	{
	  if (likebin[i].prob_p > 0 && likebin[i].prob_e > 0)
	    return log(likebin[i].prob_e / likebin[i].prob_p);
	  if (likebin[i].prob_e > 0)
	    return 1.;
	  if (likebin[i].prob_p > 0)
	    return -1.;
	}
    }
  return -999.;
}

int
TRDLike::Update(int beginrun, int endrun)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }


  PdbBankManager *bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("TRDLike::", PHError, "Aborting ... Database not writable");
      application->abort();
      return 0;
    }

  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue((int)index);
  const char *descrip = "Parameters submitted by generic recal object";

  string calibname = "calib.tec.trdlike." + Name();

  auto_ptr<PdbCalBank> trdBank(bankManager->createBank("PdbTRDlikeBank", bankID, descrip,
							 Tstart, Tstop, calibname.c_str()));

  trdBank->setLength(nbins);

  PdbTRDlike *trdbin;

  for (size_t ibin=0; ibin<nbins; ibin++)
    {
      trdbin = (PdbTRDlike *) & trdBank->getEntry(ibin);
      trdbin->set_likebin(likebin[ibin]);
    }
  application->commit();
  return 1;
}

int
TRDLike::Fetch(int runnumber)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  PdbBankManager *bankManager = PdbBankManager::instance();

  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("TRDLike::", PHError, "Aborting ... Database not readable");
      application->abort();
      return 0;
    }

  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue(index);

  string calibname = "calib.tec.trdlike." + Name();

  auto_ptr<PdbCalBank> trdBank(bankManager->fetchBank("PdbTRDlikeBank",
							bankID, calibname.c_str(), runnumber));

  //  if (!trdBank)
  //    {
  //     PHMessage("TRDLike::", PHError, "Aborting ... Search not found");
  //    application->abort();
  //    return -1;     
  //  }

  PdbTRDlike *trdbin;
  nbins = trdBank->getLength();

  likerange liketmp;

  for (size_t ibin=0; ibin<nbins; ibin++)
    {
      trdbin = (PdbTRDlike*) & trdBank->getEntry(ibin);
      liketmp = trdbin->get_likebin();
      likebin.push_back(liketmp);
    }
  application->commit();
  return 1;
}
