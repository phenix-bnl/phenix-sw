//
// This is for Run-8 reaction plane calibration
// 
#include "ReactionPlaneCalibv2.h"

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbParameter.hh>
#include <PdbCalBank.hh>
#include <RunToTime.hh>
#include "PHTimeStamp.h"

#include <RunNumberRanges.h>
#include <utiCentrality.h>
#include <PHGlobal.h>
#include <RunHeader.h>

#include <getClass.h>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace RP;
using namespace std;


ReactionPlaneCalibv2::ReactionPlaneCalibv2()
{
  Reset();

  verbosity = 0;
  RunNumber = 0;

  databasename = "calib.reactionplane";
}

void ReactionPlaneCalibv2::Reset()
{

  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
	  SumXmean[im][iz][ih][id]  = 0.0;
	  SumXsigma[im][iz][ih][id] = 1.0;
	  SumYmean[im][iz][ih][id]  = 0.0;
	  SumYsigma[im][iz][ih][id] = 1.0;

          for (int io = 0;io < NORD5;io++) {
            FlatCos[im][iz][ih][id][io] = 0.0;
	  }
          for (int io = 0;io < NORD5;io++) {
            FlatSin[im][iz][ih][id][io] = 0.0;
	  }
        }
      }
    }
  }

}

float ReactionPlaneCalibv2::Flattening(const int idet, const int ihar, const int imul, 
                                     const int izps, const float psi)
{
  float deltaPsi = 0.0;
  float psi0 = psi * (ihar + 1.0); // -pi < psi0 < pi

  for (int io = 0;io < NORD5;io++) {
//    float averageCos = FlatCos[idet][ihar][imul][izps][io];
//    float averageSin = FlatSin[idet][ihar][imul][izps][io];
    float averageCos = FlatCos[imul][izps][ihar][idet][io];
    float averageSin = FlatSin[imul][izps][ihar][idet][io];

    float cosPsi = cos( (io + 1.0) * psi0 );
    float sinPsi = sin( (io + 1.0) * psi0 );
    deltaPsi += ( -averageSin * cosPsi + averageCos * sinPsi) * 2.0 / (io + 1.0);
  }

  return atan2(sin(psi0 + deltaPsi), cos(psi0 + deltaPsi)) / (ihar + 1.0); // -pi/n < psi < pi/n
}

int ReactionPlaneCalibv2::Fetch(const int runNumber)
{
  RunNumber = runNumber;
  
  cout << "ReactionPlaneCalibv2::Fetch from DB for run " << RunNumber << endl;

  // DLW version: check env var for filename template.  This template
  // is expected to have one %d field in it, and it will be replaced with
  // the current run number.
  if ( char* fnameTemplate = getenv("RPCALIB_FILENAME_TEMPLATE") )
    {
      const int LEN = 4096;
      char fname[LEN];
      int n = snprintf(fname,LEN,fnameTemplate,runNumber);
      if ( n >= LEN ) 
	{
	  std::cout << "ReactionPlaneCalibv2::Fetch: WARNING: snprintf truncated output string" 
		    << std::endl;
	}
      std::cout << "ReactionPlaneCalibv2::Fetch: overriding DB fetch with file " << fname << std::endl;

      // Attempt to use the provided file.  If successful, return.  If not,
      // proceed with DB fetching and see if that works.
      if ( Fetch(fname) == 0 ) return 0;
      else
	std::cout << "ReactionPlaneCalibv2::Fetch: Failed to fetch calib from file, "
		  << "trying DB instead... " << std::endl;
    }

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startRead())
    {
      cout << PHWHERE << " Error : Aborting ... Database not readable" << endl;
      application->abort();
    }

  // make bank ID
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());

  // Grap a pointer to the bank ...
  PdbCalBank* rpBank = bankManager->fetchBank("PdbParameterBank", bankID, databasename.c_str(), runNumber);
  if (!rpBank)
    {
      return -1;
    }
  Fetch(rpBank);
  return 0;
}

void ReactionPlaneCalibv2::Fetch(const PHTimeStamp& tstart)
{
  cout << "ReactionPlaneCalibv2::Fetch from DB." << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startRead())
    {
      cout << PHWHERE << " Error : Aborting ... Database not readable" << endl;
      application->abort();
    }

  // Set run number from time stamp
  RunNumber = RunToTime::instance()->getRunNumber(tstart);

  // make bank ID
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());

  // Grap a pointer to the bank ...
  PHTimeStamp Tstart = tstart;
  PdbCalBank* rpBank = bankManager->fetchBank("PdbParameterBank", bankID, databasename.c_str(), Tstart);

  Fetch(rpBank);
  return ;
}

void ReactionPlaneCalibv2::Fetch(PdbCalBank* rpBank)
{
  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  three checks...length of record, scheme and no. entries...
  const int bankid = rpBank->getBankID().getInternalValue() ;
  const int length = GetDBLength(bankid);

  int truelength = rpBank->getLength();

  if (length != truelength) {
    cout << PHWHERE << " FATAL...wrong length DB read for R.P." << endl;
    return ;
  }

  int index = 0;
  PdbParameter* parameter = (PdbParameter*)(& rpBank->getEntry(index++));
  int scheme = (int)parameter->getParameter();
  if (scheme != 1) {
    cout << PHWHERE << " FATAL...wrong scheme DB read for R.P." << endl;
    return ;
  }

  parameter = (PdbParameter *)(& rpBank->getEntry(index++));
  int entries = (int)parameter->getParameter();
  if (entries != length - 2) {
    cout << PHWHERE << " FATAL...wrong entries DB read for R.P" << endl;
    return ;
  }

  //----------------------------------------------------
  //  Checks passed...get the parameters...
  cout << "READ from Database: Reaction Plane" << endl;
  cout << " read SumX/Y correction" << endl;
  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
	  SumXmean[im][iz][ih][id]  = parameter->getParameter();

          parameter = (PdbParameter*) & rpBank->getEntry(index++);
	  SumXsigma[im][iz][ih][id]  = parameter->getParameter();

          parameter = (PdbParameter*) & rpBank->getEntry(index++);
	  SumYmean[im][iz][ih][id]  = parameter->getParameter();

          parameter = (PdbParameter*) & rpBank->getEntry(index++);
	  SumYsigma[im][iz][ih][id]  = parameter->getParameter();

          if (verbosity > 2) {
            cout << "X mean " << SumXmean[im][iz][ih][id]
                 << " sigma " << SumXsigma[im][iz][ih][id]
                 << " Y mean " << SumYmean[im][iz][ih][id]
                 << " sigma " << SumYsigma[im][iz][ih][id]
                 << endl;
          }
        }
      }
    }
  }

  cout << " read flattening correction ... " << endl;
  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
          for (int io=0; io<NORD5; io++) {
            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            FlatCos[im][iz][ih][id][io] = parameter->getParameter();
          }

          for (int io=0; io<NORD5; io++) {
            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            FlatSin[im][iz][ih][id][io] = parameter->getParameter();
          }

          for (int io=0; io<NORD5; io++) {
            if (verbosity > 2) {
              cout << "cos " << FlatCos[im][iz][ih][id][io]
                   << " sin " << FlatSin[im][iz][ih][id][io]
                   << endl;
            }
          }
        }
      }
    }
  }

  delete rpBank;
}

int
ReactionPlaneCalibv2::Fetch(const char* filename)
{
  // Need to set run number before calling this function

  ifstream file(filename);
  if (!file) {
    cout << PHWHERE << " Could not open input file " << filename << endl;
    return -1;
  }

  if (verbosity > 0) {
    cout << "ReactionPlaneCalibv2:: R.P. calibration parameter from " << filename << endl;
  }

  // SumX/Y correction
  cout << " read SumX/Y correction for BBC/SMD/FCL/CNT" << endl;
  cout << " read flattening correction ... " << endl;
  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
          file >> SumXmean[im][iz][ih][id] >> SumXsigma[im][iz][ih][id]
               >> SumYmean[im][iz][ih][id] >> SumYsigma[im][iz][ih][id];

          for (int io=0; io<NORD5; io++) {
            file >>FlatCos[im][iz][ih][id][io];
          }

          for (int io=0; io<NORD5; io++) {
            file >>FlatSin[im][iz][ih][id][io];
          }

          if (verbosity > 2) {
            cout << "X mean " << SumXmean[im][iz][ih][id]
                 << " sigma " << SumXsigma[im][iz][ih][id]
                 << " Y mean " << SumYmean[im][iz][ih][id]
                 << " sigma " << SumYsigma[im][iz][ih][id]
                 << endl;
            for (int io = 0;io < NORD5;io++) {
              cout << "cos " << FlatCos[im][iz][ih][id][io]
                   << " sin " << FlatSin[im][iz][ih][id][io]
                   << endl;
            }
          }
        }
      }
    }
  }

  file.close();

  return 0;
}

void ReactionPlaneCalibv2::Update(const int beginrun, const int endrun)
{
  RunNumber = beginrun;

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0) {
    ts = runTime->getEndTime(endrun);
    Tstop = *ts;
    delete ts;
  }
  else {
    Tstop.setToFarFuture();
  }

  //  Make the managers...
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startUpdate()) {
    cout << PHWHERE << " Error : Aborting ... Database not writable" << endl;
    application->abort();
  }

  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());

  ostringstream descrip;
  descrip << "R.P. parameters for RUN " << RunNumber;

  //  Grap a pointer to the bank...
  PdbCalBank* rpBank = bankManager->createBank("PdbParameterBank", bankID, descrip.str().c_str(), Tstart, Tstop, databasename.c_str());

  return Update(rpBank, application);
}

void ReactionPlaneCalibv2::Update(const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  //  Make the managers...
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startUpdate()) {
    cout << PHWHERE << " Error : Aborting ... Database not writable" << endl;
    application->abort();
  }

  // Get run number from time stamp
  RunNumber = RunToTime::instance()->getRunNumber(tstart);

  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID());
  ostringstream descrip;
  descrip << "R.P. parameters for RUN " << RunNumber;

  PHTimeStamp Tstart = tstart;
  PHTimeStamp Tstop = tstop;

  //  Grap a pointer to the bank...
  PdbCalBank* rpBank = bankManager->createBank("PdbParameterBank", bankID, descrip.str().c_str(), Tstart, Tstop, databasename.c_str());

  return Update(rpBank, application);
}

void ReactionPlaneCalibv2::Update(PdbCalBank* rpBank, PdbApplication* application)
{

  const int bankid = rpBank->getBankID().getInternalValue() ;
  const int length = GetDBLength( bankid );
  rpBank->setLength(length);

  cout << "Length of DB = " << length << endl;


  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & rpBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & rpBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  if (verbosity > 0){
    cout << " Set parameter for BBC/SMD/MVD/FCL/CNT/MPC/RXNP R.P." << endl;
  }

  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumXmean[im][iz][ih][id]);
          parameter->setName("sumXmean");

          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumXsigma[im][iz][ih][id]);
          parameter->setName("sumXsigma");

          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumYmean[im][iz][ih][id]);
          parameter->setName("sumYmean");

          parameter = (PdbParameter*) & rpBank->getEntry(index++);
          parameter->setParameter(SumXsigma[im][iz][ih][id]);
          parameter->setName("sumYsigma");
        }
      }
    }
  }

  if (verbosity > 0) {
    cout << " set flattening correction ... " << endl;
  }
  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
          for (int io=0; io<NORD5; io++) {
            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            parameter->setParameter(FlatCos[im][iz][ih][id][io]);
            parameter->setName("flatCos");
          }

          for (int io=0; io<NORD5; io++) {
            parameter = (PdbParameter*) & rpBank->getEntry(index++);
            parameter->setParameter(FlatSin[im][iz][ih][id][io]);
            parameter->setName("flatSin");
          }
        }
      }
    }
  }

  if (verbosity>0){
    cout << "ReactionPlaneCalibv2:: Update parameter " << endl;
  }

  application->commit(rpBank);
}

void ReactionPlaneCalibv2::Write(const char* filename)
{
  // Need to set run number before calling Write() function

  cout << "Write parameter to " << filename << endl;

  ofstream fout(filename);

  for (int im=0; im<NMUL5; im++) {
    for (int iz=0; iz<NZPS5; iz++) {
      for (int ih=0; ih<NHAR5; ih++) {
        for (int id=0; id<NDET5; id++) {
          fout << SumXmean[im][iz][ih][id] << " " << SumXsigma[im][iz][ih][id] <<" ";
          fout << SumYmean[im][iz][ih][id] << " " << SumYsigma[im][iz][ih][id] <<" ";
          fout << endl;

          for (int io=0; io<NORD5; io++) {
            fout << FlatCos[im][iz][ih][id][io] << " ";
          }
          fout << endl;

          for (int io=0; io<NORD5; io++) {
            fout << FlatSin[im][iz][ih][id][io] << " ";
          }
          fout << endl;
        }
      }
    }
  }

  fout.close();

  cout << "Write parameter to " << filename << " [DONE] " << endl;
}

float ReactionPlaneCalibv2::GetSumXmean(const int idet, const int ihar, const int imul, const int izps)
{
  if(idet<0 || idet>NDET5-1 || ihar<0 || ihar>NHAR5-1 || imul<0 || imul>NMUL5-1 || izps<0 || izps>NZPS5-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }

    return 0.0;
  }
  return SumXmean[imul][izps][ihar][idet];
}

float ReactionPlaneCalibv2::GetSumXsigma(const int idet, const int ihar, const int imul, const int izps)
{
  if(idet<0 || idet>NDET5-1 || ihar<0 || ihar>NHAR5-1 || imul<0 || imul>NMUL5-1 || izps<0 || izps>NZPS5-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 1.0;
  }
  return SumXsigma[imul][izps][ihar][idet];
}

float ReactionPlaneCalibv2::GetSumYmean(const int idet, const int ihar, const int imul, const int izps)
{
  if(idet<0 || idet>NDET5-1 || ihar<0 || ihar>NHAR5-1 || imul<0 || imul>NMUL5-1 || izps<0 || izps>NZPS5-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 0.0;
  }
  return SumYmean[imul][izps][ihar][idet];
}

float ReactionPlaneCalibv2::GetSumYsigma(const int idet, const int ihar, const int imul, const int izps)
{
  if(idet<0 || idet>NDET5-1 || ihar<0 || ihar>NHAR5-1 || imul<0 || imul>NMUL5-1 || izps<0 || izps>NZPS5-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im)=("
           << idet << "," << ihar << "," << imul << ")" << endl;
    }
    return 1.0;
  }
  return SumYsigma[imul][izps][ihar][idet];
}

float ReactionPlaneCalibv2::GetFlatCos(const int idet, const int ihar, const int imul,
                                     const int izps, const int iord)
{
  if(idet<0 || idet>NDET5-1 || ihar<0 || ihar>NHAR5-1 || imul<0 || imul>NMUL5-1 || izps<0 || izps>NZPS5-1 || iord<0 || iord>NORD5-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im,iz,io)=("
           << idet << "," << ihar << "," << imul << "," << izps << "," << iord << ")" << endl;
    }
    return 0.0;
  }
  return FlatCos[imul][izps][ihar][idet][iord];
}

float ReactionPlaneCalibv2::GetFlatSin(const int idet, const int ihar, const int imul,
                                     const int izps, const int iord)
{
  if(idet<0 || idet>NDET5-1 || ihar<0 || ihar>NHAR5-1 || imul<0 || imul>NMUL5-1 || izps<0 || izps>NZPS5-1 || iord<0 || iord>NORD5-1) {
    if(verbosity>1){
      cout << PHWHERE << " Error : check index (id,ih,im,iz,io)=("
           << idet << "," << ihar << "," << imul << "," << izps << "," << iord << ")" << endl;
    }
    return 0.0;
  }
  return FlatSin[imul][izps][ihar][idet][iord];
}

int ReactionPlaneCalibv2::GetCentrality(PHCompositeNode *topNode)
{
//  RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
//  int runNumber = run->get_RunNumber();

  return (int)global->getCentrality();
}

int ReactionPlaneCalibv2::GetCentralityBin(const int cent)
{

//  int icent = (int) ( NMUL * ((cent - 0.001) / 100.) );

//
//  For Run-8 d+Au, calibration is available for 0-20% only
//
  int icent = (int) ( NMUL5 * ((cent - 0.001) / 20.) );

  if(icent<0 || icent>NMUL5-1) icent = -1;

  return icent;
}

int ReactionPlaneCalibv2::GetZVertexBin(const float bbcz)
{
  int ibbcz = (int) ( NZPS5 * ((bbcz + 30.0) / 60.0));

  if(ibbcz<0 || ibbcz>NZPS5-1) ibbcz = -1;

  return ibbcz;
}

int ReactionPlaneCalibv2::GetBankID() const
{
  // return bank id 
  // For Run-8 d+Au, it is  3

  return 3;

}

int ReactionPlaneCalibv2::GetDBLength(const int bankid) const
{
  // return DB length

  // Add RXN/MPC from Run7 (and Run8)
  int length_sum = NDET5 * NHAR5 * NMUL5 * NZPS5 * 4;
  int length_flat = NDET5 * NHAR5 * NMUL5 * NZPS5 * NORD5;

  return length_sum + 2 * length_flat + 2;
}


