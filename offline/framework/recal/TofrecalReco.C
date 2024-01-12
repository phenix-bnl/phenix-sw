#include "PHCompositeNode.h"
#include "TofrecalReco.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "TofOut.h"
#include "TofCalibObject.hh"

#include "getClass.h"
#include "recoConsts.h"

#include "PdbBankManager.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include "PdbParameter.hh"
#include "RunToTime.hh"

#include "gsl/gsl_const.h"

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>

using namespace std;


TofrecalReco::TofrecalReco(const char *name): Recalibrator(name)
{
  verbosity = 0;
  baseclasses.insert("PHCentralTrack");
  baseclasses.insert("TofOut");
  tofcalib = 0;

  return ;
}

int
TofrecalReco::isValidRun(const int runno) const
{
  if (runno >= 107445 && runno <= 123564) // before Run4
    {
      return 1;
    }
  else if (runno >= 150513 && runno <= 160487) // Run5 Cu+Cu 200GeV
    {
      return 1;
    }
  else if (runno >= 161208 && runno <= 163463) // Run5 Cu+Cu 62.4GeV
    {
      return 1;
    }
  else if (runno >= 163604 && runno <= 163681) // Run5 Cu+Cu 22.5GeV
    {
      return 1;
    }
  else if (runno >= 166416 && runno <= 179846) // Run5 p+p 200GeV
    {
      return 1;
    }
  else if (runno >= 188216 && runno <= 204639) // Run6 p+p 200GeV
    {
      return 1;
    }
  else if (runno >= 205153 && runno <= 206495) // Run6 p+p 62.4GeV
    {
      return 1;
    }
  else if (runno >= 227016 && runno <= 240121) // Run7 Au+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 246214 && runno <= 253701) // Run8 d+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 256450 && runno <= 259575) // Run8 p+p 200GeV
    {
      return 1;
    }
  else if (runno >= 276324 && runno <= 280240) // Run9 p+p 500GeV
    {
      return 1;
    }
  else if (runno >= 281911 && runno <= 291515) // Run9 p+p 200GeV
    {
      return 1;
    }
  else if (runno >= 300475 && runno <= 310454) // Run10 Au+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 310698 && runno <= 313322) // Run10 Au+Au 62GeV
    {
      return 1;
    }
  else if (runno >= 313591 && runno <= 314994) // Run10 Au+Au 39GeV
    {
      return 1;
    }
  else if (runno >= 315450 && runno <= 318939) // Run10 Au+Au 7GeV
    {
      return 1;
    }
  else if (runno >= 331130 && runno <= 340515) // Run11 p+p 500GeV
    {
      return 1;
    }
  else if (runno >= 341270 && runno <= 342345) // Run11 Au+Au 19GeV
    {
      return 1;
    }
  else if (runno >= 343031 && runno <= 349680) // Run11 Au+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 349830 && runno <= 350577) // Run11 Au+Au 27GeV
    {
      return 1;
    }
  else if (runno >= 358629 && runno <= 363228) // Run12 p+p 200GeV
    {
      return 1;
    }
  else if (runno >= 364822 && runno <= 368798) // Run12 p+p 510GeV
    {
      return 1;
    }
  else if (runno >= 369327 && runno <= 371908) // Run12 U+U 200GeV
    {
      return 1;
    }
  else if (runno >= 372403 && runno <= 377310) // Run12 Cu+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 386773 && runno <= 398149) // Run13 p+p 510GeV
    {
      return 1;
    }
  else if (runno >= 402024 && runno <= 405182) // Run14 Au+Au 15GeV
    {
      return 1;
    }
  else if (runno >= 405860 && runno <= 414988) // Run14 Au+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 415751 && runno <= 416892) // Run14 He+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 421716 && runno <= 432008) // Run15 p+p 200GeV
    {
      return 1;
    }
  else if (runno >= 432637 && runno <= 436647) // Run15 p+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 436759 && runno <= 438422) // Run15 p+Al 200GeV
    {
      return 1;
    }
  else if (runno >= 443135 && runno <= 454252) // Run16 Au+Au 200GeV first period
    {
      return 1;
    }
  else if (runno >= 454774 && runno <= 455639) // Run16 d+Au 200GeV
    {
      return 1;
    }
  else if (runno >= 455792 && runno <= 456281) // Run16 d+Au 62GeV
    {
      return 1;
    }
  else if (runno >= 457569 && runno <= 458167) // Run16 d+Au 39GeV
    {
      return 1;
    }
  else if (runno >= 456652 && runno <= 457298) // Run16 d+Au 20GeV
    {
      return 1;
    }
  else if (runno >= 458390 && runno <= 459071) // Run16 Au+Au 200GeV second period
    {
      return 1;
    }

  return 0;
}

TofrecalReco::~TofrecalReco()
{
  if (tofcalib)
    {
      delete tofcalib;
    }
}

int
TofrecalReco::Init(PHCompositeNode *topNode)
{
  tofcalib = new TofCalibObject();
  return 0;
}

int TofrecalReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  //  Zero the constants at construction...
  memset(DeltaT, 0, sizeof(DeltaT));
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  fetchDeltaT(runnumber);
  recalcnt = 0;
  recaltof = 0;
  if (mybaseclass == "PHCentralTrack")
    {
      recalcnt = 1;
    }
  else if (mybaseclass == "TofOut")
    {
      recaltof = 1;
    }

  return 0;
}

void
TofrecalReco::Print(const std::string& what) const
{
  Recalibrator::Print(what);
  if (what.find("DT") != string::npos)
    {
      cout << "Printing the Delta-T constants." << endl;
      cout << "  slat#   dt (nsec)" << endl;
      for (int i = 0; i < TOF_NSLAT; i++)
	{
	  cout << i << " " << DeltaT[i] << endl;
	}
    }
  else if (what.find("ELOSS") != string::npos)
    {
      cout << "Printing the Eloss Conversion parameter." << endl;
      cout << "  slat#   ec (GeV/ch)" << endl;
      for (int i = 0; i < TOF_NSLAT; i++)
	{
	  cout << i << " " << tofcalib->getElossConv(i) << endl;
	}

    }
  else if (what.find("SLEWING") != string::npos)
    {
      cout << "Printing the Slewing parameter." << endl;
      cout << "slewpar = a + b/sqrt(charge)" << endl;
      cout << "  slat#   a0    b0    a1    b1" << endl;
      for (int i = 0; i < TOF_NSLAT; i++)
	{
	  cout << i << " " << tofcalib->getSlewPar_a(0, i) << " " << tofcalib->getSlewPar_b(0, i)
	       << " " << tofcalib->getSlewPar_a(1, i) << " " << tofcalib->getSlewPar_b(1, i)
	       << endl;
	}
    }
  else if (what.find("MIPPEAK") != string::npos)
    {
      cout << "Printing the Mip Peak parameter." << endl;
      cout << "  slat#   mip0 (ch)  mip1 (ch)" << endl;
      for (int i = 0; i < TOF_NSLAT; i++)
	{
	  cout << i << " " << tofcalib->getMipPeak(0, i) << " " << tofcalib->getMipPeak(1, i) << endl;
	}
    }
  else if (what.find("YOFFSET") != string::npos || what.find("VELOCITY") != string::npos)
    {
      cout << "Printing the Yoffset and Velocity parameter." << endl;
      cout << "  slat#   yoffset (cm)   velocity (cm/ns)" << endl;
      for (int i = 0; i < TOF_NSLAT; i++)
	{
	  cout << i << " " << tofcalib->getYoffset(i) << " " << tofcalib->getVelocity(i) << endl;
	}
    }

  return ;
}

int
TofrecalReco::process_event(PHCompositeNode *topNode)
{

  static const float c = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns

  //  First correct the TOF hits where they lie...
  if (recaltof)
    {
      TofOut *d_tof = findNode::getClass<TofOut>(topNode, inputnodename.c_str());
      if (d_tof)
        {
          for (unsigned int i = 0; i < d_tof->get_TofNHit(); i++)
            {
              int slat = d_tof->get_slatid(i);
              if (slat >= 0 && slat < TOF_NSLAT)
                {
                  d_tof->set_tof(i, d_tof->get_tof(i) - DeltaT[slat]);
                }
            }
        }
    }
  //  Then correct the tracks you have found...
  if (recalcnt)
    {
      PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
      if (d_cnt)
        {
          for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
            {
              PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
	      sngltrk->ShutUp();
	      int testvirtual = sngltrk->isImplemented(sngltrk->get_slat());
	      sngltrk->ShutUp(0);
	      if (!testvirtual)
		{
		  break;
		}
              int slat = sngltrk->get_slat();

              if (slat >= 0 && slat < TOF_NSLAT)
                {
                  sngltrk->set_ttof(sngltrk->get_ttof() - DeltaT[slat]);
                  float p = sngltrk->get_mom();
                  float t = sngltrk->get_ttof();
                  float L = sngltrk->get_pltof();
                  float m2 = 0;
                  if (L > 0)
                    {
                      m2 = p * p * ( t * t * c * c / (L * L) - 1);
                    }
                  sngltrk->set_m2tof(m2);
                }
            }
        }
    }
  return 0;
}

void
TofrecalReco::fetchDeltaTFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "TofrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  //  Each line of the text file contains the following:
  //
  //  b   /   i
  //  d   \    DeltaT
  //

  int slat;
  float dt;

  if (verbosity > 0)
    cout << "Slat DeltaT" << endl;
  while (!file.eof())
    {
      file >> slat >> dt;
      DeltaT[slat] = dt;
      if (verbosity > 0)
        {
          cout << slat << " ";
          cout << DeltaT[slat] << " ";
          cout << endl;
        }
    }

  file.close();

  return ;
}

void
TofrecalReco::fetchElossConvFromFile(const char * filename)
{
  //  Each line of the text file contains the following:
  //
  //  b   /   i
  //  d   \   ElossConv
  //

  tofcalib->fetchElossConvFromFile(filename);

  if (verbosity > 0)
    {
      cout << "Slat ElossConv" << endl;
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << islat << " ";
          cout << tofcalib->getElossConv(islat) << " ";
          cout << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchMipPeakFromFile(const char * filename)
{
  //  Each line of the text file contains the following:
  //
  //  b   /   slatid, pmt
  //  d   \   MipPeak
  //

  tofcalib->fetchMipPeakFromFile(filename);

  if (verbosity > 0)
    {
      cout << "PMT MipPeak" << endl;
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << islat << " ";
          for (int ipmt = 0;ipmt < 2;ipmt++)
            {
              cout << ipmt << " ";
              cout << tofcalib->getMipPeak(ipmt, islat) << " ";
            }
          cout << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchSlewingFromFile(const char * filename)
{
  //  Each line of the text file contains the following:
  //
  //  b   /   slatid, pmt
  //  d   \   Slewing
  //

  tofcalib->fetchSlewParFromFile(filename);

  if (verbosity > 0)
    {
      cout << "Slewing correction" << endl;
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << islat << " ";
          for (int ipmt = 0;ipmt < 2;ipmt++)
            {
              cout << ipmt << " ";
              cout << tofcalib->getSlewPar_a(ipmt, islat) << " "
		   << tofcalib->getSlewPar_b(ipmt, islat);
            }
          cout << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchYoffsetFromFile(const char* filename)
{
  //  Each line of the text file contains the following:
  //
  //  b   /   slatid
  //  d   \   Yoffset
  //

  tofcalib->fetchYoffsetFromFile(filename);

  if (verbosity > 0)
    {
      cout << "Yoffset correction" << endl;
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << islat << " " << tofcalib->getYoffset(islat) << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchVelocityFromFile(const char* filename)
{
  //  Each line of the text file contains the following:
  //
  //  b   /   slatid
  //  d   \   Velocity
  //

  tofcalib->fetchVelocityFromFile(filename);

  if (verbosity > 0)
    {
      cout << "Velocity correction" << endl;
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << islat << " " << tofcalib->getVelocity(islat) << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchDeltaT(const int run)
{
  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of DeltaT calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b   /   slat
  //  d   \   DeltaT
  //
  //  The number of times the body repeats will be TOF_NSLAT
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 2-4-2004
  //

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("TofrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *nameDB = "calib.test.tofrecal";
  //  const char *descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB, run);
  if (deltaBank)
    {
      //----------------------------------------------------
      //  OK...now is the time to actually unpack the data...
      //  three checks...length of record, scheme and no. entries...
      int length = 2 * TOF_NSLAT + 2;
      int truelength = deltaBank->getLength();
      if (length != truelength)
        {
          cout << "TofrecalReco:: FATAL...wrong length DB read for delta t" << endl;
          return ;
        }


      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "TofrecalReco:: FATAL...wrong scheme DB read for delta-t" << endl;
          return ;
        }


      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "TofrecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          return ;
        }


      //----------------------------------------------------
      //  Checks passed...get the parameters...
      if (verbosity == 0)
        cout << "READ from Database: Slat Delta-T" << endl;
      for (int i = 0; i < TOF_NSLAT; i++)
        {
          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
          int slat = (int)parameter->getParameter();

          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
          DeltaT[slat] = parameter->getParameter();
          if (verbosity > 0)
            {
              cout << " slat: " << slat;
              cout << " dt: " << DeltaT[slat];
              cout << endl;
            }

        }
      delete deltaBank;
    }
  else
    {
      cout << PHWHERE << " Could not load calibration for run " << run << endl;
      exit(1);
    }

  return ;
}

void
TofrecalReco::fetchElossConv(const int run)
{
  // fetch ElossConv via TofCalibObject
  //  b   /   slat
  //  d   \   ElossConv
  //

  tofcalib->fetchElossConv(run);

  if (verbosity > 0)
    {
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << " slat: " << islat
	       << " ec: " << tofcalib->getElossConv(islat)
	       << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchMipPeak(const int run)
{
  // fetch MipPeak via TofCalibObject
  //  b   /   slat, pmt
  //  d   \   MipPeak
  //

  tofcalib->fetchMipPeak(run);

  if (verbosity > 0)
    {
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << " slat: " << islat;
          for (int ipmt = 0;ipmt < 2;ipmt++)
            {
              cout << " pmt: " << ipmt
		   << " mp: " << tofcalib->getMipPeak(ipmt, islat);
            }
          cout << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchSlewing(const int run)
{
  // fetch Slewing via TofCalibObject
  //  b   /   slat, pmt
  //  d   \   Slewing
  //

  tofcalib->fetchSlewPar(run);

  if (verbosity > 0)
    {
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << " slat: " << islat;
          for (int ipmt = 0;ipmt < 2;ipmt++)
            {
              cout << " pmt: " << ipmt
		   << " slewing a: " << tofcalib->getSlewPar_a(ipmt, islat)
		   << " b: " << tofcalib->getSlewPar_b(ipmt, islat);
            }
          cout << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchYoffset(const int run)
{
  // fetch Yoffset via TofCalibObject
  //  b   /   slat
  //  d   \   Yoffset
  //

  tofcalib->fetchYoffset(run);

  if (verbosity > 0)
    {
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << " slat: " << islat
	       << " yoffset: " << tofcalib->getYoffset(islat)
	       << endl;
        }
    }

  return ;
}

void
TofrecalReco::fetchVelocity(const int run)
{
  // fetch Velocity via TofCalibObject
  //  b   /   slat
  //  d   \   Velocity
  //

  tofcalib->fetchVelocity(run);

  if (verbosity > 0)
    {
      for (int islat = 0;islat < TOF_NSLAT;islat++)
        {
          cout << " slat: " << islat
	       << " v: " << tofcalib->getVelocity(islat)
	       << endl;
        }
    }

  return ;
}

void
TofrecalReco::updateDeltaT(int beginrun, int endrun)
{

  //  OK gang...now it gets MORE intense.
  //  In this routine we will be storing the entire bank
  //  of DeltaT calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  b   /   slat
  //  d   \   DeltaT
  //
  //  The number of times the body repeats will be TOF_NSLAT
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 2-4-2004
  //
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startUpdate())
    {
      PHMessage("TofrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *nameDB = "calib.test.tofrecal";
  const char *descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, nameDB);
  int length = 2 * TOF_NSLAT + 2; // array + 2 hdr
  deltaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (int i = 0; i < TOF_NSLAT; i++)
    {

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(i);
      parameter->setName("Slat");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(DeltaT[i]);
      parameter->setName("dt");

    }

  application->commit();

  if (verbosity > 0)
    cout << Name() << ":: update DeltaT parameter, DONE !!!" << endl;

  return ;
}

void
TofrecalReco::updateElossConv(int beginrun, int endrun)
{
  // update ElossConv via TofCalibObject
  //  b   /   slat
  //  d   \   ElossConv
  //

  tofcalib->updateElossConv(beginrun, endrun);

  if (verbosity > 0)
    cout << Name() << ":: update ElossConv parameter, DONE !!!" << endl;

  return ;
}

void
TofrecalReco::updateMipPeak(int beginrun, int endrun)
{
  // update MipPeak via TofCalibObject
  //  b   /   slat, pmt
  //  d   \   MipPeak
  //

  tofcalib->updateMipPeak(beginrun, endrun);

  if (verbosity > 0)
    cout << Name() << ":: update MipPeak parameter, DONE !!!" << endl;

  return ;
}

void
TofrecalReco::updateSlewing(int beginrun, int endrun)
{
  // update Slewing via TofCalibObject
  //  b   /   slat, pmt
  //  d   \   Slewing
  //

  tofcalib->updateSlewPar(beginrun, endrun);

  if (verbosity > 0)
    cout << Name() << ":: update Slewing parameter, DONE !!!" << endl;

  return ;
}

void
TofrecalReco::updateYoffset(int beginrun, int endrun)
{
  // update Yoffset via TofCalibObject
  //  b   /   slat
  //  d   \   Yoffset
  //

  tofcalib->updateYoffset(beginrun, endrun);

  if (verbosity > 0)
    cout << Name() << ":: update Yoffset parameter, DONE !!!" << endl;

  return ;
}

void
TofrecalReco::updateVelocity(int beginrun, int endrun)
{
  // update Velocity via TofCalibObject
  //  b   /   slat
  //  d   \   Velocity
  //

  tofcalib->updateVelocity(beginrun, endrun);

  if (verbosity > 0)
    cout << Name() << ":: update Velocity parameter, DONE !!!" << endl;

  return ;
}


void
TofrecalReco::setElossConv(const int islat, const float val)
{
  return tofcalib->setElossConv(islat, val);
}

void
TofrecalReco::setMipPeak(const int islat, const int ipmt, const float val)
{
  return tofcalib->setMipPeak(ipmt, islat, val, 0.0);
}

void
TofrecalReco::setSlewing(const int islat, const int ipmt, const float val_a, const float val_b)
{
  tofcalib->setSlewPar_a(ipmt, islat, val_a);
  tofcalib->setSlewPar_b(ipmt, islat, val_b);

  return ;
}

void
TofrecalReco::setYoffset(const int islat, const float val)
{
  return tofcalib->setYoffset(islat, val);
}

void
TofrecalReco::setVelocity(const int islat, const float val)
{
  return tofcalib->setVelocity(islat, val);
}

float TofrecalReco::getElossConv(const int islat) const
{
  return tofcalib->getElossConv(islat);
}

float TofrecalReco::getMipPeak(const int islat, const int ipmt) const
  {
    return tofcalib->getMipPeak(ipmt, islat);
  }

float TofrecalReco::getSlewPar_a(const int islat, const int ipmt) const
  {
    return tofcalib->getSlewPar_a(ipmt, islat);
  }

float TofrecalReco::getSlewPar_b(const int islat, const int ipmt) const
  {
    return tofcalib->getSlewPar_b(ipmt, islat);
  }

float TofrecalReco::getYoffset(const int islat) const
  {
    return tofcalib->getYoffset(islat);
  }

float TofrecalReco::getVelocity(const int islat) const
  {
    return tofcalib->getVelocity(islat);
  }

