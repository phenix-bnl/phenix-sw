#include <PidrecalReco.h>

#include <PHCompositeNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankList.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>

#include <string>
#include <fstream>

using namespace std;

//==============================================================================
// T O F - P I D   P A R A M E T E R S (update: June 16, 2005, T.Chujo)
// _____________________________________________________________________________
//
// PidPar_plus[0] : Sigma alpha, angular resolution  for plus charge (mrad) 
// PidPar_plus[1] : Sigma ms, multiple scattering    for plus charge (mrad GeV)
// PidPar_plus[2] : Sigma tof, tof resolution        for plus charge (ns)
// PidPar_plus[3] : K1, field integral value         for plus charge (mrad GeV)
//
// PidPar_minus[0] : Sigma alpha, angular resolution for minus charge (mrad) 
// PidPar_minus[1] : Sigma ms, multiple scattering   for minus charge (mrad GeV)
// PidPar_minus[2] : Sigma tof, tof resolution       for minus charge (ns)
// PidPar_minus[3] : K1, field integral value        for minus charge (mrad GeV)
// 
// MeanParPp : measured m2tof mean for pi+    (as a function of mom, [GeV^2]^2)
// MeanParPm : measured m2tof mean for pi-    (as a function of mom, [GeV^2]^2)
// MeanParKp : measured m2tof mean for K+     (as a function of mom, [GeV^2]^2)
// MeanParKm : measured m2tof mean for K-     (as a function of mom, [GeV^2]^2)
// MeanParPr : measured m2tof mean for proton (as a function of mom, [GeV^2]^2)
// MeanParPB : measured m2tof mean for pbar   (as a function of mom, [GeV^2]^2)
//==============================================================================

PidrecalReco::PidrecalReco(const char *name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");

  //  Zero the constants at construction...
  for (unsigned int i = 0; i < NPAR_PID; i++)
    {
      PidPar_plus[i]  = 0.0;
      PidPar_minus[i] = 0.0;
    }
  for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
    {
      MeanParPp[i] = 0.0;
      MeanParPm[i] = 0.0;
    }
  for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
    {
      MeanParKp[i] = 0.0;
      MeanParKm[i] = 0.0;
    }
  for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
    {
      MeanParPr[i] = 0.0;
      MeanParPb[i] = 0.0;
    }


  dataName_ParPlus  = "calib.pidrecal.par.plus"; 
  dataName_ParMinus = "calib.pidrecal.par.minus";
  dataName_MeanPp   = "calib.pidrecal.mean.pp";
  dataName_MeanPm   = "calib.pidrecal.mean.pm";
  dataName_MeanKp   = "calib.pidrecal.mean.kp";
  dataName_MeanKm   = "calib.pidrecal.mean.km";
  dataName_MeanPr   = "calib.pidrecal.mean.pr";
  dataName_MeanPb   = "calib.pidrecal.mean.pb";

  return ;
}

int
PidrecalReco::isValidRun(const int runno) const
{
  if (runno >= 106935 && runno <= 122223) // Run4 Au+Au 200 GeV 
    {
      return 1;
    }
  else if (runno >= 122466 && runno <= 123564) // Run4 Au+Au 62.4 GeV
    {
      return 1;
    }
  else if (runno >= 126496 && runno <= 130553) // Run4 p+p 200 GeV
    {
      return 1;
    }
  else if (runno >= 149539 && runno <= 160487) // Run5 Cu+Cu 200 GeV
    {
      return 1;
    }
  else if (runno >= 161196 && runno <= 163463) // Run5 Cu+Cu 62.4 GeV
    {
      return 1;
    }
  else if (runno >= 163604 && runno <= 163681) // Run5 Cu+Cu 22.5 GeV
    {
      return 1;
    }

  return 0;
}

int
PidrecalReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  if (verbosity > 0)
    {
      cout << "Fetching for run number: " << runnumber << endl;
    }
  calibration_ok = 1;
  fetchPidParPlus(runnumber);
  fetchPidParMinus(runnumber);
  fetchMeanPp(runnumber);
  fetchMeanPm(runnumber);
  fetchMeanKp(runnumber);
  fetchMeanKm(runnumber);
  fetchMeanPr(runnumber);
  fetchMeanPb(runnumber);

  return 0;
}

int
PidrecalReco::process_event(PHCompositeNode *topNode)
{
  if (!calibration_ok)
    {
      return ABORTEVENT;
    }

  d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

  //  Then "correct" m2tof
  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {
          PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
          float m2tof = sngltrk->get_m2tof();
	  // do not run if m2tof is not valid (track did not hit tof)
	  if (!sngltrk->isValid(m2tof))
	    {
	      continue;
	    }
          float mom = sngltrk->get_mom();
          short charge = sngltrk->get_charge();

          float nPi = IsPion(m2tof, mom, charge);
          float nK = IsKaon(m2tof, mom, charge);
          float nP = IsProton(m2tof, mom, charge);

	  sngltrk->ShutUp();
          sngltrk->set_isPi(nPi);
          sngltrk->set_isK(nK);
          sngltrk->set_isP(nP);
	  sngltrk->ShutUp(0);
        }
    }

  return 0;
}

void PidrecalReco::Print(const string &what) const
{
  Recalibrator::Print(what);
  if (what == "PAR")
    {
      cout << "Printing the PidRecal parameter constants:" << endl;

      // PID parameters ... 
      for (unsigned int i = 0; i < NPAR_PID; i++)
	{
	  cout << " PidPar_plus[" << i << "]= " << PidPar_plus[i] << endl;
	}
      for (unsigned int i = 0; i < NPAR_PID; i++)
	{
	  cout << " PidPar_minus[" << i << "]= " << PidPar_minus[i] << endl;
	}
      // Mean ...
      for (unsigned int i = 0; i < NPAR_MEAN_PION; i++) // pi+
	{
	  cout << " MeanParPp[" << i << "]= " << MeanParPp[i] << endl;
	}
      for (unsigned int i = 0; i < NPAR_MEAN_PION; i++) // pi-
	{
	  cout << " MeanParPm[" << i << "]= " << MeanParPm[i] << endl;
	}
      for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++) // K+
	{
	  cout << " MeanParKp[" << i << "]= " << MeanParKp[i] << endl;
	}
      for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++) // K-
	{
	  cout << " MeanParKm[" << i << "]= " << MeanParKm[i] << endl;
	}
      for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++) // Proton
	{
	  cout << " MeanParPr[" << i << "]= " << MeanParPr[i] << endl;
	}
      for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++) // Pbar
	{
	  cout << " MeanParPb[" << i << "]= " << MeanParPb[i] << endl;
	}
    }
  return ;
}

// Fetch form DB ____________________________________________________

void PidrecalReco::fetchPidParPlus(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_ParPlus, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_PID + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (PidParPlus)..." << endl;
      for (unsigned int i = 0; i < NPAR_PID; i++)
        {

          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          PidPar_plus[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " PidPar_plus[" << i << "]= " << PidPar_plus[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_ParPlus << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// -----
void PidrecalReco::fetchPidParMinus(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_ParMinus, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_PID + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (PidParMinus)..." << endl;
      for (unsigned int i = 0; i < NPAR_PID; i++)
        {

          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          PidPar_minus[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " PidPar_minus[" << i << "]= " << PidPar_minus[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_ParMinus << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// Pp
void PidrecalReco::fetchMeanPp(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_MeanPp, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_MEAN_PION + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (MeanPp)..." << endl;
      for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
        {
          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          MeanParPp[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " MeanParPp[" << i << "]= " << MeanParPp[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_MeanPp << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// Pm
void PidrecalReco::fetchMeanPm(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_MeanPm, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_MEAN_PION + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (MeanPm)..." << endl;
      for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
        {
          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          MeanParPm[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " MeanParPm[" << i << "]= " << MeanParPm[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_MeanPm << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// Kp
void PidrecalReco::fetchMeanKp(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_MeanKp, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_MEAN_KAON + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (MeanKp)..." << endl;
      for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
        {
          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          MeanParKp[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " MeanParKp[" << i << "]= " << MeanParKp[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_MeanKp << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// Km
void PidrecalReco::fetchMeanKm(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_MeanKm, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_MEAN_KAON + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (MeanKm)..." << endl;
      for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
        {
          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          MeanParKm[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " MeanParKm[" << i << "]= " << MeanParKm[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_MeanKm << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// Pr
void PidrecalReco::fetchMeanPr(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_MeanPr, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_MEAN_PROTON + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (MeanPr)..." << endl;
      for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
        {
          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          MeanParPr[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " MeanParPr[" << i << "]= " << MeanParPr[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_MeanPr << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}

// Pb
void PidrecalReco::fetchMeanPb(const int run)
{
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);

  //  Grap a pointer to the bank with name dataName......YOU MUST make dataName unique in the constructor!!!
  PdbParameter *parameter;
  int index = 0;
  PdbCalBank *pidBank = bankManager->fetchBank("PdbParameterBank", bankID, dataName_MeanPb, run);
  if (pidBank)
    {
      //  *** bank length check ***
      int length = NPAR_MEAN_PROTON + 2;
      int truelength = pidBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong length DB read for PidPar: " << truelength << endl;
          cout << "                                       expected length: " << length << endl;
          return ;
        }

      //  *** recognized scheme check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...unknown scheme DB read for PidPar:" << scheme << endl;
          return ;
        }

      //  *** internal data entries check ***
      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
	  cout << PHWHERE;
          cout << "PidrecalReco:: FATAL...wrong entries DB read for PidPar" << endl;
          return ;
        }

      //----------------------------------------------------
      // OK, ALL Checks passed...get the parameters...
      cout << "PidrecalReco::READING from Database (MeanPb)..." << endl;
      for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
        {
          parameter = (PdbParameter *) & pidBank->getEntry(index++);
          MeanParPb[i] = parameter->getParameter();

          if (verbosity > 0)
            cout << " MeanParPb[" << i << "]= " << MeanParPb[i] << endl;
        }
      delete pidBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibration in "
	   << dataName_MeanPb << " for run " << run << endl;
      calibration_ok = 0;
    }
  return ;
}



// Fetch from File ____________________________________________________

void
PidrecalReco::fetchPidParPlusFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (PidParPlus)..." << endl;

  for (unsigned int i = 0; i < NPAR_PID; i++)
    {
      file >> par;
      PidPar_plus[i] = par;

      cout << "PidPar_plus[" << i << "] = " << PidPar_plus[i] << endl;
    }

  file.close();
  return ;
}

// ---
void
PidrecalReco::fetchPidParMinusFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (PidParMinus)..." << endl;

  for (unsigned int i = 0; i < NPAR_PID; i++)
    {
      file >> par;
      PidPar_minus[i] = par;

      cout << "PidPar_minus[" << i << "] = " << PidPar_minus[i] << endl;
    }

  file.close();
  return ;
}

// Pp
void
PidrecalReco::fetchMeanPpFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (MeanPp)..." << endl;

  for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
    {
      file >> par;
      MeanParPp[i] = par;

      cout << "MeanParPp[" << i << "] = " << MeanParPp[i] << endl;
    }

  file.close();
  return ;
}

// Pm
void
PidrecalReco::fetchMeanPmFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (MeanPm)..." << endl;

  for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
    {
      file >> par;
      MeanParPm[i] = par;

      cout << "MeanParPm[" << i << "] = " << MeanParPm[i] << endl;
    }

  file.close();
  return ;
}

// Kp
void
PidrecalReco::fetchMeanKpFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (MeanKp)..." << endl;

  for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
    {
      file >> par;
      MeanParKp[i] = par;

      cout << "MeanParKp[" << i << "] = " << MeanParKp[i] << endl;
    }

  file.close();
  return ;
}

// Km
void
PidrecalReco::fetchMeanKmFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (MeanKm)..." << endl;

  for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
    {
      file >> par;
      MeanParKm[i] = par;

      cout << "MeanParKm[" << i << "] = " << MeanParKm[i] << endl;
    }

  file.close();
  return ;
}

// Pr
void
PidrecalReco::fetchMeanPrFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (MeanPr)..." << endl;

  for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
    {
      file >> par;
      MeanParPr[i] = par;

      cout << "MeanParPr[" << i << "] = " << MeanParPr[i] << endl;
    }

  file.close();
  return ;
}

// Pb
void
PidrecalReco::fetchMeanPbFromFile(const char * filename)
{
  ifstream file(filename);
  if (!file)
    {
      cout << "PidrecalReco:: Could not open input file " << filename << " !!" << endl;
      return ;
    }

  float par;

  cout << "PidPar fetching from file (MeanPb)..." << endl;

  for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
    {
      file >> par;
      MeanParPb[i] = par;

      cout << "MeanParPb[" << i << "] = " << MeanParPb[i] << endl;
    }

  file.close();
  return ;
}

// DB Update ____________________________________________________

void
PidrecalReco::updatePidParPlus(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_ParPlus);

  int length = NPAR_PID + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_PID; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( PidPar_plus[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// ---
void
PidrecalReco::updatePidParMinus(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_ParMinus);

  int length = NPAR_PID + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_PID; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( PidPar_minus[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// Pp
void
PidrecalReco::updateMeanPp(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_MeanPp);

  int length = NPAR_MEAN_PION + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( MeanParPp[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// Pm
void
PidrecalReco::updateMeanPm(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_MeanPm);

  int length = NPAR_MEAN_PION + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_MEAN_PION; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( MeanParPm[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// Kp
void
PidrecalReco::updateMeanKp(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_MeanKp);

  int length = NPAR_MEAN_KAON + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( MeanParKp[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// Km
void
PidrecalReco::updateMeanKm(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_MeanKm);

  int length = NPAR_MEAN_KAON + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_MEAN_KAON; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( MeanParKm[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// Pr
void
PidrecalReco::updateMeanPr(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_MeanPr);

  int length = NPAR_MEAN_PROTON + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( MeanParPr[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// Pb
void
PidrecalReco::updateMeanPb(int beginrun, int endrun)
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

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("PidrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  const char *descrip = "Parameters submitted by pid recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *pidBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, dataName_MeanPb);

  int length = NPAR_MEAN_PROTON + 2; // array + 2 hdr
  pidBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & pidBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (unsigned int i = 0; i < NPAR_MEAN_PROTON; i++)
    {

      parameter = (PdbParameter *) & pidBank->getEntry(index++);
      parameter->setParameter( MeanParPb[i] );
      parameter->setName("PAR");
    }

  application->commit();
  return ;
}

// ==== isPi, isK, isP ===

float
PidrecalReco::IsPion(const float m2tof, const float mom, const short charge)
{
  float nPi = 9999.9;
  float m2pi_Exp_mean = 9999.9;

  if (charge > 0) // pi+
    {
      if (mom <= 0.3)
        m2pi_Exp_mean = MeanParPp[0];
      else if (mom <= 0.4)
        m2pi_Exp_mean = MeanParPp[1];
      else if (mom <= 0.5)
        m2pi_Exp_mean = MeanParPp[2];
      else if (mom <= 0.6)
        m2pi_Exp_mean = MeanParPp[3];
      else if (mom <= 0.7)
        m2pi_Exp_mean = MeanParPp[4];
      else if (mom <= 0.8)
        m2pi_Exp_mean = MeanParPp[5];
      else if (mom <= 0.9)
        m2pi_Exp_mean = MeanParPp[6];
      else if (mom <= 1.0)
        m2pi_Exp_mean = MeanParPp[7];
      else if (mom <= 1.1)
        m2pi_Exp_mean = MeanParPp[8];
      else if (mom <= 1.2)
        m2pi_Exp_mean = MeanParPp[9];
      else if (mom <= 1.3)
        m2pi_Exp_mean = MeanParPp[10];
      else if (mom <= 1.4)
        m2pi_Exp_mean = MeanParPp[11];
      else if (mom <= 1.5)
        m2pi_Exp_mean = MeanParPp[12];
      else if (mom <= 1.6)
        m2pi_Exp_mean = MeanParPp[13];
      else if (mom <= 1.7)
        m2pi_Exp_mean = MeanParPp[14];
      else
        m2pi_Exp_mean = MeanParPp[15];
    }

  else if (charge < 0) // pi-
    {
      if (mom <= 0.3)
        m2pi_Exp_mean = MeanParPm[0];
      else if (mom <= 0.4)
        m2pi_Exp_mean = MeanParPm[1];
      else if (mom <= 0.5)
        m2pi_Exp_mean = MeanParPm[2];
      else if (mom <= 0.6)
        m2pi_Exp_mean = MeanParPm[3];
      else if (mom <= 0.7)
        m2pi_Exp_mean = MeanParPm[4];
      else if (mom <= 0.8)
        m2pi_Exp_mean = MeanParPm[5];
      else if (mom <= 0.9)
        m2pi_Exp_mean = MeanParPm[6];
      else if (mom <= 1.0)
        m2pi_Exp_mean = MeanParPm[7];
      else if (mom <= 1.1)
        m2pi_Exp_mean = MeanParPm[8];
      else if (mom <= 1.2)
        m2pi_Exp_mean = MeanParPm[9];
      else if (mom <= 1.3)
        m2pi_Exp_mean = MeanParPm[10];
      else if (mom <= 1.4)
        m2pi_Exp_mean = MeanParPm[11];
      else if (mom <= 1.5)
        m2pi_Exp_mean = MeanParPm[12];
      else if (mom <= 1.6)
        m2pi_Exp_mean = MeanParPm[13];
      else if (mom <= 1.7)
        m2pi_Exp_mean = MeanParPm[14];
      else
        m2pi_Exp_mean = MeanParPm[15];
    }
  else
    {
      return nPi;
    }

  nPi = (m2tof - m2pi_Exp_mean) / sigmaM2_analytical(m2pi_Exp_mean, mom, charge);
  return nPi;
}

float
PidrecalReco::IsKaon(const float m2tof, const float mom, const short charge)
{
  float nK = 9999.9;
  float m2k_Exp_mean = 9999.9;

  if (charge > 0) // K+
    {
      if (mom <= 0.5)
        m2k_Exp_mean = MeanParKp[0];
      else if (mom <= 0.6)
        m2k_Exp_mean = MeanParKp[1];
      else if (mom <= 0.7)
        m2k_Exp_mean = MeanParKp[2];
      else if (mom <= 0.8)
        m2k_Exp_mean = MeanParKp[3];
      else if (mom <= 0.9)
        m2k_Exp_mean = MeanParKp[4];
      else if (mom <= 1.0)
        m2k_Exp_mean = MeanParKp[5];
      else if (mom <= 1.1)
        m2k_Exp_mean = MeanParKp[6];
      else if (mom <= 1.2)
        m2k_Exp_mean = MeanParKp[7];
      else if (mom <= 1.3)
        m2k_Exp_mean = MeanParKp[8];
      else if (mom <= 1.4)
        m2k_Exp_mean = MeanParKp[9];
      else if (mom <= 1.5)
        m2k_Exp_mean = MeanParKp[10];
      else if (mom <= 1.6)
        m2k_Exp_mean = MeanParKp[11];
      else if (mom <= 1.7)
        m2k_Exp_mean = MeanParKp[12];
      else
        m2k_Exp_mean = MeanParKp[13];
    }

  else if (charge < 0) // K-
    {
      if (mom <= 0.5)
        m2k_Exp_mean = MeanParKm[0];
      else if (mom <= 0.6)
        m2k_Exp_mean = MeanParKm[1];
      else if (mom <= 0.7)
        m2k_Exp_mean = MeanParKm[2];
      else if (mom <= 0.8)
        m2k_Exp_mean = MeanParKm[3];
      else if (mom <= 0.9)
        m2k_Exp_mean = MeanParKm[4];
      else if (mom <= 1.0)
        m2k_Exp_mean = MeanParKm[5];
      else if (mom <= 1.1)
        m2k_Exp_mean = MeanParKm[6];
      else if (mom <= 1.2)
        m2k_Exp_mean = MeanParKm[7];
      else if (mom <= 1.3)
        m2k_Exp_mean = MeanParKm[8];
      else if (mom <= 1.4)
        m2k_Exp_mean = MeanParKm[9];
      else if (mom <= 1.5)
        m2k_Exp_mean = MeanParKm[10];
      else if (mom <= 1.6)
        m2k_Exp_mean = MeanParKm[11];
      else if (mom <= 1.7)
        m2k_Exp_mean = MeanParKm[12];
      else
        m2k_Exp_mean = MeanParKm[13];
    }
  else
    {
      return nK;
    }

  nK = (m2tof - m2k_Exp_mean) / sigmaM2_analytical(m2k_Exp_mean, mom, charge);
  return nK;
}

float
PidrecalReco::IsProton(const float m2tof, const float mom, const short charge)
{
  float nP = 9999.9;
  float m2p_Exp_mean = 9999.9;

  if (charge > 0) // proton
    {
      if (mom <= 0.6)
        m2p_Exp_mean = MeanParPr[0];
      else if (mom <= 0.7)
        m2p_Exp_mean = MeanParPr[1];
      else if (mom <= 0.8)
        m2p_Exp_mean = MeanParPr[2];
      else if (mom <= 0.9)
        m2p_Exp_mean = MeanParPr[3];
      else if (mom <= 1.0)
        m2p_Exp_mean = MeanParPr[4];
      else if (mom <= 1.1)
        m2p_Exp_mean = MeanParPr[5];
      else if (mom <= 1.2)
        m2p_Exp_mean = MeanParPr[6];
      else if (mom <= 1.3)
        m2p_Exp_mean = MeanParPr[7];
      else if (mom <= 1.4)
        m2p_Exp_mean = MeanParPr[8];
      else if (mom <= 1.5)
        m2p_Exp_mean = MeanParPr[9];
      else if (mom <= 1.6)
        m2p_Exp_mean = MeanParPr[10];
      else if (mom <= 1.7)
        m2p_Exp_mean = MeanParPr[11];
      else if (mom <= 1.8)
        m2p_Exp_mean = MeanParPr[12];
      else if (mom <= 1.9)
        m2p_Exp_mean = MeanParPr[13];
      else if (mom <= 2.0)
        m2p_Exp_mean = MeanParPr[14];
      else if (mom <= 2.2)
        m2p_Exp_mean = MeanParPr[15];
      else if (mom <= 2.4)
        m2p_Exp_mean = MeanParPr[16];
      else if (mom <= 2.6)
        m2p_Exp_mean = MeanParPr[17];
      else if (mom <= 2.8)
        m2p_Exp_mean = MeanParPr[18];
      else if (mom <= 3.0)
        m2p_Exp_mean = MeanParPr[19];
      else
        m2p_Exp_mean = MeanParPr[20];
    }

  else if (charge < 0) // pbar
    {
      if (mom <= 0.6)
        m2p_Exp_mean = MeanParPb[0];
      else if (mom <= 0.7)
        m2p_Exp_mean = MeanParPb[1];
      else if (mom <= 0.8)
        m2p_Exp_mean = MeanParPb[2];
      else if (mom <= 0.9)
        m2p_Exp_mean = MeanParPb[3];
      else if (mom <= 1.0)
        m2p_Exp_mean = MeanParPb[4];
      else if (mom <= 1.1)
        m2p_Exp_mean = MeanParPb[5];
      else if (mom <= 1.2)
        m2p_Exp_mean = MeanParPb[6];
      else if (mom <= 1.3)
        m2p_Exp_mean = MeanParPb[7];
      else if (mom <= 1.4)
        m2p_Exp_mean = MeanParPb[8];
      else if (mom <= 1.5)
        m2p_Exp_mean = MeanParPb[9];
      else if (mom <= 1.6)
        m2p_Exp_mean = MeanParPb[10];
      else if (mom <= 1.7)
        m2p_Exp_mean = MeanParPb[11];
      else if (mom <= 1.8)
        m2p_Exp_mean = MeanParPb[12];
      else if (mom <= 1.9)
        m2p_Exp_mean = MeanParPb[13];
      else if (mom <= 2.0)
        m2p_Exp_mean = MeanParPb[14];
      else if (mom <= 2.2)
        m2p_Exp_mean = MeanParPb[15];
      else if (mom <= 2.4)
        m2p_Exp_mean = MeanParPb[16];
      else if (mom <= 2.6)
        m2p_Exp_mean = MeanParPb[17];
      else if (mom <= 2.8)
        m2p_Exp_mean = MeanParPb[18];
      else if (mom <= 3.0)
        m2p_Exp_mean = MeanParPb[19];
      else
        m2p_Exp_mean = MeanParPb[20];
    }
  else
    {
      return nP;
    }

  nP = (m2tof - m2p_Exp_mean) / sigmaM2_analytical(m2p_Exp_mean, mom, charge);
  return nP;
}

float
PidrecalReco::sigmaM2_analytical(const float m2tof, const float mom, const short charge)
{
  if (mom == 0)
    return 9999;
  
  float SigmaAlpha = 0.0;
  float SigmaMs    = 0.0;
  float SigmaTof   = 0.0;
  float K1         = 0.0;

  if (charge > 0) 
    {
      SigmaAlpha = PidPar_plus[0];
      SigmaMs    = PidPar_plus[1];
      SigmaTof   = PidPar_plus[2];
      K1         = PidPar_plus[3];
    }
  else if (charge < 0)
    {
      SigmaAlpha = PidPar_minus[0];
      SigmaMs    = PidPar_minus[1];
      SigmaTof   = PidPar_minus[2];
      K1         = PidPar_minus[3];
    }

//   float Sigma = sqrt(4 * pow(SigmaAlpha / K1 * mom * m2tof, 2) +
//                      4 * pow(m2tof * SigmaMs / K1, 2) * (1 + m2tof / (mom * mom)) +
//                      4 * pow(SigmaTof / 17, 2) * (m2tof + mom * mom) * mom * mom);

  float Sigma = sqrt(4 * (SigmaAlpha / K1 * mom * m2tof)*(SigmaAlpha / K1 * mom * m2tof) +
                     4 * (m2tof * SigmaMs / K1)*(m2tof * SigmaMs / K1) * (1 + m2tof / (mom * mom)) +
                     4 * (SigmaTof / 17)*(SigmaTof / 17) * (m2tof + mom * mom) * mom * mom);

  return Sigma;
}
