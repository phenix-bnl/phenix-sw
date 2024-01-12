#include "EmctofrecalReco.h"
#include "MasterRecalibrator.h"

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <PHGlobal.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankList.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>

//needed for LaserLC
#include <emcCalibrationDataHelper.h>
#include <emcCalFEM.h>
#include <EmcIndexer.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>

#include <TH1.h>
#include <TH2.h>

#include <gsl/gsl_const.h>

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <map>
#include <sstream>

using namespace std;

EmctofrecalReco::EmctofrecalReco(const int slew, const string &name):
  Recalibrator(name),
  thisrunshift(0.),
  NumRuns(0),
  // set momentum dependent t0 shift to 0
  t0_shift(0.),
  //flag defaults
  FlagTimeFromRaw(true),
  FlagDeltaT(true),
  FlagRunbyRun(true),
  FlagT0Shift(true),//this correct the 'momentum dependance'
  FlagDB(1), //go to the correct db -- before =0, the stable db -- don't change this one
  SlewScheme(slew), // should set to 3 for tower by tower slewing
  runnumber(0),
  recalcnt(1),
  abortevent(0)
{
  memset(DeltaT,0,sizeof(DeltaT));// tower time shift
  memset(Gain,0,sizeof(Gain));// tower gains
  memset(SlewParameters,0,sizeof(SlewParameters)); // tower chi sq of slew fits

  //  Initialize constants to 9999.9...
  fill(&SlewParameters[0][0][0][0][0],&SlewParameters[0][0][0][0][0]+sizeof(SlewParameters)/sizeof(float),9999.0);

  // this is for PWG calibrations
  memset(a0,0,sizeof(a0));
  memset(a1,0,sizeof(a1));
  memset(a2,0,sizeof(a2));
  memset(a3,0,sizeof(a3));

  //  baseclasses.insert("emcClusterContainer");
  baseclasses.insert("PHCentralTrack");

  return ;
}


int
EmctofrecalReco::isValidRun(const int runno) const
{
  // range of calibrated runs for Run4
  if (runno >= 107515 && runno <= 123564)
    {
      return 1;
    }

  // Run5 Cu+Cu
  if (runno >= 150654 && runno <= 163681)
    {
      return 1;
    }
  
  // Run7 Au+Au
  if (runno >= 227016 && runno <= 240121)
    {
      return 1;
    }
  
  return 0;
}

int 
EmctofrecalReco::Init(PHCompositeNode *topNode) // all PWG stuff
{
  //
  // setting up Photon timing correction 
  //
  float b0[8] = { -1.026610e+00, -1.369900e+00, -1.508400e+00, -1.325410e+00, -1.719550e+00, -1.697250e+00, 2.525e+00, 1.769e+00 };
  float b1[8] = { 1.251240e+00, 1.579780e+00, 1.764870e+00, 1.555930e+00, 1.990800e+00, 2.019790e+00, -2.225e+00, -1.651e+00 };
  float b2[8] = { 9.451340e-01, 9.989640e-01, 1.020430e+00, 9.835980e-01, 1.053600e+00, 1.064600e+00, 8.688e-01, 7.741e-01 };
  float b3[8] = { 1.703190e-01, 1.697040e-01, 1.714140e-01, 1.783520e-01, 2.284610e-01, 3.087720e-01, 1.500e-01, 3.199e-01 };

//   ostringstream title;
   for (int i = 0;i < 8;i++)
     {
       a0[i] =b0[i];
       a1[i] =b1[i];
       a2[i] =b2[i];
       a3[i] =b3[i];

//       if(corrsec[i]) continue;
//       title.str("");
//       title << "CorrPhotonSec" << i;
//       corrsec[i] = new TF1(title.str().c_str(), "[0]+[1]/([2]+[3]*x*x)", 0.1, 7.1);
//       corrsec[i]->SetParameter(0, a0[i]);
//       corrsec[i]->SetParameter(1, a1[i]);
//       corrsec[i]->SetParameter(2, a2[i]);
//       corrsec[i]->SetParameter(3, a3[i]);
    }

  // Photon end
  return 0;
}

int 
EmctofrecalReco::InitRun(PHCompositeNode *topNode)
{
  // run at each new runnumber

  if (verbosity)    cout << "EmctofrecalReco InitRun" << endl;
  recoConsts *rc = recoConsts::instance();
  int runnumber =  rc->get_IntFlag("RUNNUMBER");
  if (verbosity)    cout << "Fetching for run number: " << runnumber << endl;

  //for Run5 Cu+Cu
  if (runnumber>=150654 && runnumber<=163681)
    {
      FlagRunbyRun = false;
    }

  //for Run7 Au+Au
  if (runnumber>=227016 && runnumber<=240121)
    {
      FlagRunbyRun = false;
    }

      //fetch calibration values
      if (FlagTimeFromRaw)
        fetchGain (runnumber);
      if (FlagDeltaT)
        fetchDeltaT (runnumber);
      if (FlagT0Shift)
        fetchT0Shift (runnumber);
      if (SlewScheme > 0)
        fetchSlew (runnumber);

  // get run by run shift
  if (FlagRunbyRun)
    {
      thisrunshift = 0;
      bool oldFetch=true;

      //if I don't have a map, get one
      if (RunShifts.empty())
	{
	  fetchRunbyRun(runnumber);
	  NumRuns = RunShifts.size();
	  oldFetch=false;
	}

      // search for runnumber in the map
      unsigned int runCount = RunShifts.count(runnumber);
      if (runCount ==0 && oldFetch)
	{
	  // might need to read a different row from the database
	  // because phtimestamp comes from runnumber
	  // and database picks the row based on phtimestamps of first and last runs of commit
	  // could have stepped over the boundary of one row and onto another
	  RunShifts.clear();
	  fetchRunbyRun(runnumber);
	  NumRuns = RunShifts.size();
	  oldFetch = false;
	  runCount = RunShifts.count(runnumber);
	}
      if (runCount==1)  thisrunshift = RunShifts[runnumber]; //there is a match!
      if (runCount == 0 && !oldFetch)
	{
	  // run is not in RunShifts, pick shift value of closest runnumber
	  
	  map<int,float>::iterator iter1;
	  iter1 = RunShifts.lower_bound(runnumber); // key greater than or equal to runnumber
	  int runAfter = iter1->first;
	  --iter1;
	  int runBefore = iter1->first; 

	  if ((runnumber - runBefore) <= (runAfter - runnumber)) //if before is closer
	    {
	      thisrunshift = RunShifts[runBefore];
	    }
	  else thisrunshift = RunShifts[runAfter];               //if after is closer
	}
      if (runCount > 1)
	{
	  //this should never ever happen --
	  cout << PHWHERE << "more than one entry for run " << runnumber << "don't know what to do" << endl;
	  cout << PHWHERE << "keep run shift value of 0 -- ie incorrect calibrations..." << endl;
	  cout << PHWHERE << "here are all " << runCount << " values: " << endl;

	  map<int,float>::iterator MapIter;
	  MapIter = RunShifts.find(runnumber);
	  if (MapIter!=RunShifts.end())
	    {
	      cout << MapIter->first << "    " << MapIter->second << endl;
	    }
	}	    

      if (thisrunshift == 0 && verbosity)
	{
	  cout << PHWHERE << "thisrunshift = " << thisrunshift << "for run " << runnumber << "BIG PROBLEM" <<endl;
	}
    }

  MasterRecalibrator *mr = GetMasterRecalibrator();
  phglobalnodes.clear(); // do not propagate this from previous run
  mr->searchNodeTree(topNode, "PHGlobal", phglobalnodes);

  vector<string>::const_iterator it;
  if (verbosity > 0)
    {
      for (it = phglobalnodes.begin(); it != phglobalnodes.end(); ++it)
        {
          cout << PHWHERE << "found Node with PHGlobal Object: " << *it << endl;
        }
    }

  return 0;
}

void EmctofrecalReco::Print(const string &what) const
{
  Recalibrator::Print(what);

  if (FlagTimeFromRaw || FlagDeltaT || SlewScheme == 3)
    {
      if (verbosity > 2)
	{
	  if (FlagTimeFromRaw)
	    cout << "Printing the gain constants:" << endl;
	  if (FlagDeltaT)
	    cout << "Printing the Delta-T constants:" << endl;
	  if (SlewScheme == 3)
	    cout << "Printing the Slew constants:" << endl;

	  for (int i = 0; i < EmcPar::NEMC_ARM; i++)
	    {
	      for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
		{
		  for (int k = 0; k < EmcPar::NEMC_Y; k++)
		    {
		      for (int l = 0; l < EmcPar::NEMC_Z; l++)
			{
// 			  cout << " arm: "    << i;
// 			  cout << " sector: " << j;
// 			  cout << " y: "      << k;
// 			  cout << " z: "      << l;
// 			  if (FlagTimeFromRaw) cout << " Gain: "  << Gain[i][j][k][l];
// 			  if (FlagDeltaT)      cout << " dt: "     << DeltaT[i][j][k][l];
// 			  if (SlewScheme == 3)
// 			    {
// 			      cout << " Slew: ";
// 			      cout << " " << SlewParameters[i][j][k][l][0];
// 			      cout << " " << SlewParameters[i][j][k][l][1];
// 			      cout << " " << SlewParameters[i][j][k][l][2];
// 			      cout << " " << SlewParameters[i][j][k][l][3];
// 			    }
			  cout << endl;
			}
		    }
		}
	    }
	  cout << endl;
	}
    }
  
  if (FlagRunbyRun)
    {
      if (verbosity > 2)
	{
// 	  TH1D *RunValues = new TH1D ("RunValues", "RunValues", 16000, 107499.5, 123500.5);
// 	  cout << PHWHERE << "Printing the runbyrun shifts: " << endl;

// 	  map<int,float>::iterator MapIter;
// 	  for (MapIter = RunShifts.begin(); MapIter != RunShifts.end(); MapIter++)
// 	    {
// 	      cout << " run: " << MapIter->first << "  shift: " << MapIter->second << endl;

// 	      //		  RunValues->Fill(MapIter->first, abs(MapIter->second));
// 	      RunValues->Fill(MapIter->first, MapIter->second);
// 	    }

// 	  RunValues->GetXaxis()->SetTitle("Run Number");
// 	  RunValues->GetYaxis()->SetTitle("Run-By-Run Shift");
// 	  RunValues->Draw("P");
	}
    }

  if (FlagT0Shift)
    {
      //        cout << "Printing the t0 shift: " << t0_shift << endl;
    }

  return ;
}

int EmctofrecalReco::process_event(PHCompositeNode *topNode)
{

  if (abortevent)
    {
      return ABORTEVENT;
    }

  if (recalcnt)
    {

      //get PHCentral obj
      PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

      //don't calibrate without CNT
      if (!d_cnt)
	{
	  if (verbosity > 0) cout << PHWHERE << "No PHCentralTrack" << endl;
	  return 0;
	}
      if (verbosity) cout << "EmctofrecalReco::process_event -- d_cnt " << endl;

      //get PHGlobal obj
      vector<string>::const_iterator it;
      PHGlobal *d_gbl = 0;
      for (it = phglobalnodes.begin(); it != phglobalnodes.end(); ++it)
	{
	  d_gbl = findNode::getClass<PHGlobal>(topNode, (*it).c_str());
	  if (d_gbl->isValid(d_gbl->getBbcTimeZero()))
	    {
	      if (verbosity > 0)
		{
		  cout << "Found PHGlobal node with BBC T0: " << *it << endl;
		  cout << "  prepare to break..." << endl;
		}
	      break;
	    }
	}

      //don't calibrate without global -- need bbct0
      if (!d_gbl) 
	{
	  if (verbosity > 0) cout << PHWHERE << "No PHGlobal" << endl;
	  return 0;
	}

      // get t0 from bbc -- will need this for TimefromRaw
      float TZero = d_gbl->getBbcTimeZero();

      //don't calibrate if these methods aren't available
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
	{
	  PHSnglCentralTrack *sngltrk_check = d_cnt->get_track(i); 
	  sngltrk_check->ShutUp();
	  int impl1 = sngltrk_check->isImplemented(sngltrk_check->get_emcrawtdc());
	  int impl2 = sngltrk_check->isImplemented(sngltrk_check->get_emcrawadc());
	  sngltrk_check->ShutUp(0);
	  if (!impl1 || !impl2)
	    {
	      // this object cannot be recalibrated so we just switch the
	      // CNT recalibration off. The InitRun(...) will reset this value
	      // in case the next object of this name has a recalibratable cnt
	      if (verbosity) cout << PHWHERE << "object lacks neccessary methods" << endl;
	      recalcnt = 0;
	      return 0;
	    }

	  // don't calibrate if these methods aren't valid
	  if (!	(sngltrk_check->isValid(sngltrk_check->get_dcarm())) &&
		(sngltrk_check->isValid(sngltrk_check->get_sect ())) &&
		(sngltrk_check->isValid(sngltrk_check->get_ysect())) &&
	        (sngltrk_check->isValid(sngltrk_check->get_zsect())) )
	    {
	      if (verbosity) cout << PHWHERE << "missing valid values" << endl;
	      continue;
	    }
	  
	  // if flags are on apply calibrations
	  if (FlagTimeFromRaw)	    applyTimeFromRaw(sngltrk_check, TZero);
	  if (FlagDeltaT )	    applyDeltaT     (sngltrk_check);
	  if (FlagRunbyRun )
	    {
	      int iret = applyRunbyRun (sngltrk_check);
	      if (iret < 0)
		{
		  return iret;
		}
	    }
	  if (SlewScheme > 0 )	    applySlewing    (sngltrk_check);
	  if (FlagT0Shift )	    applyT0Shift    (sngltrk_check);

	  // if you have changed the timing, update the mass^2
	  //  Almost always update the mass^2...
	  if (FlagTimeFromRaw || FlagDeltaT || (SlewScheme > 0) || FlagRunbyRun || FlagT0Shift)
	    {
	      if (verbosity) cout << " mass before: " << sngltrk_check->get_m2emc() << endl;
	      static const double c = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns
	  
	      float plemc = sngltrk_check->get_plemc();
	      float temc  = sngltrk_check->get_temc();
	      float mom   = sngltrk_check->get_mom();
	      sngltrk_check->set_m2emc(mom*mom*(c*c*temc*temc / plemc / plemc - 1.0));
	      if (verbosity) cout << " mass after: " << sngltrk_check->get_m2emc() << endl;
		  
	    }
	}
    }
  if (verbosity) cout << "done with process_event" << endl;
  return 0;
}

void EmctofrecalReco::applyTimeFromRaw(PHSnglCentralTrack *sngltrk, const float TZero)
{
  if (verbosity) cout << " temc before correction: " << sngltrk->get_temc() << endl;

  int arm = sngltrk->get_dcarm();
  int sect = sngltrk->get_sect ();
  int ysect = sngltrk->get_ysect();
  int zsect = sngltrk->get_zsect();
      
  // Not for PbGl...
  if (arm == 0 && sect < 2)    return;
      
  //  Check that index is in the array
  if ( arm < 0 || arm >= EmcPar::NEMC_ARM)       return;
  if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)  return;
  if (ysect < 0 || ysect >= EmcPar::NEMC_Y)      return;
  if (zsect < 0 || zsect >= EmcPar::NEMC_Z)   	return;
      
  float emcrawtdc = sngltrk->get_emcrawtdc();
  // raw time * gain - bbc t0
  float TimeFromRaw = emcrawtdc * Gain[arm][sect][ysect][zsect] - TZero;

  // reset time with the new calculations
  sngltrk->set_temc (TimeFromRaw);

  if (verbosity)   cout << "temc after correction: " << sngltrk->get_temc() << endl;

  return ;
}

void EmctofrecalReco::applyDeltaT(PHSnglCentralTrack *sngltrk)
{
  int arm = sngltrk->get_dcarm();
  int sect = sngltrk->get_sect();
  int ysect = sngltrk->get_ysect();
  int zsect = sngltrk->get_zsect();

  // Not for PbGl...
  if (arm == 0 && sect < 2)      return;

  //  Check that index is in array
  if ( arm < 0 || arm >= EmcPar::NEMC_ARM)         return;
  if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)    return;
  if (ysect < 0 || ysect >= EmcPar::NEMC_Y)        return;
  if (zsect < 0 || zsect >= EmcPar::NEMC_Z)        return;

  //flag to see where get the time from
  float temc = sngltrk->get_temc();
  // reset time with the new calculations -- time - shift for that tower
  sngltrk->set_temc (temc - DeltaT[arm][sect][ysect][zsect]);

  return ;
}

void EmctofrecalReco::applySlewing(PHSnglCentralTrack *sngltrk)
{
  //place where slewing schems are defined! sort of also see EmctofCal
  if (SlewScheme != 3)
    {
      cout << PHWHERE << "Obsolete scheme.  You should switch to scheme 3 or turn off slewing scheme 0" << endl;
    }
  if (SlewScheme == 3)
    {  
      if (verbosity) cout << "temc before slewing: " << sngltrk->get_temc() << endl;

      int arm = sngltrk->get_dcarm();
      int sect = sngltrk->get_sect ();      
      int ysect = sngltrk->get_ysect();
      int zsect = sngltrk->get_zsect();
      
      // Not for PbGl...
      if (arm == 0 && sect < 2)	return;
      
      //  Check that index is in the array
      if (  arm < 0 || arm >= EmcPar::NEMC_ARM )	    return;
      if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)  return;
      if (ysect < 0 || ysect >= EmcPar::NEMC_Y )	    return;
      if (zsect < 0 || zsect >= EmcPar::NEMC_Z )	    return;

      // get pulse height from adc value
      float adc = sngltrk->get_emcrawadc();
      
      // get slewing parameters
      float param0 = SlewParameters[arm][sect][ysect][zsect][0];
      float param1 = SlewParameters[arm][sect][ysect][zsect][1];
      float param2 = SlewParameters[arm][sect][ysect][zsect][2];
      
      //set new timing with calibrations -- subtract the function [0]/adc + [1]/(adc^2) + [2]
      sngltrk->set_temc (sngltrk->get_temc() - (param0 / adc + param1 / (adc*adc) + param2));

      if (verbosity) cout << "temc after slewing: " << sngltrk->get_temc();
    }
  return ;
}

int EmctofrecalReco::applyRunbyRun(PHSnglCentralTrack *sngltrk)
{
  if (thisrunshift == 0)
    {
      abortevent = 1;
      recoConsts *rc = recoConsts::instance();
      int runnumber = rc->get_IntFlag("RUNNUMBER");
      cout << PHWHERE << "No T0 shift calibration for run " << runnumber;
      cout << ", aborting all events from this run without further comment" << endl;
      return ABORTEVENT;
    }
  else
    {
      float temc = sngltrk->get_temc();
      // set new time using calibration -- - run shift
      sngltrk->set_temc(temc - thisrunshift);
    }

  return EVENT_OK;
}

void EmctofrecalReco::applyT0Shift(PHSnglCentralTrack *sngltrk)
{
  float temc = sngltrk->get_temc();
  // set new time using calibration -- + t0_shift
  sngltrk->set_temc (temc + t0_shift);

  return;
}

void EmctofrecalReco::fetchDeltaT(const int run)
{
  //GETS values from database

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
  //  b   /   arm
  //  o   |   sector
  //  d   |   ysect
  //  y   |   zsect
  //      \   DeltaT
  //
  //  The number of times the body repeats will be EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 2-4-2004
  //

  if (verbosity)
    cout << "EmctofrecalReco::fetchDeltaT" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "calib.test.emctofrecal";
  else if (FlagDB == 2)
    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
  //  if (!bankID) cout << PHWHERE << "      NO BANKID HOW CAN THIS BE!!!" << endl;

  PdbApplication* application = bankManager->getApplication();
  application->startRead();
  if (!application->startRead())
    {
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (deltaBank)
    {

      if (verbosity)
	{
	  deltaBank->print();
	  deltaBank->printEntry(1);
	}
      int index = 0;

      //----------------------------------------------------
      //  three checks...length of record, scheme and no. entries...
      int length = 5 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2;
      int truelength = deltaBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong length DB read for delta t: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong scheme DB read for delta-t" << endl;
          return ;
        }

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          return ;
        }


      //----------------------------------------------------
      if (verbosity)
        {
          cout << "READ from Database: Slat Delta-T" << endl;
        }
      for (int i = 0; i < EmcPar::NEMC_ARM; i++)
        {
          for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
            {
              for (int k = 0; k < EmcPar::NEMC_Y; k++)
                {
                  for (int l = 0; l < EmcPar::NEMC_Z; l++)
                    {

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int arm = (int)parameter->getParameter(); //read arm

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int sect = (int)parameter->getParameter(); // read sector

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int ysect = (int)parameter->getParameter(); // read y value

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int zsect = (int)parameter->getParameter(); // read z value

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      DeltaT[arm][sect][ysect][zsect] = parameter->getParameter();
		      // read delta t shift for the tower with arm,sector,y,z and put in array

                      // 	  if (verbosity)
                      // 	    {
                      // 	      cout << arm    << " ";
                      // 	      cout << sect << " ";
                      // 	      cout << ysect  << " ";
                      // 	      cout << zsect  << " ";
                      // 	      cout << DeltaT[arm][sect][ysect][zsect]  << " ";
                      // 	      cout << endl;
                      // 	    }
                    }
                }
            }
        }
      delete deltaBank;
    }
  else
    {
      cout << PHWHERE << " Could not find calibrations from "
	   << nameDB << " for run " 
           << run << endl;
      exit(1);
    }
  return ;
}

void EmctofrecalReco::updateDeltaT(const int beginrun, const int endrun)
{
  //STORES values in database

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
  //  b   /   arm
  //  o   |   sector
  //  d   |   ysect
  //  y   |   zsect
  //      \   DeltaT
  //
  //  The number of times the body repeats will be EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 2-4-2004
  //
  if (verbosity)
    {
      cout << PHWHERE << "EmctofrecalReco::updateDeltaT of runnumbers ";
      cout << PHWHERE << beginrun << " to " << endrun << endl;
    }

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
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "calib.test.emctofrecal";
  else if (FlagDB == 2)
    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 5 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2; // array + 2 hdr
  deltaBank->setLength(length); // set length

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme"); // set scheme

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries"); // set entries

  // loop over DeltaT array and put arm,sector,y,z and deltaT values in database
  for (int i = 0; i < EmcPar::NEMC_ARM; i++)
    {
      for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
        {
          for (int k = 0; k < EmcPar::NEMC_Y; k++)
            {
              for (int l = 0; l < EmcPar::NEMC_Z; l++)
                {

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("Ysect");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("Zsect");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(DeltaT[i][j][k][l]);
                  parameter->setName("dt");
                }
            }
        }
    }

  cout << "commit dt" << endl;
  application->commit();

  return ;
}

void EmctofrecalReco::fetchSlew(const int run)
{
  // READ the slewing parameters from the database

  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of Slew calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //  body = float list...
  //
  //  The number of times the body repeats will depend upon the scheme.
  //
  //                          TKH 2-4-2004
  //

  if (verbosity)
    cout << "EmctofrecalReco::fetchSlew" << endl;

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calib.test.emctofslew";

  //  Grap a pointer to the bank...
  PdbParameter *parameter;
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  if (deltaBank)
    {
      if (verbosity == 0)
	{
	  cout << "Slew READ from Database: arm, sector, y, z, 3 parameters, and chisq" << endl;
	}

      int index = 0;
      int length = 8 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2; 
      //each tower has 8 values saved b/c 4 for arm sector y and z + 3 params + 1 chisq, +2 for the scheme and entries

      int truelength = deltaBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong length DB read for slewing: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      SlewScheme = (int)parameter->getParameter();

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong entries DB read for slewing" << endl;
          return ;
        }

      //----------------------------------------------------
      if (verbosity)
        cout << PHWHERE << "READ from Database: Slewing" << endl;
      for (int i = 0; i < EmcPar::NEMC_ARM; i++)
        {
          for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
            {
              for (int k = 0; k < EmcPar::NEMC_Y; k++)
                {
                  for (int l = 0; l < EmcPar::NEMC_Z; l++)
                    {

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int arm = (int)parameter->getParameter();

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int sect = (int)parameter->getParameter();

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int ysect = (int)parameter->getParameter();

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      int zsect = (int)parameter->getParameter();

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      SlewParameters[arm][sect][ysect][zsect][0] = parameter->getParameter(); //parameter 0

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      SlewParameters[arm][sect][ysect][zsect][1] = parameter->getParameter(); // parameter 1

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      SlewParameters[arm][sect][ysect][zsect][2] = parameter->getParameter(); // parameter 2

                      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                      SlewParameters[arm][sect][ysect][zsect][3] = parameter->getParameter(); // chi squared of fit

                      // 	  if (verbosity)
                      // 	    {
                      // 	      cout << arm    << " ";
                      // 	      cout << sect << " ";
                      // 	      cout << ysect  << " ";
                      // 	      cout << zsect  << " ";
                      // 	      cout << SlewParameters[arm][sect][ysect][zsect][0]  << " ";
                      // 	      cout << SlewParameters[arm][sect][ysect][zsect][1]  << " ";
                      // 	      cout << SlewParameters[arm][sect][ysect][zsect][2]  << " ";
                      // 	      cout << SlewParameters[arm][sect][ysect][zsect][3]  << " ";
                      // 	      cout << endl;
                      // 	    }
                    }
                }
            }
        }
      delete deltaBank;
    }
  else
    {
      cout << PHWHERE << PHWHERE
      << " Could not load calibration from "
      << nameDB << " for run "
      << run << endl;
      exit(1);
    }
  return ;
}

void
EmctofrecalReco::updateSlew(const int beginrun, const int endrun)
{
  // PUT slewing parameters into database

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
  //  body = float list...
  //
  //  The number of times the body repeats will depend upon scheme.
  //
  //                          TKH 2-4-2004
  //
  if (verbosity)
    {
      cout << PHWHERE << "EmctofrecalReco::updateSlew of runnumbers ";
      cout << beginrun << " to " << endrun << endl;
    }

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
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calib.test.emctofslew";
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  // put in headers
  int length = 8 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2; // array + 2 hdr
  deltaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(SlewScheme);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  // put in array
  for (int i = 0; i < EmcPar::NEMC_ARM; i++)
    {
      for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
        {
          for (int k = 0; k < EmcPar::NEMC_Y; k++)
            {
              for (int l = 0; l < EmcPar::NEMC_Z; l++)
                {

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("Ysect");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("Zsect");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(SlewParameters[i][j][k][l][0]);
                  parameter->setName("par0");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(SlewParameters[i][j][k][l][1]);
                  parameter->setName("par1");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(SlewParameters[i][j][k][l][2]);
                  parameter->setName("par2");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(SlewParameters[i][j][k][l][3]);
                  parameter->setName("chisq");
                }
            }
        }
    }

  cout << "commit slewing array" << endl;
  application->commit();

  return ;
}

void EmctofrecalReco::fetchLaserLC (const int runnumber)
{
  // this was used to get the laser LC values from the emc databases
  // don't use this

  if (verbosity)
    cout << PHWHERE << "EmctofrecalReco::fetchLaserLC" << endl;

  // create histogram
  char histoname[500];
  sprintf (histoname, "HistoLC_%i", runnumber);
  int channels = EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z;
  float max = (float)channels - 0.5;
  TH2F *HistoLC = new TH2F(histoname, histoname, channels, -0.5, max, 200, 10, 50);

  //create and reset array
  float maxValue[EmcPar::NEMC_ARM][EmcPar::NEMC_SECTOR][EmcPar::NEMC_Y][EmcPar::NEMC_Z];
  memset(maxValue,0,sizeof(maxValue));

  // gets values from emcdatabase
  int towerid = 0;
  int arm = 0;
  int sect = 0;
  int y = 0;
  int z = 0;
  float value = 0.0;
  const char* calibType = "LCTofs";
  emcCalibrationDataHelper cdh(runnumber, false);

  //fill HistoLC with db values
  for (unsigned int absolute_fem_number = 0; absolute_fem_number < 171; absolute_fem_number++)
    {  //just PbSc goes upto 171
      const emcCalFEM *lc = cdh.getCalibration(absolute_fem_number, calibType);
      if (!lc)
        {
          cout << PHWHERE << "NULL pointer " << absolute_fem_number << endl;
          continue;
        }
      for (unsigned int femchannel = 0; femchannel < 144; femchannel++)
        {
          //get new values
          towerid = EmcIndexer::PXSM144iCH_iPX(absolute_fem_number, femchannel);
          EmcIndexer::TowerLocation (towerid, arm, sect, y, z);
          value = lc->getValue(femchannel);

          //different arm labeling for dc arm, which is what i use
          arm = arm + 1;
          if (arm > 1)
            arm = 0;

          int index = z + EmcPar::NEMC_Z * y + EmcPar::NEMC_Z * EmcPar::NEMC_Y * sect + EmcPar::NEMC_Z * EmcPar::NEMC_Y * EmcPar::NEMC_SECTOR * arm;
          HistoLC->Fill(index, value);
        }
    }

  //slice HistoLC and get max and put into maxValue array
  //  float scale =31.563; //the same as value used in get time from raw except positive
  for (int i = 0; i < EmcPar::NEMC_ARM; i++)
    {
      for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
        {
          for (int k = 0; k < EmcPar::NEMC_Y; k++)
            {
              for (int l = 0; l < EmcPar::NEMC_Z; l++)
                {
                  //slice HistoLC and find max
                  int index2 = l + EmcPar::NEMC_Z * k + EmcPar::NEMC_Z * EmcPar::NEMC_Y * j + EmcPar::NEMC_Z * EmcPar::NEMC_Y * EmcPar::NEMC_SECTOR * i;
                  char slicename [500];
                  sprintf (slicename, "HistoLCslice_a%i_s%i_y%02i_z%02i", i, j, k, l);
                  TH1D* slice = HistoLC->ProjectionY(slicename, index2 + 1, index2 + 1);  // first bin = bin _1_
                  maxValue [i][j][k][l] = slice->GetBinCenter(slice->GetMaximumBin());
                  //maxvalue can't be 0
                  if (maxValue[i][j][k][l] == 0)
                    {
                      cout << PHWHERE << "Error LCtofs maxValue can't be 0, divide by 0!!! set gain to 1" << endl;
                      Gain [i][j][k][l] = -31.563;
                    }
                  else
                    {
                      Gain [i][j][k][l] = -1 / maxValue[i][j][k][l];
                    }
                  // 		  if (verbosity>0)
                  // 		    {
                  // 		      cout << "LC for arm: ";
                  // 		      cout << i << " sect: " << j << " y: " << k << " z: " << l << " is ";
                  // 		      cout << Gain[i][j][k][l]  << " ";
                  // 		      cout << endl;
                  // 		    }
                  delete slice;
                }
            }
        }
    }
  delete HistoLC;
  return ;
}

void EmctofrecalReco::fetchGain(const int run)
{
  // READ gain values for each tower

  //  OK gang...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of gain calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  |  entries == number of entries that *SHOULD* follow
  //     \  flightTimeReference
  //
  //  b   /   arm
  //  d   |   sector, y ,z
  //  y   \   gain
  //
  //  The number of times the body repeats will be EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 2-4-2004
  //

  if (verbosity)
    cout << "EmctofrecalReco::fetchGain" << endl;

  if (FlagDB == 2)
    fetchLaserLC(runnumber);
  else
    {
      //  Make the managers...
      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();
      if (!application->startRead())
        {
          PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }

      //  Make a bank ID...
      PdbBankID bankID(1);
      string nameDB;
      // pick database name --don't change
      if (FlagDB == 0)
        nameDB = "calib.emctofgain";
      else if (FlagDB == 1)
        nameDB = "calib.test.emctofppc";
      else
        {
          cout << PHWHERE << "Error unacceptable database flag" << endl;
          return ;
        }

      //  Grap a pointer to the bank...
      PdbParameter *parameter;
      PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

      //----------------------------------------------------
      //  OK...now is the time to actually unpack the data...
      //  three checks...length of record, scheme and no. entries...
      if (deltaBank)
        {
	  int index = 0;
          int length = 5 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2; 
	  // 2 headers (scheme and entries) and 5 parameters (arm ,sector, y, z, gain) for each tower
          int truelength = deltaBank->getLength();
          //length check
          if (length != truelength)
            {
              cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong length DB read for gains" << endl;
              return ;
            }

          //scheme check
          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
          int scheme = (int)parameter->getParameter();
          if (scheme != 1)
            {
              cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong scheme DB read for gains" << endl;
              return ;
            }

          //no. entries check
          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
          int entries = (int)parameter->getParameter();
          if (entries != length - 2)
            //      if (entries != length-3)
            {
              cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong entries DB read for ppc" << endl;
              return ;
            }


          //----------------------------------------------------
          //  Checks passed...get the parameters...

          if (verbosity)
            cout << "READ from Database: Gains" << endl;
          for (int i = 0; i < EmcPar::NEMC_ARM; i++)
            {
              for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
                {
                  for (int k = 0; k < EmcPar::NEMC_Y; k++)
                    {
                      for (int l = 0; l < EmcPar::NEMC_Z;l++)
                        {
                          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                          int arm = (int)parameter->getParameter();

                          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                          int sect = (int)parameter->getParameter();

                          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                          int y = (int)parameter->getParameter();

                          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                          int z = (int)parameter->getParameter();

                          parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                          Gain[arm][sect][y][z] = parameter->getParameter(); //want them all

                          //  		      if (verbosity>0)
                          //  			{
                          //  			  cout << PHWHERE << "ppc: ";
                          //  			  cout << arm    << " ";
                          //  			  cout << sect << " ";
                          //    			  cout << y << " ";
                          //    			  cout << z << " ";
                          //  			  cout << Gain[arm][sect][y][z]  << " ";
                          // 			  //  cout << Gain[arm][sect][0][0]  << " ";
                          // 			  cout << endl;
                          //    			}
                        }
                    }
                }
            }
          delete deltaBank;
        }
      else
        {
          cout << PHWHERE
          << " Could not load calibration from "
          << nameDB << " for run "
          << run << endl;
          exit(1);

        }
      //      cout << "done with Gains" << endl;
    }
  return ;

}

void EmctofrecalReco::updateGain(const int beginrun, const int endrun)
{
  //UPDATE database with new gain values

  //  OK gang...now it gets MORE intense.
  //  In this routine we will be storing the entire bank
  //  of Ppc calibration parameters...YEE HAW!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  |  entries == number of entries that *SHOULD* follow
  //
  //  b   /   arm
  //  d   |   sector, y,z
  //  y   \   gain
  //
  //  The number of times the body repeats will be EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR
  //  in the first scheme known as scheme 1.
  //
  //                          TKH 2-4-2004
  //

  if (FlagDB == 2)
    {
      cout << PHWHERE << "Can't update LC db" << endl;
      return ;
    }

  if (verbosity)
    {
      cout << "EmctofrecalReco::updateGain of runnumbers ";
      cout << beginrun << " to " << endrun << endl;
    }

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
  cout << " done PdbApplication" << endl;
  if (!application->startUpdate())
    {
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  cout << " done bankID" << endl;
  string nameDB;
  // pick a database with flags --don't change this
  if (FlagDB == 0)
    nameDB = "calib.emctofgain";
  else if (FlagDB == 1)
    nameDB = "calib.test.emctofppc";
  else
    {
      cout << PHWHERE << "unacceptable FlagDB" << endl;
      return ;
    }
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 5 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2;
  // array(5 parameters for each tower) + 2 hdr
  deltaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  for (int i = 0; i < EmcPar::NEMC_ARM; i++)
    {
      for (int j = 0; j < EmcPar::NEMC_SECTOR; j++)
        {
          for (int k = 0; k < EmcPar::NEMC_Y; k++)
            {
              for (int l = 0; l < EmcPar::NEMC_Z; l++)
                {
                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("y");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("z");

                  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
                  parameter->setParameter(Gain[i][j][k][l]);
                  parameter->setName("Gain");
                }
            }
        }
    }

  application->commit();
  return ;
}

void EmctofrecalReco::fetchRunbyRun(const int run)
{
  // GET runbyrun shifts from the database

  // based on fetchDeltaT
  // header contains: scheme, entries, number of runs
  // body contains runnumber and shift in an array called RunShifts
  // -scc 1/6/05

  //  Make the managers...
  if (verbosity)
    cout << "EmctofrecalReco::fetchRunbyRun of runnumber: " << run << endl;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB;
  // pick database name -- don't change this
  if (FlagDB == 0)
    nameDB = "calib.emctofrun";
  else if (FlagDB == 1)
    nameDB = "calib.test.emctofrun";
  else if (FlagDB == 2)
    nameDB = "calib.emctoflcrun";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }

  //  Grap a pointer to the bank...
  PdbParameter *parameter;
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  three checks...scheme, length of record and no entries...
  if (deltaBank)
    {
      int index = 0;
      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
	{
	  cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong scheme DB read for runbyrun" << endl;
	  return ;
	}
      else if (verbosity)
	cout << "Good, fetchRunbyRun scheme = " << scheme << ", = 1 " << endl;

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      // need the length to check the number of entries

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      NumRuns = (int)parameter->getParameter();
      if (verbosity)
	cout << "NumRuns: " << NumRuns << endl;
      int length = 2 * NumRuns + 3; // 3 b/c header
      int truelength = deltaBank->getLength();
      if (length != truelength)
	{
	  cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong length DB read for runbyrun: " << truelength << endl;
	  cout << "                  expected length:                          " << length << endl;
	  return ;
	}
      else if (verbosity)
	cout << "Good, fetchRunbyRun length " << length << " = truelength " << truelength << endl;

      // needed length,to check the number of entries
      if (entries != length - 3)       //2 b/c header
	{
	  cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong entries DB read for runbyrun" << endl;
	  return ;
	}
      else if (verbosity)
	{
	  cout << "Good, fetchRunbyRun entries " << entries;
	  cout << " = length " << length << " - 3" << endl;
	}

      //----------------------------------------------------
      //  Checks passed...get the parameters...
      if (verbosity)
	cout << "READ from Database: RunbyRun Shifts" << endl;

      for (int j = 0; j < NumRuns; j++)
	{
	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  int runKey = (int) parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float shiftValue = parameter->getParameter();

	  if (shiftValue !=0)
	    {
	      RunShifts[runKey] = shiftValue; 
	      // 	  pair<int,float>OneRunShift(runKey, shiftValue);
	      // 	  RunShifts.insert(OneRunShift);  //insert key and shift into map
	      //	      cout << runKey << "   " << RunShifts[runKey] << "     " << shiftValue-RunShifts[runKey] << endl;
	    }
	}
      delete deltaBank;
    }
  else
    {
      cout << PHWHERE << " Could not load calibration from " << nameDB << " for run " << run << endl;
      exit(1);
    }

  return ;
}

void EmctofrecalReco::updateRunbyRun(const int beginrun, const int endrun)
{
  // UPDATE the database with new runbyrun values

  //based off of update ppc
  // header contains: scheme, entries, number of runs
  // body contains runnumber and shift in an array called RunShifts
  // -scc 1/6/05

  if (verbosity)
    {
      cout << "EmctofrecalReco::updateRunbyRun of runnumbers ";
      cout << beginrun << " to " << endrun << endl;
    }

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
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB;
  // pick database name --don't change
  if (FlagDB == 0)
    nameDB = "calib.emctofrun";
  else if (FlagDB == 1)
    nameDB = "calib.test.emctofrun";
  else if (FlagDB == 2)
    nameDB = "calib.emctoflcrun";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 2 * NumRuns + 3; // array has 2 parameters for each run (run, shift) + 3hdr (scheme, entries, number of runs)
  if (verbosity)
    cout << "NumRuns: " << NumRuns << endl;
  if (verbosity)
    cout << "length: " << length << endl;
  deltaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");
  if (verbosity)
    cout << "scheme: 1.0" << endl;

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 3); // b/c 3 hdr
  parameter->setName("entries");
  if (verbosity)
    cout << "entries: " << length - 3 << endl;

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(NumRuns);
  parameter->setName("size");
  if (verbosity)
    cout << "size: " << NumRuns << endl;

  map<int,float>::iterator MapIter;
  for (MapIter = RunShifts.begin(); MapIter != RunShifts.end(); ++MapIter)
    {
      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->first);
      parameter->setName("Run");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second);
      parameter->setName("Shift");
    }

  application->commit();
  return ;
}

void EmctofrecalReco::fetchT0Shift(const int run)
{
  // GET t0 shift -- removed momentum dependence

  // based on fetchDeltaT
  // header contains: scheme, entries, number of runs
  // body contains a double called t0_shift
  // -scc 1/6/05

  //  Make the managers...
  if (verbosity)
    cout << "EmctofrecalReco::fetchT0Shift of runnumber: " << run << endl;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB;
  nameDB = "calib.emctofmom";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  three checks...scheme, length of record and no entries...
  if (deltaBank)
    {
      int index = 0;
      PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong scheme DB read for t0 shift" << endl;
          return ;
        }
      else if (verbosity)
        cout << "Good, fetcht0shift scheme = " << scheme << ", = 1 " << endl;

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      // need the length to check the number of entries

      int length = 1 + 3; // 3 b/c header and 1 for the value -- only 1 value for all runs and all towers
      int truelength = deltaBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong length DB read for t0 shift: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }
      else if (verbosity)
        cout << "Good, fetchT0Shift length " << length << " = truelength " << truelength << endl;

      // needed length,to check the number of entries
      if (entries != length - 3)       //2 b/c header
        {
          cout << PHWHERE << "EmctofrecalReco:: FATAL...wrong entries DB read for t0 shift" << endl;
          return ;
        }
      else if (verbosity)
        {
          cout << "Good, fetchT0Shift entries " << entries;
          cout << " = length " << length << " - 3" << endl;
        }

      //----------------------------------------------------
      //  Checks passed...get the parameters...
      if (verbosity)
        cout << "READ from Database: T0Shift" << endl;
      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      t0_shift = (double)parameter->getParameter();

      if (verbosity)
        {
          cout << "t0 shift = " << t0_shift << endl;
        }
      delete deltaBank;
    }
  else
    {
      cout << PHWHERE
      << " Could not load calibration from "
      << nameDB << " for run "
      << run << endl;
      exit(1);
    }
  return ;
}

void EmctofrecalReco::updateT0Shift(const int beginrun, const int endrun)
{
  // update a new t0 shift in database

  //based off of update runbyrun
  // header contains: scheme, entries, number of runs
  // body contains double called t0_shift
  // -scc 1/6/05

  if (verbosity)
    {
      cout << "EmctofrecalReco::updateT0Shift of runnumbers ";
      cout << beginrun << " to " << endrun << endl;
    }

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
    Tstop.setToFarFuture();

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startUpdate())
    {
      PHMessage("EmctofrecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB;
  nameDB = "calib.emctofmom";
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 1 + 3; // 1 value + 3hdr
  if (verbosity)
    cout << "length: " << length << endl;
  deltaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme");
  if (verbosity)
    cout << "scheme: 1.0" << endl;

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 3); // b/c 3 hdr
  parameter->setName("entries");
  if (verbosity)
    cout << "entries: " << length - 3 << endl;

  // set value t0_shift
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(t0_shift);
  parameter->setName("t0");

  application->commit();
  return ;
}
