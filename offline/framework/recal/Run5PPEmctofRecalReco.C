#include "Run5PPEmctofRecalReco.h"
#include "MasterRecalibrator.h"

#include <PHCentralTrack.h>
#include <PHGlobal.h>
#include <PHSnglCentralTrack.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankList.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>


#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>

#include <TH1.h>
#include <TH2.h>

#include <gsl/gsl_const.h>

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>

using namespace std;

struct rbr_sect{
   float e2;
   float e3;
   float w0;
   float w1;
   float w2;
   float w3;
};

typedef map<int, struct rbr_sect> rbr_map;
static rbr_map rbr_vals;
rbr_sect thisrunshift;
	      
static const double c = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns
Run5PPEmctofRecalReco::Run5PPEmctofRecalReco(const string &name):
  Recalibrator(name),
  NumRuns(0),
  // set momentum dependent t0 shift to 0
  t0_shift(0),
  recalcnt(1), // this works for cnts only but if vars are missing we need
  abortevent(0)
{

  memset(Gain,0,sizeof(Gain));
  memset(DeltaT,0,sizeof(DeltaT));
  memset(SlewParameters,0,sizeof(SlewParameters));
  memset(warnmap,0,sizeof(warnmap));
  // clear run shifts map
  thisrunshift.e2 = 0;
  thisrunshift.e3 = 0;
  thisrunshift.w0 = 0;
  thisrunshift.w1 = 0;
  thisrunshift.w2 = 0;
  thisrunshift.w3 = 0;

  baseclasses.insert("PHCentralTrack");

  return ;
}

int
Run5PPEmctofRecalReco::isValidRun(const int runno) const
{
   //range of run-by-run offsets from Kazuya
  if (runno >= 168314 && runno <= 179846)
    {
      return 1;
    }
  return 0;
}

int 
Run5PPEmctofRecalReco::InitRun(PHCompositeNode *topNode)
{

  if (verbosity)    cout << "Run5PPEmctofRecalReco InitRun" << endl;
  recoConsts *rc = recoConsts::instance();
  int runnumber =  rc->get_IntFlag("RUNNUMBER");
  if (verbosity)    cout << "Fetching for run number: " << runnumber << endl;

  //get the calibrations
  fetchGain(runnumber);
  fetchDeltaT(runnumber);
  fetchSlew(runnumber);
  //fetchT0Shift(runnumber);  we don't have one for run5
  fetchRunbyRun(runnumber);
  fetchWarnmap(runnumber);

      
  unsigned int runCount = rbr_vals.count(runnumber);
  
  if(runCount==1){
     thisrunshift.e2=rbr_vals[runnumber].e2;
     thisrunshift.e3=rbr_vals[runnumber].e3;
     thisrunshift.w0=rbr_vals[runnumber].w0;
     thisrunshift.w1=rbr_vals[runnumber].w1;
     thisrunshift.w2=rbr_vals[runnumber].w2;
     thisrunshift.w3=rbr_vals[runnumber].w3;
  }
      
  if (!runCount){
     // run is not in rbr_vals, pick shift value of closest runnumber
     // from Sarah's code

     rbr_map::iterator iter1;
     iter1 = rbr_vals.lower_bound(runnumber); // key greater than or equal to runnumber
     int runAfter = iter1->first;
     --iter1;
     int runBefore = iter1->first; 

     if ((runnumber - runBefore) <= (runAfter - runnumber)) //if before is closer
     {
	thisrunshift.e2 = rbr_vals[runBefore].e2;
	thisrunshift.e3 = rbr_vals[runBefore].e3;
	thisrunshift.w0 = rbr_vals[runBefore].w0;
	thisrunshift.w1 = rbr_vals[runBefore].w1;
	thisrunshift.w2 = rbr_vals[runBefore].w2;
	thisrunshift.w3 = rbr_vals[runBefore].w3;
     }
     else{
       	thisrunshift.e2 = rbr_vals[runAfter].e2;      //if after is closer
       	thisrunshift.e3 = rbr_vals[runAfter].e3;      //if after is closer
       	thisrunshift.w0 = rbr_vals[runAfter].w0;      //if after is closer
       	thisrunshift.w1 = rbr_vals[runAfter].w1;      //if after is closer
       	thisrunshift.w2 = rbr_vals[runAfter].w2;      //if after is closer
       	thisrunshift.w3 = rbr_vals[runAfter].w3;      //if after is closer
     }
  }
  if (runCount > 1){
     //this should never ever happen --
     cout << PHWHERE << "more than one entry for run " << runnumber << "don't know what to do" << endl;
  }

  MasterRecalibrator *mr = GetMasterRecalibrator();
  phglobalnodes.clear(); // do not propagate this from previous run
  mr->searchNodeTree(topNode, "PHGlobal", phglobalnodes);

  return 0;
}

int Run5PPEmctofRecalReco::process_event(PHCompositeNode *topNode)
{

  if (abortevent)  return ABORTEVENT;


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
      if (verbosity) cout << "Run5PPEmctofRecalReco::process_event -- d_cnt " << endl;

      //get PHGlobal obj
      vector<string>::const_iterator it;
      PHGlobal *d_gbl = 0;
      for (it = phglobalnodes.begin(); it != phglobalnodes.end(); ++it)
	{
	  d_gbl = findNode::getClass<PHGlobal>(topNode, (*it).c_str());
	}

      //don't calibrate without global -- need bbct0
      if (!d_gbl) 
	{
	  if (verbosity > 0) cout << PHWHERE << "No PHGlobal" << endl;
	  return 0;
	}

      // get t0 from bbc -- will need this for TimefromRaw
      float TZero = d_gbl->getBbcTimeZero();

      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
	{
	   PHSnglCentralTrack *sngltrk_check = d_cnt->get_track(i); 
	   sngltrk_check->ShutUp();
	  if(!i){ //only check on 1st event
   	     int imp = sngltrk_check->isImplemented(sngltrk_check->get_emcrawtdc());
   	     imp += sngltrk_check->isImplemented(sngltrk_check->get_emcrawadc());
   	     imp += sngltrk_check->isImplemented(sngltrk_check->get_dcarm());
   	     imp += sngltrk_check->isImplemented(sngltrk_check->get_sect());
   	     imp += sngltrk_check->isImplemented(sngltrk_check->get_ysect());
   	     imp += sngltrk_check->isImplemented(sngltrk_check->get_zsect());
   	     sngltrk_check->ShutUp(0);
   	     if (imp!=6)
 	     {
	      // this object cannot be recalibrated so we just switch the
	      // CNT recalibration off. The InitRun(...) will reset this value
	      // in case the next object of this name has a recalibratable cnt
  		if (verbosity) cout << PHWHERE << "object lacks neccessary methods" << endl;
  		recalcnt = 0;
  		return 0;
	    }
	  }
	  if(sngltrk_check->get_dcarm()==0 && sngltrk_check->get_sect()<2)continue; //don't do PbGl

	  // apply calibrations
	  if(verbosity){
	     cout << TZero << " ";
	     cout << sngltrk_check->get_temc() << " ";
	     cout << sngltrk_check->get_dcarm() << " ";
	     cout << sngltrk_check->get_sect() << " ";
	     cout << sngltrk_check->get_ysect() << " ";
	     cout << sngltrk_check->get_zsect() << " ";
	     cout << sngltrk_check->get_emcrawadc() << " ";
	     cout << sngltrk_check->get_emcrawtdc() << endl;
	  }
	  int iret=0;
	  iret = applyWarn(sngltrk_check);
	  if(iret==-1){
	     sngltrk_check->set_m2emc(-9999); //don't calibrate tracks that hit bad towers
	     continue;
	  }

	  applyTimeFromRaw(sngltrk_check, TZero);
	  applyDeltaT(sngltrk_check);
	  iret = applyRunbyRun(sngltrk_check);
	  if(iret==-1){
	     sngltrk_check->set_m2emc(-9999); //don't calibrate tracks that don't have run by run
	     continue;
	  }
	  applySlewing(sngltrk_check);
//	  applyT0Shift(sngltrk_check);

	  //update mass
	  if (verbosity) cout << " mass before: " << sngltrk_check->get_m2emc() << endl;
	  float plemc = sngltrk_check->get_plemc();
      	  float temc  = sngltrk_check->get_temc();
	  float mom   = sngltrk_check->get_mom();
	  if(verbosity)cout << "after recal: " << sngltrk_check->get_temc() << endl;
	  sngltrk_check->set_m2emc(mom*mom*(c*c*temc*temc / plemc / plemc - 1.0));
	  if (verbosity) cout << " mass after: " << sngltrk_check->get_m2emc() << endl;

	}
    }
  if (verbosity) cout << "done with process_event" << endl;
  return 0;
}

int Run5PPEmctofRecalReco::applyWarn(PHSnglCentralTrack *sngltrk)
{
   int arm=sngltrk->get_dcarm();
   int sect=sngltrk->get_sect();
   int ysect=sngltrk->get_ysect();
   int zsect=sngltrk->get_zsect();
   
  //  Check that index is in the array
  if ( arm < 0 || arm >= EmcPar::NEMC_ARM)       return -1;
  if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)  return -1;
  if (ysect < 0 || ysect >= EmcPar::NEMC_Y)      return -1;
  if (zsect < 0 || zsect >= EmcPar::NEMC_Z)   	return -1;

  int remove=0;
  if(warnmap[arm][sect][ysect][zsect]>0 && warnmap[arm][sect][ysect][zsect]<600)remove=1;
  if(warnmap[arm][sect][ysect][zsect]==1000) remove=1;

  if(remove){
     if(verbosity){
	cout << "bad tower " << arm << " ";
	cout << sect << " " << ysect << " ";
	cout << zsect << " val " << warnmap[arm][sect][ysect][zsect] << endl;
     }
     return -1;
  }
  return EVENT_OK;
}

     
void Run5PPEmctofRecalReco::applyTimeFromRaw(PHSnglCentralTrack *sngltrk, const float TZero)
{
  if (verbosity) cout << " temc before correction: " << sngltrk->get_temc() << endl;

  int arm = sngltrk->get_dcarm();
  int sect = sngltrk->get_sect ();
  int ysect = sngltrk->get_ysect();
  int zsect = sngltrk->get_zsect();
      
  //  Check that index is in the array
  if ( arm < 0 || arm >= EmcPar::NEMC_ARM)       return;
  if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)  return;
  if (ysect < 0 || ysect >= EmcPar::NEMC_Y)      return;
  if (zsect < 0 || zsect >= EmcPar::NEMC_Z)   	return;
      
  float emcrawtdc = sngltrk->get_emcrawtdc();
  // raw time * gain - bbc t0 (factor of 1/1000 from Kazuya's recal)
  float TimeFromRaw = -1.0*emcrawtdc*Gain[arm][sect][ysect][zsect]/1000.0;
  TimeFromRaw-=TZero;

  // reset time with the new calculations
  sngltrk->set_temc (TimeFromRaw);
  if (verbosity)   cout << "temc after correction: " << sngltrk->get_temc() << endl;
  return ;
}

void Run5PPEmctofRecalReco::applyDeltaT(PHSnglCentralTrack *sngltrk)
{
  int arm = sngltrk->get_dcarm();
  int sect = sngltrk->get_sect();
  int ysect = sngltrk->get_ysect();
  int zsect = sngltrk->get_zsect();

  //  Check that index is in array
  if ( arm < 0 || arm >= EmcPar::NEMC_ARM)         return;
  if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)    return;
  if (ysect < 0 || ysect >= EmcPar::NEMC_Y)        return;
  if (zsect < 0 || zsect >= EmcPar::NEMC_Z)        return;

  float temc = sngltrk->get_temc();
  if(verbosity)cout << "before DeltaT " << temc;
  // reset time with the new calculations -- time - shift for that tower
  sngltrk->set_temc (temc - DeltaT[arm][sect][ysect][zsect]);
  if(verbosity)cout << " after-> " << sngltrk->get_temc() << endl;

  return ;
}

void Run5PPEmctofRecalReco::applySlewing(PHSnglCentralTrack *sngltrk)
{
   if (verbosity) cout << "temc before slewing: " << sngltrk->get_temc() << endl;
   int arm = sngltrk->get_dcarm();
   int sect = sngltrk->get_sect ();      
   int ysect = sngltrk->get_ysect();
   int zsect = sngltrk->get_zsect();

   //  Check that index is in the array
   if (  arm < 0 || arm >= EmcPar::NEMC_ARM )	    return;
   if ( sect < 0 || sect >= EmcPar::NEMC_SECTOR)  return;
   if (ysect < 0 || ysect >= EmcPar::NEMC_Y )	    return;
   if (zsect < 0 || zsect >= EmcPar::NEMC_Z )	    return;

   // get pulse height from adc value
   float adc = sngltrk->get_emcrawadc();

   // get slewing parameters
   float p0 = SlewParameters[arm][sect][ysect][zsect][0];
   float p1 = SlewParameters[arm][sect][ysect][zsect][1];
   float p2 = SlewParameters[arm][sect][ysect][zsect][2];
   float p3 = SlewParameters[arm][sect][ysect][zsect][3];
   float p4 = SlewParameters[arm][sect][ysect][zsect][4];

   
   if(verbosity){
      cout << "slew parameters " << p0 << " ";
      cout << p1 << " "<< p2 << " " << p3 << " " << p4 << endl;
   }
   //set new timing with calibrations 
   //subtract [0]+[1]/x+[2]/x^2+[3]/x^3+[4]/x^4
   float dslew = p0+p1/adc+p2/(adc*adc)+p3/(adc*adc*adc)+p4/(adc*adc*adc*adc);

   sngltrk->set_temc (sngltrk->get_temc() - dslew);
   
   if (verbosity) cout << "temc after slewing: " << sngltrk->get_temc();
   return ;
}

int Run5PPEmctofRecalReco::applyRunbyRun(PHSnglCentralTrack *sngltrk)
{
  if (thisrunshift.e2==0)
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
      if(verbosity){
	 cout << "before rbr " << temc;
	 cout << "shifts " << thisrunshift.e2 << " ";
	 cout << thisrunshift.e3 << " ";
	 cout << thisrunshift.w0 << " ";
	 cout << thisrunshift.w1 << " ";
	 cout << thisrunshift.w2 << " ";
	 cout << thisrunshift.w3 << " " << endl;
      }
      // set new time using calibration -- - run shift
      if(sngltrk->get_dcarm()==0&&sngltrk->get_sect()==2)
         sngltrk->set_temc(temc - thisrunshift.e2);
      else if(sngltrk->get_dcarm()==0&&sngltrk->get_sect()==3)
         sngltrk->set_temc(temc - thisrunshift.e3);
      else if(sngltrk->get_dcarm()==1&&sngltrk->get_sect()==0)
         sngltrk->set_temc(temc - thisrunshift.w0);
      else if(sngltrk->get_dcarm()==1&&sngltrk->get_sect()==1)
         sngltrk->set_temc(temc - thisrunshift.w1);
      else if(sngltrk->get_dcarm()==1&&sngltrk->get_sect()==2)
         sngltrk->set_temc(temc - thisrunshift.w2);
      else if(sngltrk->get_dcarm()==1&&sngltrk->get_sect()==3)
         sngltrk->set_temc(temc - thisrunshift.w3);
      if(verbosity)cout << "after-> " << sngltrk->get_temc() << endl;
    }

  return EVENT_OK;
}

void Run5PPEmctofRecalReco::applyT0Shift(PHSnglCentralTrack *sngltrk)
{
   //no run5 T0 shift right now
  return;
}

void Run5PPEmctofRecalReco::fetchDeltaT(const int run)
{
   if(verbosity)cout << "fetching DeltaT" << endl;
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbBankID bankID(1);
  string nameDB = "calibtestemctofrecal";
  PdbApplication* application = bankManager->getApplication();

  application->startRead();
  if (!application->startRead())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (deltaBank){
     if (verbosity){
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
   	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong length DB read for delta t: " << truelength << endl;
 	cout << "                  expected length:                          " << length << endl;
	return ;
     }

     PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
     int scheme = (int)parameter->getParameter();
     if (scheme != 1)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong scheme DB read for delta-t" << endl;
	return ;
     }

     parameter = (PdbParameter *) & deltaBank->getEntry(index++);
     int entries = (int)parameter->getParameter();
     if (entries != length - 2)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
	return ;
     }

     for (int i = 0; i < EmcPar::NEMC_ARM; i++){
	for (int j = 0; j < EmcPar::NEMC_SECTOR; j++){
	   for (int k = 0; k < EmcPar::NEMC_Y; k++){
	      for (int l = 0; l < EmcPar::NEMC_Z; l++){
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
		 if(arm==1&&sect==3&&ysect==25&&zsect==44)cout << DeltaT[arm][sect][ysect][zsect] << " THIS is your deltat " << endl;

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

void Run5PPEmctofRecalReco::updateDeltaT(const int beginrun, const int endrun)
{
  if (verbosity)
    {
      cout << PHWHERE << "Run5PPEmctofRecalReco::updateDeltaT of runnumbers ";
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
  else Tstop.setToFarFuture();

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startUpdate())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB = "calibtestemctofrecal";
  string descrip = "Parameters for run5 pp emcal timing";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  // array + 2 header lines
  int length = 5 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2;
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
  for (int i = 0; i < EmcPar::NEMC_ARM; i++){
     for (int j = 0; j < EmcPar::NEMC_SECTOR; j++){
	for (int k = 0; k < EmcPar::NEMC_Y; k++){
	   for (int l = 0; l < EmcPar::NEMC_Z; l++){

	      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	      parameter->setParameter(i);
	      parameter->setName("Arm");

	      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	      parameter->setParameter(j);
	      parameter->setName("Sector");

	      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      	      parameter->setParameter(k);
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

void Run5PPEmctofRecalReco::fetchSlew(const int run)
{
  if (verbosity)cout << "Run5PPEmctofRecalReco::fetchSlew" << endl;

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calibtestemctofslew";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  if (deltaBank)
    {
      if (verbosity == 0)
        cout << "Slew READ from Database: arm, sector, y, z, 3 parameters, and chisq" << endl;

  int index = 0;
      int length=9*EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z+2; 
      //each tower has 9 values saved... 4 for the tower + 5 for the fit +2 for the scheme and entries

      int truelength = deltaBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong length DB read for slewing: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      if (strcmp(parameter->getName(),"scheme"))
	{
	  cout << "invalid first parameter " << parameter->getName() << endl;
	}
      // if we ever have a scheme, here is the place to check it
      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong entries DB read for slewing" << endl;
          return ;
        }

      //----------------------------------------------------
      if (verbosity)
        cout << PHWHERE << "READ from Database: Slewing" << endl;
      for (int i = 0; i < EmcPar::NEMC_ARM; i++){
	 for (int j = 0; j < EmcPar::NEMC_SECTOR; j++){
	    for (int k = 0; k < EmcPar::NEMC_Y; k++){
	       for (int l = 0; l < EmcPar::NEMC_Z; l++){
		  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
		  int arm = (int)parameter->getParameter();
                  
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  int sect = (int)parameter->getParameter();
                  
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  int ysect = (int)parameter->getParameter();
                  
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  int zsect = (int)parameter->getParameter();
                  
		  //par0
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  SlewParameters[arm][sect][ysect][zsect][0] = parameter->getParameter(); 

		  //par1
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  SlewParameters[arm][sect][ysect][zsect][1] = parameter->getParameter(); 
          
		  //par2
      		  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  		  SlewParameters[arm][sect][ysect][zsect][2] = parameter->getParameter(); 
                  
		  //par3
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  SlewParameters[arm][sect][ysect][zsect][3] = parameter->getParameter(); 
		  
		  //par4
	      	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  	  SlewParameters[arm][sect][ysect][zsect][4] = parameter->getParameter(); 
                    
		  if(verbosity){
		     if(arm==1&&sect==3&&ysect==25&&zsect==44){
			cout << "slew parameters ";
			cout << SlewParameters[arm][sect][ysect][zsect][0] << " ";
			cout << SlewParameters[arm][sect][ysect][zsect][1] << " ";
			cout << SlewParameters[arm][sect][ysect][zsect][2] << " ";
			cout << SlewParameters[arm][sect][ysect][zsect][3] << " ";
			cout << SlewParameters[arm][sect][ysect][zsect][4] << " ";
			cout << endl;
		     }
		  }
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
Run5PPEmctofRecalReco::updateSlew(const int beginrun, const int endrun)
{
  if (verbosity)
    {
      cout << PHWHERE << "Run5PPEmctofRecalReco::updateSlew of runnumbers ";
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
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calibtestemctofslew";
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  // put in headers
  int length=9*EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z+2; // array + 2 hdr
  deltaBank->setLength(length);

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setName("scheme");

  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries");

  // put in array
  for (int i = 0; i < EmcPar::NEMC_ARM; i++){
     for (int j = 0; j < EmcPar::NEMC_SECTOR; j++){
	for (int k = 0; k < EmcPar::NEMC_Y; k++){
	   for (int l = 0; l < EmcPar::NEMC_Z; l++){

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
	      parameter->setName("par3");

	      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	      parameter->setParameter(SlewParameters[i][j][k][l][4]);
	      parameter->setName("par4");
	   }
	}
     }
  }

  cout << "commit slewing array" << endl;
  application->commit();

  return ;
}

void Run5PPEmctofRecalReco::fetchGain(const int run)
{
  if (verbosity)
    cout << "Run5PPEmctofRecalReco::fetchGain" << endl;

      
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
  {
     PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not readable");
     application->abort();
  }
  
  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB;
  // pick database name --don't change
  nameDB = "calibtestemctofppc";  //again Sarah's naming scheme
  				    
  //  Grap a pointer to the bank...
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
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong length DB read for gains" << endl;
	return ;
     }
     
     //scheme check
     PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
     int scheme = (int)parameter->getParameter();
     if (scheme != 1)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong scheme DB read for gains" << endl;
	return ;
     }
     
     
     //no. entries check
     parameter = (PdbParameter *) & deltaBank->getEntry(index++);
     int entries = (int)parameter->getParameter();
     if (entries != length - 2)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong entries DB read for ppc" << endl;
	return ;
     }

     //----------------------------------------------------
     //  Checks passed...get the parameters...
     if (verbosity)
	cout << "READ from Database: Gains" << endl;
     for (int i = 0; i < EmcPar::NEMC_ARM; i++){
	for (int j = 0; j < EmcPar::NEMC_SECTOR; j++){
	   for (int k = 0; k < EmcPar::NEMC_Y; k++){
	      for (int l = 0; l < EmcPar::NEMC_Z;l++){
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
  cout << "done reading gains" << endl;
 
  return;
  
}

void Run5PPEmctofRecalReco::updateGain(const int beginrun, const int endrun)
{
  if (verbosity)
    {
      cout << "Run5PPEmctofRecalReco::updateGain of runnumbers ";
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
  else Tstop.setToFarFuture();

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  cout << " done bankID" << endl;
  string nameDB;
  // pick a database with flags --don't change this
    
  nameDB = "calibtestemctofppc"; //these parameters are not Sarah's
  				   //ppc correction
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 5*EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z + 2;
  // array(5 parameter for each tower) + 2 hdr
  // (4 to identify the tower and one gain value)
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

void Run5PPEmctofRecalReco::fetchWarnmap(const int run)
{
  if (verbosity)
    cout << "Run5PPEmctofRecalReco::fetchWarnmap" << endl;

      
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
  {
     PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not readable");
     application->abort();
  }
  
  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calibemctofwarn";
  				    
  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  
  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  three checks...length of record, scheme and no. entries...
  if (deltaBank)
  {
     int index = 0;
     int length = 5 * EmcPar::NEMC_ARM * EmcPar::NEMC_SECTOR * EmcPar::NEMC_Y * EmcPar::NEMC_Z + 2; 
     // 2 headers (scheme and entries) and 5 parameters (arm ,sector, y, z, val) for each tower
     int truelength = deltaBank->getLength();
     //length check
     if (length != truelength)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong length DB read for gains" << endl;
	return ;
     }
     
     //scheme check
      PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
     int scheme = (int)parameter->getParameter();
     if (scheme != 1)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong scheme DB read for gains" << endl;
	return ;
     }
     
     
     //no. entries check
     parameter = (PdbParameter *) & deltaBank->getEntry(index++);
     int entries = (int)parameter->getParameter();
     if (entries != length - 2)
     {
	cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong entries DB read for warnmap" << endl;
	return ;
     }

     if (verbosity)
	cout << "READ from Database: Warnmap" << endl;
     for (int i = 0; i < EmcPar::NEMC_ARM; i++){
	for (int j = 0; j < EmcPar::NEMC_SECTOR; j++){
	   for (int k = 0; k < EmcPar::NEMC_Y; k++){
	      for (int l = 0; l < EmcPar::NEMC_Z;l++){
		 parameter = (PdbParameter *) & deltaBank->getEntry(index++);
		 int arm = (int)parameter->getParameter();
		 parameter = (PdbParameter *) & deltaBank->getEntry(index++);
		 int sect = (int)parameter->getParameter();
		 parameter = (PdbParameter *) & deltaBank->getEntry(index++);
		 int y = (int)parameter->getParameter();

		 parameter = (PdbParameter *) & deltaBank->getEntry(index++);
		 int z = (int)parameter->getParameter();

		 parameter = (PdbParameter *) & deltaBank->getEntry(index++);
		 warnmap[arm][sect][y][z] = parameter->getParameter();
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
  cout << "done reading warnmap" << endl;
 
  return;
  
}

void Run5PPEmctofRecalReco::updateWarnmap(const int beginrun, const int endrun)
{
  if (verbosity)
    {
      cout << "Run5PPEmctofRecalReco::updateWarnmap of runnumbers ";
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
  else Tstop.setToFarFuture();

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calibemctofwarn";
  string descrip = "Parameters submitted by recal object";

  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 5*EmcPar::NEMC_ARM*EmcPar::NEMC_SECTOR*EmcPar::NEMC_Y*EmcPar::NEMC_Z + 2;
  // array(5 parameter for each tower) + 2 hdr
  // (4 to identify the tower and one gain value)
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
                  parameter->setParameter(warnmap[i][j][k][l]);
                  parameter->setName("Warnmap");
                }
            }
        }
    }

  application->commit();
  return ;
}

void Run5PPEmctofRecalReco::fetchRunbyRun(const int run)
{
  // GET runbyrun shifts from the database

  if (verbosity) cout << "Run5PPEmctofRecalReco::fetchRunbyRun of runnumber: " << run << endl;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB = "calibtestemctofrun";
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

  if (deltaBank)
    {
      int index=0;
       PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
	{
	  cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong scheme DB read for runbyrun" << endl;
	  return ;
	}
      else if (verbosity) cout << "Good, fetchRunbyRun scheme = " << scheme << ", = 1 " << endl;

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      // need the length to check the number of entries

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      NumRuns = (int)parameter->getParameter();
      if (verbosity)
	cout << "NumRuns: " << NumRuns << endl;
      int length = 7 * NumRuns + 3; // 3 b/c header
      int truelength = deltaBank->getLength();
      if (length != truelength)
	{
	  cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong length DB read for runbyrun: " << truelength << endl;
	  cout << "                  expected length:                          " << length << endl;
	  return ;
	}
      else if (verbosity)
	cout << "Good, fetchRunbyRun length " << length << " = truelength " << truelength << endl;

      // needed length,to check the number of entries
      if (entries != length - 3)       //2 b/c header
	{
	  cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong entries DB read for runbyrun" << endl;
	  return ;
	}
      else if (verbosity)cout << "Good, fetchRunbyRun entries " << entries;

      if (verbosity)
	cout << "READ from Database: RunbyRun Shifts" << endl;

      for (int j = 0; j < NumRuns; j++)
	{
	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  int runKey = (int) parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float e2 = parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float e3 = parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float w0 = parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float w1 = parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float w2 = parameter->getParameter();

	  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
	  float w3 = parameter->getParameter();

	  if (runKey !=0)
	    {
	      rbr_vals[runKey].e2=e2;
	      rbr_vals[runKey].e3=e3;
	      rbr_vals[runKey].w0=w0;
	      rbr_vals[runKey].w1=w1;
	      rbr_vals[runKey].w2=w2;
	      rbr_vals[runKey].w3=w3;
	      if(verbosity){
		 cout << "rbr_vals ";
		 cout << e2 << " "<< e3 << " " << w0 << " ";
		 cout << w1 << " "<< w2 << " " << w3 << endl;
	      }
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

void Run5PPEmctofRecalReco::updateRunbyRun(const int beginrun, const int endrun)
{
  // UPDATE the database with new runbyrun values

  if (verbosity)
    {
      cout << "Run5PPEmctofRecalReco::updateRunbyRun of runnumbers ";
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
  else Tstop.setToFarFuture();

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startUpdate())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  PdbBankID bankID(1);
  string nameDB = "calibtestemctofrun";
  string descrip = "Parameters submitted by recal object";

  //  Grap a pointer to the bank...
  PdbCalBank *deltaBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 7 * NumRuns + 3; // array has 7 parameters for each run 
  //(run, 6 shifts) + 3hdr (scheme, entries, number of runs)
  if (verbosity)  cout << "NumRuns: " << NumRuns << endl;
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

  rbr_map::iterator MapIter;
  for (MapIter = rbr_vals.begin(); MapIter != rbr_vals.end(); ++MapIter)
    {
      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->first);
      parameter->setName("Run");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second.e2);
      parameter->setName("E2 Shift");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second.e3);
      parameter->setName("E3 Shift");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second.w0);
      parameter->setName("W0 Shift");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second.w1);
      parameter->setName("W1 Shift");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second.w2);
      parameter->setName("W2 Shift");

      parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      parameter->setParameter(MapIter->second.w3);
      parameter->setName("W3 Shift");
    }

  application->commit();
  return ;
}

void Run5PPEmctofRecalReco::fetchT0Shift(const int run)
{
  // GET t0 shift -- removed momentum dependence

  if (verbosity)
    cout << "Run5PPEmctofRecalReco::fetchT0Shift of runnumber: " << run << endl;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbBankID bankID(1);
  string nameDB = "calibemctofmom";

  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);

  if (deltaBank)
    {
      int index = 0;
       PdbParameter *parameter = (PdbParameter *) & deltaBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong scheme DB read for t0 shift" << endl;
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
          cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong length DB read for t0 shift: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }
      else if (verbosity)
        cout << "Good, fetchT0Shift length " << length << " = truelength " << truelength << endl;

      // needed length,to check the number of entries
      if (entries != length - 3)       //2 b/c header
        {
          cout << PHWHERE << "Run5PPEmctofRecalReco:: FATAL...wrong entries DB read for t0 shift" << endl;
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

void Run5PPEmctofRecalReco::updateT0Shift(const int beginrun, const int endrun)
{
  // update a new t0 shift in database

  if (verbosity)
    {
      cout << "Run5PPEmctofRecalReco::updateT0Shift of runnumbers ";
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
      PHMessage("Run5PPEmctofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  string nameDB;
  nameDB = "calibemctofmom";
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

void Run5PPEmctofRecalReco::readslewing(const char *filename){


   //function to read in tower by tower slewing from file
   ifstream ft0;
   ft0.open(filename);
   int tmp1, tmp2, tmp3, tmp4;
   float tmp5, tmp6, tmp7, tmp8, tmp9;
   while(1){
      ft0>>tmp1>>tmp2>>tmp3>>tmp4>>tmp5>>tmp6>>tmp7>>tmp8>>tmp9;
      if(!ft0.good())break;
      SlewParameters[tmp1][tmp2][tmp3][tmp4][0]=tmp5;
      SlewParameters[tmp1][tmp2][tmp3][tmp4][1]=tmp6;
      SlewParameters[tmp1][tmp2][tmp3][tmp4][2]=tmp7;
      SlewParameters[tmp1][tmp2][tmp3][tmp4][3]=tmp8;
      SlewParameters[tmp1][tmp2][tmp3][tmp4][4]=tmp9;
      if(verbosity && tmp4){
	 cout << tmp1 << " " << tmp2 << " " << tmp3 << " " << tmp4;
	 cout << SlewParameters[tmp1][tmp2][tmp3][tmp4-1][0] << " ";
	 cout << SlewParameters[tmp1][tmp2][tmp3][tmp4-1][1] << " ";
	 cout << SlewParameters[tmp1][tmp2][tmp3][tmp4-1][2] << " ";
	 cout << SlewParameters[tmp1][tmp2][tmp3][tmp4-1][3] << " ";
	 cout << SlewParameters[tmp1][tmp2][tmp3][tmp4-1][4] << " ";
	 cout << endl;
      }
   }
   cout << "done reading slewing" << endl;
}

void Run5PPEmctofRecalReco::readgain(const char *filename){

//      function to read in tower by tower gain
  
   ifstream ft0;
   ft0.open(filename);
   int tmp1, tmp2, tmp3, tmp4;
   float tmp5;
   while(1){
      ft0 >> tmp1 >> tmp2 >> tmp3 >> tmp4 >> tmp5;
      if(!ft0.good())break;
      Gain[tmp1][tmp2][tmp3][tmp4]=tmp5;
      if(verbosity){
	 cout << Gain[tmp1][tmp2][tmp3][tmp4] << endl;
      }
   }
   cout << "done reading gain" << endl;

}
void Run5PPEmctofRecalReco::readRunbyRun(const char *filename){

//      function to read in run-by-run
   ifstream ft0;
   ft0.open(filename);
   int trun;
   int nruns=0;
   float tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
   //expects run# E2, E3, W0, W1, W2, & W3
   while(1){
      ft0 >> trun >> tmp2 >> tmp3 >> tmp4 >> tmp5 >> tmp6 >> tmp7;
      if(!ft0.good())break;
      rbr_vals[trun].e2=tmp2;
      rbr_vals[trun].e3=tmp3;
      rbr_vals[trun].w0=tmp4;
      rbr_vals[trun].w1=tmp5;
      rbr_vals[trun].w2=tmp6;
      rbr_vals[trun].w3=tmp7;
      if(verbosity){
	 cout << trun << " " << tmp2 << " " << tmp7 << endl;
      }
      nruns++;
   }

   cout << "done reading run by run values for " << nruns << " runs" << endl;
   cout << rbr_vals.size() << " runs" << endl;
   NumRuns=rbr_vals.size();
}

void Run5PPEmctofRecalReco::readDeltaT(const char *filename){

//      function to read in tower by tower DeltaT offset

   ifstream ft0;
   ft0.open(filename);
   int tmp1, tmp2, tmp3, tmp4;
   float tmp5;
   while(1){
      ft0 >> tmp1 >> tmp2 >> tmp3 >> tmp4 >> tmp5;
      if(!ft0.good())break;
      DeltaT[tmp1][tmp2][tmp3][tmp4]=tmp5;
      if(verbosity)cout << DeltaT[tmp1][tmp2][tmp3][tmp4] << endl;
   }

   cout << "done reading tower by tower" << endl;
}
                                                         
void Run5PPEmctofRecalReco::readwarnmap(const char *filename){
   //read in the warnmap
   ifstream fg;
   fg.open(filename);
   int tmp1, tmp2, tmp3, tmp4;
   int good=0, bad=0;
   float tmp5;
   while(1){
      fg >> tmp1 >> tmp2 >> tmp3 >> tmp4 >> tmp5;
      if(!fg.good())break;
      warnmap[tmp1][tmp2][tmp3][tmp4]=tmp5;
      if(verbosity)cout << warnmap[tmp1][tmp2][tmp3][tmp4] << endl;
      if(tmp5==0)good++;
      else if(tmp5<600||tmp5==1000)bad++;
      else good++;
      
   }
   cout << "done reading warnmap" << endl;
   cout << "good " << good << " bad " << bad << endl;
}
      
