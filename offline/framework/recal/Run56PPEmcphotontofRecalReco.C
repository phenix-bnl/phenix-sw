#include "Run56PPEmcphotontofRecalReco.h"
#include "MasterRecalibrator.h"

#include <PHGlobal.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>

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
#include <emcDBMS.h>
#include <emcDataStorageMap.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>

#include <gsl/gsl_const.h>

#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

Run56PPEmcphotontofRecalReco::Run56PPEmcphotontofRecalReco(const int slew, const string &name):
  Recalibrator(name),
  Switch_edep(0),
  A_PbSc(0.),
  B_PbSc(0.),
  C_PbSc(0),
  A_PbGl(0),
  B_PbGl(0),
  C_PbGl(0),
  //flag defaults
  FlagTimeFromRaw(true),
  FlagDeltaT(true),
  FlagRunbyRun(true),
  FlagDB(1), //go to the correct db -- before =0, the stable db -- don't change this one
  SlewScheme(slew),// should set to 3 for tower by tower slewing
  firstInit(true),
  runnumber(0),
  recalemc(0)
{
  memset(SecOffset,0,sizeof(SecOffset));
  memset(TwrOffset,0,sizeof(TwrOffset));
  memset(WalkPar,0,sizeof(WalkPar));
  memset(Pwalk,0,sizeof(Pwalk));
  memset(Ptzero,0,sizeof(Ptzero));
  memset(Plc,0,sizeof(Plc));
  memset(PGain,0,sizeof(PGain));
  memset(PNormt,0,sizeof(PNormt));
  memset(PScale,0,sizeof(PScale));
  baseclasses.insert("emcClusterContainer");

  return ;
}

int 
Run56PPEmcphotontofRecalReco::isValidRun(const int runno) const
{
  // Run5 p+p 200GeV
  if (runno >= 168314 && runno <= 179846)
    {
      return 1;
    }
  // Run6 p+p 200GeV  189579~204612 
  if (runno >= 189579 && runno <= 204612)
    {
      return 1;
    }
  
  return 0;
}

int 
Run56PPEmcphotontofRecalReco::InitRun(PHCompositeNode *topNode)
{
  // run at each new runnumber

  if (verbosity)    cout << "Run56PPEmcphotontofRecalReco InitRun" << endl;
  recoConsts *rc = recoConsts::instance();
  int runnumber =  rc->get_IntFlag("RUNNUMBER");
  if (verbosity)    cout << "Fetching for run number: " << runnumber << endl;
  //for Run5 pp 200GeV residual correction parameter
  if (runnumber >= 168314 && runnumber <= 179846)
    {
      Switch_edep=1; // linear function

      A_PbSc=-0.076;
      B_PbSc=-0.23;

      A_PbGl=0.20;
      B_PbGl=-0.13;
    }

  //for Run6 pp 200GeV residual energy dependent correction parameter
  if (runnumber >= 189579 && runnumber <= 204612)
    {
      Switch_edep=2; // 1/x function
      A_PbSc=-1.30;
      B_PbSc=7.20;
      C_PbSc=4.12;

      A_PbGl=4.70;
      B_PbGl=-51.7;
      C_PbGl=8.66;
    }

  if(verbosity){
    cout << "energy dependent correction switch= " << Switch_edep << endl;
    cout << "A_PbSc= " << A_PbSc << "  B_PbSc= " << B_PbSc << "  C_PbSc= " << C_PbSc << endl;
    cout << "A_PbGl= " << A_PbGl << "  B_PbGl= " << B_PbGl << "  C_PbGl= " << C_PbGl <<endl;
  }
  

  if (firstInit)
    { //only need to fetch these once
      //cout << "FirstInitRun: " << firstInit << endl;

      //fetch calibration values (They are constant through the entire run.)
      fetchgtoftparprod(runnumber);
    }

  // get run by run shift
    {
      fetchgtofeparprod(runnumber);   // run by run energy calibration in the production

      fetchgtofsecoffset(runnumber);
      fetchgtoftwroffset(runnumber);
      fetchgtoftwrwalk(runnumber);

    }

  if (firstInit && verbosity) cout << PHWHERE << "done with FirstInitRun" << endl;

  firstInit = false; 

  // this is all master recalibrator stuff 
  if (mybaseclass == "emcClusterContainer")
    {
      recalemc = 1;
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

int
Run56PPEmcphotontofRecalReco::process_event(PHCompositeNode *topNode)
{
  float vtxz;
  float vtx[3];

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

  if (!d_gbl) 
    {
      if (verbosity > 0) cout << PHWHERE << "No PHGlobal use vtx=0 for tflash" << endl;
      vtxz = 0.;
    }
  else
    {
      // get t0 from bbc -- will need this for TimefromRaw
      vtxz = d_gbl->getBbcZVertex();
      if(vtxz<-1000 || vtxz>1000)
        {
          if (verbosity > 0) cout << PHWHERE << "No Bbcz use vtx=0 for tflash" << endl;
	  vtxz = 0.;
        }
    }

  vtx[0]=0;
  vtx[1]=0;
  vtx[2]=vtxz;

  //  Get pointers to necessary nodes...

  // PWG stuff
  if (recalemc)
    {
      emcClusterContainer *d_pwg = findNode::getClass<emcClusterContainer>(topNode, inputnodename.c_str());
      for (size_t i = 0 ; i < d_pwg->size(); i++)
        {
          emcClusterContent *emccl = d_pwg->getCluster(i);

          int arm=emccl->arm();
	  int sector=emccl->sector();
	  int ypos=emccl->iypos();
	  int zpos=emccl->izpos();
          float ecent=emccl->ecent();
          float rawtdc=emccl->rawtdc();

          int adc=GetAdc(ecent,arm,sector,ypos,zpos);
          int adc1GeV=GetAdc(1.,arm,sector,ypos,zpos); // adc for 1GeV 
          float tof2=Tdc2Tof(rawtdc,adc,arm,sector,ypos,zpos,adc1GeV);
          float tflash=Gettflash(emccl,vtx);

         // correction for residual component
	 // One reason is offset was done in 1-2GeV cluster with the old walk correction.
	  float dt=0.;
	  if(Switch_edep==1){
            if(arm==0||sector>1) // PbSc
  	      {
	        dt=A_PbSc*emccl->e()+B_PbSc;
              }                  // PbGl
            else
	      {
	        dt=A_PbGl*emccl->e()+B_PbGl;
              }
          }else if(Switch_edep==2){
            if(arm==0||sector>1) // PbSc
  	      {
	        dt=A_PbSc+B_PbSc/(emccl->e()+C_PbSc);
              }                  // PbGl
            else
	      {
	        dt=A_PbGl+B_PbGl/(emccl->e()+C_PbGl);
              }
          }

	  emccl->set_tofcorr(tof2-dt-tflash);
        }
    }
	    


  if (verbosity) cout << "done with process_event" << endl;
  return 0;
}

float Run56PPEmcphotontofRecalReco::Gettflash(const emcClusterContent *gt, const float vtx[]) const
{
  float tflash;

  float vx = vtx[0]-gt->x();
  float vy = vtx[1]-gt->y();
  float vz = vtx[2]-gt->z();
  
  float lactual = sqrt( vx*vx + vy*vy + vz*vz );
  float lnominal = sqrt(gt->x() * gt->x() + gt->y() * gt->y() + gt->z() * gt->z());
  float dd = lactual - lnominal;

  tflash = dd / 30.0;

  return tflash;
}

int Run56PPEmcphotontofRecalReco::GetAdc(const float ecent,const int arm,const int sector,const int ypos,const int zpos) const
{
 float normt,gain;
 int adc;

 if(arm==0||sector>1){ // PbSc
   normt = PNormt[arm][sector][ypos][zpos];
   gain  = PGain[arm][sector][ypos][zpos];

   float fadc=(ecent*normt/gain);

   float scale = PScale[arm][sector][ypos][zpos];
   if ( scale < 12.0 || scale > 18.0 ){
     scale = 15.4; 
   }

   adc=(int)(fadc/scale+0.5);                // assuming LG.  no pedestal info
   
 }else{                // PbGl
    normt = PNormt[arm][sector][ypos][zpos];
    gain  = PGain[arm][sector][ypos][zpos];

    adc=(int)(ecent*normt/gain+0.5);
    if(adc>4095*22) adc=4095*22;  // shouldn't be higher than it
 }

/*
 if(verbosity){
   cout << "Run56Emctof GetAdc: " 
        << arm << " " 
        << sector << " " 
        << ypos << " " 
        << zpos << " " 
        << normt << " " 
        << gain << " " 
        << scale << " " 
	<< ecent << " " 
	<< adc << endl;
 }
*/

 return adc;

}

float Run56PPEmcphotontofRecalReco::Tdc2Tof(const float rawtdc,const int adc,const int arm,const int sector,const int ypos,const int zpos,const int adcbase) const
{
 float tofcorr;

 float t0=Ptzero[arm][sector][ypos][zpos];
 float lc=Plc[arm][sector][ypos][zpos];
 float wk =Pwalk[arm][sector][ypos][zpos]; 

 if(arm==0||sector>1){ // PbSc
   lc = ((lc>25.&&lc<65.)? lc : 40.0)/1000.;
   float walkbase=(adcbase>0.&&wk<0.)? wk*4000./(float)adcbase : 0.;

   float wknew=WalkPar[arm][sector][ypos][zpos]; 
   float walkcube=-1000./cbrt((float)adc)*wknew;      // wknew<0
   float walkcube0=-1000./cbrt((float)adcbase)*wknew;

   tofcorr= - lc*(rawtdc-walkbase+walkcube-walkcube0) - t0;  // cubic root
          


 }else{          // PbGl
   lc = ((lc>20. && lc<55.)? lc : 38.9)/1000.;

   float dt = wk*Log(adc);
   tofcorr=- (rawtdc-dt)*lc - t0;
 }

 tofcorr=tofcorr-SecOffset[arm][sector]-TwrOffset[arm][sector][ypos][zpos];

/*
 if(verbosity && adc>100){
     cout << "Run56EMCalTof : " << arm << " " << sector << " " << ypos << " " << zpos << " " 
          << t0 << " " << lc << " " << wk << " " << wknew << " " << SecOffset[arm][sector] << " " << TwrOffset[arm][sector][ypos][zpos] << endl; 
 }
*/

 return tofcorr;
}


float Run56PPEmcphotontofRecalReco::Log(const int adc) const
{
  assert(adc>=0 && adc<=4095*22);
  if (adc == 0)
    {
      return 0.;
    }
  return(log(adc));
}

void 
Run56PPEmcphotontofRecalReco::fetchgtoftparprod(const int run)
{
  //GETS values from database

  // fetch values used in the production

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
  //      |   walk
  //      |   tzero 
  //      \   lc
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          
  //

  if (verbosity)
    cout << "Run56PPEmcphotontofRecalReco::fetchgtoftparprod" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtoftparprod"; // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *gtoftparBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (gtoftparBank)
    {

      if (verbosity)
	{
	  gtoftparBank->print();
	  gtoftparBank->printEntry(1);
	}
      int index = 0;

      //----------------------------------------------------
      //  three checks...length of record, scheme and no. entries...
      int length = 7 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2;
      int truelength = gtoftparBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong length DB read for gtoftpar: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong scheme DB read for gtoftpar" << endl;
          return ;
        }

      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong entries DB read for gtoftpar" << endl;
          return ;
        }


      //----------------------------------------------------
      if (verbosity)
        {
          cout << "READ from Database: Slat gtoftpar" << endl;
        }
      for (int i = 0; i < NEMC_ARM; i++)
        {
          for (int j = 0; j < NEMC_SECTOR; j++)
            {
              for (int k = 0; k < NEMC_Y; k++)
                {
                  for (int l = 0; l < NEMC_Z; l++)
                    {

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      int arm = (int)parameter->getParameter(); //read arm

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      int sect = (int)parameter->getParameter(); // read sector

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      int ysect = (int)parameter->getParameter(); // read y value

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      int zsect = (int)parameter->getParameter(); // read z value

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      Pwalk[arm][sect][ysect][zsect] = parameter->getParameter();    

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      Ptzero[arm][sect][ysect][zsect] = parameter->getParameter();  

                      parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                      Plc[arm][sect][ysect][zsect] = parameter->getParameter();

		      // read delta t shift for the tower with arm,sector,y,z and put in array

/*
                        if (verbosity)
                          {
                            cout << arm    << " ";
                            cout << sect << " ";
                            cout << ysect  << " ";
                            cout << zsect  << " ";
                            cout << Pwalk[arm][sect][ysect][zsect]  << " ";
                            cout << endl;
                          }
*/
                    }
                }
            }
        }
      delete gtoftparBank;
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

void Run56PPEmcphotontofRecalReco::updategtoftparprod(const int beginrun, const int endrun)
{
  //STORES values in database
  // store values used in the production

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
  //      |   walk
  //      |   tzero 
  //      \   lc
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //  
  //                          
  //
  if (verbosity)
    {
      cout << PHWHERE << "Run56PPEmcphotontofRecalReco::updategtoftparprod of runnumbers ";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtoftparprod";  // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
//  string descrip = "Parameters submitted by recal object";
  string descrip = "wk,t0,lc for the production";

  //  Grap a pointer to the bank...
  PdbCalBank *gtoftparBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 7 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2; // array + 2 hdr
  gtoftparBank->setLength(length); // set length

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme"); // set scheme

  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries"); // set entries

  // loop over DeltaT array and put arm,sector,y,z and deltaT values in database
  for (int i = 0; i < NEMC_ARM; i++)
    {
      for (int j = 0; j < NEMC_SECTOR; j++)
        {
          for (int k = 0; k < NEMC_Y; k++)
            {
              for (int l = 0; l < NEMC_Z; l++)
                {

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("Ysect");

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("Zsect");

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(Pwalk[i][j][k][l]);
                  parameter->setName("wk");

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(Ptzero[i][j][k][l]);
                  parameter->setName("tzero");

                  parameter = (PdbParameter *) & gtoftparBank->getEntry(index++);
                  parameter->setParameter(Plc[i][j][k][l]);
                  parameter->setName("lc");
                }
            }
        }
    }

  cout << "commit gtoftparprod" << endl;
  application->commit();

  return ;
}

void Run56PPEmcphotontofRecalReco::fetchgtofeparprod(const int run)
{
  //GETS values from database

  // fetch values used in the production

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
  //      |   gain
  //      |   normt
  //      \   scale
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          
  //

  if (verbosity)
    cout << "Run56PPEmcphotontofRecalReco::fetchgtofeparprod" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtofeparprod"; // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *gtofeparBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (gtofeparBank)
    {

      if (verbosity)
	{
	  gtofeparBank->print();
	  gtofeparBank->printEntry(1);
	}
      int index = 0;

      //----------------------------------------------------
      //  three checks...length of record, scheme and no. entries...
      int length = 7 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2;
      int truelength = gtofeparBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong length DB read for gtofepar: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong scheme DB read for gtofepar" << endl;
          return ;
        }

      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong entries DB read for gtofepar" << endl;
          return ;
        }


      //----------------------------------------------------
      if (verbosity)
        {
          cout << "READ from Database: Slat gtofepar" << endl;
        }
      for (int i = 0; i < NEMC_ARM; i++)
        {
          for (int j = 0; j < NEMC_SECTOR; j++)
            {
              for (int k = 0; k < NEMC_Y; k++)
                {
                  for (int l = 0; l < NEMC_Z; l++)
                    {

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      int arm = (int)parameter->getParameter(); //read arm

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      int sect = (int)parameter->getParameter(); // read sector

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      int ysect = (int)parameter->getParameter(); // read y value

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      int zsect = (int)parameter->getParameter(); // read z value

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      PGain[arm][sect][ysect][zsect] = parameter->getParameter();    

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      PNormt[arm][sect][ysect][zsect] = parameter->getParameter();  

                      parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                      PScale[arm][sect][ysect][zsect] = parameter->getParameter();

		      // read delta t shift for the tower with arm,sector,y,z and put in array

/*
                        if (verbosity)
                          {
                            cout << arm    << " ";
                            cout << sect << " ";
                            cout << ysect  << " ";
                            cout << zsect  << " ";
                            cout << PGain[arm][sect][ysect][zsect]  << " ";
                            cout << endl;
                          }
*/
                    }
                }
            }
        }
      delete gtofeparBank;
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

void Run56PPEmcphotontofRecalReco::updategtofeparprod(const int beginrun, const int endrun)
{
  //STORES values in database
  // store values used in the production

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
  //      |   walk
  //      |   tzero 
  //      \   lc
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //  
  //                          
  //
  if (verbosity)
    {
      cout << PHWHERE << "Run56PPEmcphotontofRecalReco::updategtofeparprod of runnumbers ";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtofeparprod";  // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
//  string descrip = "Parameters submitted by recal object";
  string descrip = "gain,normt,scale for the production";

  //  Grap a pointer to the bank...
  PdbCalBank *gtofeparBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 7 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2; // array + 2 hdr
  gtofeparBank->setLength(length); // set length

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme"); // set scheme

  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries"); // set entries

  // loop over DeltaT array and put arm,sector,y,z and deltaT values in database
  for (int i = 0; i < NEMC_ARM; i++)
    {
      for (int j = 0; j < NEMC_SECTOR; j++)
        {
          for (int k = 0; k < NEMC_Y; k++)
            {
              for (int l = 0; l < NEMC_Z; l++)
                {

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("Ysect");

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("Zsect");

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(PGain[i][j][k][l]);
                  parameter->setName("gain");

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(PNormt[i][j][k][l]);
                  parameter->setName("normt");

                  parameter = (PdbParameter *) & gtofeparBank->getEntry(index++);
                  parameter->setParameter(PScale[i][j][k][l]);
                  parameter->setName("scale");
                }
            }
        }
    }

  cout << "commit gtofeparprod" << endl;
  application->commit();

  return ;
}

void Run56PPEmcphotontofRecalReco::fetchgtofsecoffset(const int run)
{
  //GETS values from database

  // fetch values for sector tof offset

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
  //  d   |  
  //  y   |  
  //      \   secoffset
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          
  //

  if (verbosity)
    cout << "Run56PPEmcphotontofRecalReco::fetchgtofsecoffset" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtofsecoffset"; // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *gtofsecoffsetBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (gtofsecoffsetBank)
    {

      if (verbosity)
	{
	  gtofsecoffsetBank->print();
	  gtofsecoffsetBank->printEntry(1);
	}
      int index = 0;

      //----------------------------------------------------
      //  three checks...length of record, scheme and no. entries...
      int length = 3 * NEMC_ARM * NEMC_SECTOR + 2;
      int truelength = gtofsecoffsetBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong length DB read for gtofsecoffset: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong scheme DB read for gtofsecoffset" << endl;
          return ;
        }

      parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong entries DB read for gtofsecoffset" << endl;
          return ;
        }


      //----------------------------------------------------
      if (verbosity)
        {
          cout << "READ from Database: Slat gtofsecoffset" << endl;
        }
      for (int i = 0; i < NEMC_ARM; i++)
        {
          for (int j = 0; j < NEMC_SECTOR; j++)
            {
               parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
               int arm = (int)parameter->getParameter(); //read arm

               parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
               int sect = (int)parameter->getParameter(); // read sector

               parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
               SecOffset[arm][sect] = parameter->getParameter();    
/*
             if (verbosity)
               {
	          cout << "fetchgtofsecoffset: " << arm << " " 
	               << sect << " " 
		       << SecOffset[arm][sect] << endl;
	       }
*/
	    }
        }
      delete gtofsecoffsetBank;
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

void Run56PPEmcphotontofRecalReco::updategtofsecoffset(const int beginrun, const int endrun)
{
  //STORES values in database
  // store values for sector tof offset

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
  //  d   |  
  //  y   |  
  //      \   secoffset
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //  
  //                          
  //
  if (verbosity)
    {
      cout << PHWHERE << "Run56PPEmcphotontofRecalReco::updategtofsecoffsetprod of runnumbers ";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtofsecoffset";  // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
//  string descrip = "Parameters submitted by recal object";
  string descrip = "sector offset";

  //  Grap a pointer to the bank...
  PdbCalBank *gtofsecoffsetBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 3 * NEMC_ARM * NEMC_SECTOR + 2; // array + 2 hdr
  gtofsecoffsetBank->setLength(length); // set length

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme"); // set scheme

  parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries"); // set entries

  // loop over DeltaT array and put arm,sector,y,z and deltaT values in database
  for (int i = 0; i < NEMC_ARM; i++)
    {
      for (int j = 0; j < NEMC_SECTOR; j++)
        {
           parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
           parameter->setParameter(i);
           parameter->setName("Arm");

           parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
           parameter->setParameter(j);
           parameter->setName("Sector");

           parameter = (PdbParameter *) & gtofsecoffsetBank->getEntry(index++);
           parameter->setParameter(SecOffset[i][j]);
           parameter->setName("secoffset");
        }
    }

  cout << "commit gtofsecoffsetprod" << endl;
  application->commit();

  return ;
}

void Run56PPEmcphotontofRecalReco::fetchgtoftwroffset(const int run)
{
  //GETS values from database

  // fetch values for tower tof offset

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
  //      \   twroffset
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          
  //

  if (verbosity)
    cout << "Run56PPEmcphotontofRecalReco::fetchgtoftwroffset" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtoftwroffset"; // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *gtoftwroffsetBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (gtoftwroffsetBank)
    {

      if (verbosity)
	{
	  gtoftwroffsetBank->print();
	  gtoftwroffsetBank->printEntry(1);
	}
      int index = 0;

      //----------------------------------------------------
      //  three checks...length of record, scheme and no. entries...
      int length = 5 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2;
      int truelength = gtoftwroffsetBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong length DB read for gtoftwroffset: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong scheme DB read for gtoftwroffset" << endl;
          return ;
        }

      parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong entries DB read for gtoftwroffset" << endl;
          return ;
        }


      //----------------------------------------------------
      if (verbosity)
        {
          cout << "READ from Database: Slat gtoftwroffset" << endl;
        }
      for (int i = 0; i < NEMC_ARM; i++)
        {
          for (int j = 0; j < NEMC_SECTOR; j++)
            {
              for (int k = 0; k < NEMC_Y; k++)
                {
                  for (int l = 0; l < NEMC_Z; l++)
                    {

                      parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                      int arm = (int)parameter->getParameter(); //read arm

                      parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                      int sect = (int)parameter->getParameter(); // read sector

                      parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                      int ysect = (int)parameter->getParameter(); // read y value

                      parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                      int zsect = (int)parameter->getParameter(); // read z value

                      parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                      TwrOffset[arm][sect][ysect][zsect] = parameter->getParameter();    


		      // read delta t shift for the tower with arm,sector,y,z and put in array

/*
                       	  if (verbosity)
                       	    {
                       	      cout << arm    << " ";
                       	      cout << sect << " ";
                       	      cout << ysect  << " ";
                       	      cout << zsect  << " ";
                       	      cout << TwrOffset[arm][sect][ysect][zsect]  << " ";
                       	      cout << endl;
                       	    }
*/
                    }
                }
            }
        }
      delete gtoftwroffsetBank;
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

void Run56PPEmcphotontofRecalReco::updategtoftwroffset(const int beginrun, const int endrun)
{
  //STORES values in database
  // store values for tower tof offset

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
  //      \   twroffset
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //  
  //                          
  //
  if (verbosity)
    {
      cout << PHWHERE << "Run56PPEmcphotontofRecalReco::updategtoftwroffsetprod of runnumbers ";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtoftwroffset";  // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
//  string descrip = "Parameters submitted by recal object";
  string descrip = "tower offset";

  //  Grap a pointer to the bank...
  PdbCalBank *gtoftwroffsetBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 5 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2; // array + 2 hdr
  gtoftwroffsetBank->setLength(length); // set length

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme"); // set scheme

  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries"); // set entries

  // loop over DeltaT array and put arm,sector,y,z and deltaT values in database
  for (int i = 0; i < NEMC_ARM; i++)
    {
      for (int j = 0; j < NEMC_SECTOR; j++)
        {
          for (int k = 0; k < NEMC_Y; k++)
            {
              for (int l = 0; l < NEMC_Z; l++)
                {

                  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("Ysect");

                  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("Zsect");

                  parameter = (PdbParameter *) & gtoftwroffsetBank->getEntry(index++);
                  parameter->setParameter(TwrOffset[i][j][k][l]);
                  parameter->setName("twroffset");

                }
            }
        }
    }

  cout << "commit gtoftwroffsetprod" << endl;
  application->commit();

  return ;
}

void Run56PPEmcphotontofRecalReco::fetchgtoftwrwalk(const int run)
{
  //GETS values from database

  // fetch values for tower walk

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
  //      \   twrwalk
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //                          
  //

  if (verbosity)
    cout << "Run56PPEmcphotontofRecalReco::fetchgtoftwrwalk" << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();

  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtoftwrwalk"; // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

  PdbCalBank *gtoftwrwalkBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB.c_str(), run);
  if (gtoftwrwalkBank)
    {

      if (verbosity)
	{
	  gtoftwrwalkBank->print();
	  gtoftwrwalkBank->printEntry(1);
	}
      int index = 0;

      //----------------------------------------------------
      //  three checks...length of record, scheme and no. entries...
      int length = 5 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2;
      int truelength = gtoftwrwalkBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong length DB read for gtoftwrwalk: " << truelength << endl;
          cout << "                  expected length:                          " << length << endl;
          return ;
        }

      PdbParameter *parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong scheme DB read for gtoftwrwalk" << endl;
          return ;
        }

      parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << PHWHERE << "Run56PPEmcphotontofRecalReco:: FATAL...wrong entries DB read for gtoftwrwalk" << endl;
          return ;
        }


      //----------------------------------------------------
      if (verbosity)
        {
          cout << "READ from Database: Slat gtoftwrwalk" << endl;
        }
      for (int i = 0; i < NEMC_ARM; i++)
        {
          for (int j = 0; j < NEMC_SECTOR; j++)
            {
              for (int k = 0; k < NEMC_Y; k++)
                {
                  for (int l = 0; l < NEMC_Z; l++)
                    {

                      parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                      int arm = (int)parameter->getParameter(); //read arm

                      parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                      int sect = (int)parameter->getParameter(); // read sector

                      parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                      int ysect = (int)parameter->getParameter(); // read y value

                      parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                      int zsect = (int)parameter->getParameter(); // read z value

                      parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                      WalkPar[arm][sect][ysect][zsect] = parameter->getParameter();    

/*
                       	  if (verbosity)
                       	    {
                       	      cout << arm    << " ";
                       	      cout << sect << " ";
                       	      cout << ysect  << " ";
                       	      cout << zsect  << " ";
                       	      cout << WalkPar[arm][sect][ysect][zsect]  << " ";
                       	      cout << endl;
                       	    }
*/
                    }
                }
            }
        }
      delete gtoftwrwalkBank;
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

void Run56PPEmcphotontofRecalReco::updategtoftwrwalk(const int beginrun, const int endrun)
{
  //STORES values in database
  // store values for tower walk

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
  //      \   twrwalk
  //
  //  The number of times the body repeats will be NEMC_ARM*NEMC_SECTOR*NEMC_Y*NEMC_Z
  //  in the first scheme known as scheme 1.
  //
  //  
  //                          
  //
  if (verbosity)
    {
      cout << PHWHERE << "Run56PPEmcphotontofRecalReco::updategtoftwrwalkprod of runnumbers ";
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
      PHMessage("Run56PPEmcphotontofRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
      return ;
    }

  //  Make a bank ID...
  PdbBankID bankID(1);
  // pick database --don't change
  string nameDB;
  if (FlagDB == 0)
;//    nameDB = "calib.emctofdt";
  else if (FlagDB == 1)
    nameDB = "emcgtoftwrwalk";  // KO
  else if (FlagDB == 2)
;//    nameDB = "calibemctoflcdt";
  else
    {
      cout << PHWHERE << "unknown db flag" << endl;
      return ;
    }
//  string descrip = "Parameters submitted by recal object";
  string descrip = "walk parameter cubic root";

  //  Grap a pointer to the bank...
  PdbCalBank *gtoftwrwalkBank = bankManager->createBank("PdbParameterBank", bankID, descrip.c_str(), Tstart, Tstop, nameDB.c_str());

  int length = 5 * NEMC_ARM * NEMC_SECTOR * NEMC_Y * NEMC_Z + 2; // array + 2 hdr
  gtoftwrwalkBank->setLength(length); // set length

  PdbParameter *parameter;
  int index = 0;
  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
  parameter->setParameter(1.0);
  parameter->setName("scheme"); // set scheme

  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
  parameter->setParameter(length - 2);
  parameter->setName("entries"); // set entries

  // loop over DeltaT array and put arm,sector,y,z and deltaT values in database
  for (int i = 0; i < NEMC_ARM; i++)
    {
      for (int j = 0; j < NEMC_SECTOR; j++)
        {
          for (int k = 0; k < NEMC_Y; k++)
            {
              for (int l = 0; l < NEMC_Z; l++)
                {

                  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                  parameter->setParameter(i);
                  parameter->setName("Arm");

                  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                  parameter->setParameter(j);
                  parameter->setName("Sector");

                  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                  parameter->setParameter(k);
                  parameter->setName("Ysect");

                  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                  parameter->setParameter(l);
                  parameter->setName("Zsect");

                  parameter = (PdbParameter *) & gtoftwrwalkBank->getEntry(index++);
                  parameter->setParameter(WalkPar[i][j][k][l]);
                  parameter->setName("twrwalk");

                }
            }
        }
    }

  cout << "commit gtoftwrwalk" << endl;
  application->commit();

  return ;
}

//  KO read from file
void Run56PPEmcphotontofRecalReco::readsecoffset(const char *filename){

   ifstream ft0;
   ft0.open(filename);
   int tmp1, tmp2;
   float tmp3;
   while(1){
      ft0 >> tmp1 >> tmp2 >> tmp3;
      if(!ft0.good())break;
      SecOffset[tmp1][tmp2]=tmp3;
      if(verbosity)cout << SecOffset[tmp1][tmp2] << endl;
   }

   ft0.close();

   cout << "done reading tower by tower" << endl;

}

void Run56PPEmcphotontofRecalReco::readtwroffset(const char *filename){

  int arm,sec;

   ifstream ft0;
   ft0.open(filename);
   int tmp1, tmp2, tmp3;
   float tmp5;
   while(1){
      ft0 >> tmp1 >> tmp2 >> tmp3 >> tmp5;
      if(!ft0.good())break;
      if(tmp1<4){
        arm=0;
	sec=tmp1;
      }else{
        arm=1;
	sec=7-tmp1;
      }
      int iypos=tmp2;
      int izpos=tmp3;
        
      TwrOffset[arm][sec][iypos][izpos]=tmp5;
      if(verbosity)cout << arm << " " << sec << " " << iypos << " " << izpos << " " << TwrOffset[arm][sec][iypos][izpos] << endl;
   }

   ft0.close();

   cout << "done reading tower by tower" << endl;

}

void Run56PPEmcphotontofRecalReco::readtwrwalk(const char *filename){


   ifstream ft0;
   ft0.open(filename);
   int tmp1;
   float tmp5;

   int arm,sec,iypos,izpos;


   while(1){
      ft0 >> tmp1 >> tmp5;
      if(!ft0.good())break;
      EmcIndexer::TowerLocation(tmp1,arm,sec,iypos,izpos);
      WalkPar[arm][sec][iypos][izpos]=tmp5;
      if(verbosity)cout << arm << " " << sec << " " << iypos << " " << izpos << " " <<  WalkPar[arm][sec][iypos][izpos] << endl;
   }

   ft0.close();

   cout << "done reading tower by tower walk" << endl;

}

// KO read from the database
void Run56PPEmcphotontofRecalReco::readprodpar(const int runnumber)
{
 emcCalibrationDataHelper *ch;

 int towerid,fem,channel;
 float scale,normt,gain;

 const emcCalFEM *calfem_lc;
 const emcCalFEM *calfem_wt;
 const emcCalFEM *calfem_t0;

 string ksLCTofs="LCTofs";
 string ksWalkTofs="WalkTofs";
 string ksTofT0s="TofT0Bs";

//
 time_t incremental_time;
 RunToTime *run_time=RunToTime::instance();
 PHTimeStamp time_begin(*(run_time->getBeginTime(runnumber)));
 incremental_time=time_begin.getTics();

 emcDataStorageMap ds_map(emcDBMS::get());
 ch= new emcCalibrationDataHelper(runnumber,time_begin,false,
       			            ds_map.storage(),"emcal");

//
 string ksGains;

 const emcCalFEM *calfem;
 const emcCalFEM *calfem_hlratios;

 for(int arm=0;arm<2;arm++)
   {
     for(int sec=0;sec<4;sec++)
       {
          for(int iypos=0;iypos<NEMC_Y;iypos++)
	    {
               for(int izpos=0;izpos<NEMC_Z;izpos++)
	         {
                    towerid=EmcIndexer::TowerID(arm,sec,iypos,izpos);
		    EmcIndexer::PXPXSM144CH(towerid,fem,channel);
//
                    calfem_lc=ch->getCalibration(fem,ksLCTofs.c_str());
                    calfem_wt=ch->getCalibration(fem,ksWalkTofs.c_str());
                    calfem_t0=ch->getCalibration(fem,ksTofT0s.c_str());

                    float t0=calfem_t0->getValueFast(channel,0);
                    float lc=calfem_lc->getValueFast(channel,0);
                    float wk = calfem_wt->getValueFast(channel,1);

//
                    calfem_hlratios = ch->getCalibration(fem,"HLRatios");
                    scale = calfem_hlratios->getValueFast(channel);    

                    if(arm==0||sec>1) // PbSc
		      {
		         ksGains="Gains:BLR:0:x:ZS:AVOFRATIO:164777";
                         calfem = ch->getCalibration(fem,ksGains.c_str());
                         normt = calfem->getValue(channel,incremental_time);
                         gain  = ch->getEnergyCalibration(towerid);
                      }else{
                         ksGains="Gains";
                         calfem = ch->getCalibration(fem,ksGains.c_str());
                         normt = calfem->getValue(channel,incremental_time);
                         gain  = ch->getEnergyCalibration(towerid);
                      }
//
		    if(arm==0||sec>1)  // pbsc
                      {
		        if(izpos>=72||iypos>=36){
			  t0=0;
			  lc=0;
			  wk=0;
			  gain=0;
			  normt=0;
			  scale=0;
                        }
                      }

                    Ptzero[arm][sec][iypos][izpos]=t0; 
                    Plc[arm][sec][iypos][izpos]=lc; 
                    Pwalk[arm][sec][iypos][izpos]=wk; 

                    PGain[arm][sec][iypos][izpos]=gain; 
                    PNormt[arm][sec][iypos][izpos]=normt;
                    PScale[arm][sec][iypos][izpos]=scale;


                 }
             }
        }
    }
  delete ch;
}

void Run56PPEmcphotontofRecalReco::printprodpar(void)
{
 float t0,lc,wk,gain,normt,scale;

 for(int arm=0;arm<2;arm++)
   {
     for(int sec=0;sec<4;sec++)
       {
          for(int iypos=0;iypos<NEMC_Y;iypos++)
	    {
               for(int izpos=0;izpos<NEMC_Z;izpos++)
	         {
                    t0=Ptzero[arm][sec][iypos][izpos]; 
                    lc=Plc[arm][sec][iypos][izpos];
                    wk=Pwalk[arm][sec][iypos][izpos]; 

                    gain=PGain[arm][sec][iypos][izpos];
                    normt=PNormt[arm][sec][iypos][izpos];
                    scale=PScale[arm][sec][iypos][izpos];

		    cout << arm << " " << sec << " " << iypos << " " << izpos << ": " 
		    << t0 << " " << lc << " "  << wk << " " 
		    << gain << " " << normt << " " << scale << endl;
                 }
             }
        }
    }
}

