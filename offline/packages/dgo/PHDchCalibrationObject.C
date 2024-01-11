// Implementation of class file: PHDchCalibrationObject.h
// Created by: Federica Ceretto at Wed Feb 17 15:19:41 1999
// Purpose: Write in/Read from the DB the Calibration Values for the
// Drift Chamber
//
//  TKH--Updated to include stereo calibrations...

#include <fstream>
#include <iostream>

#include "PdbParameter.hh"
#include "PHDchCalibrationObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include <cmath>

using namespace std;

void PHDchCalibrationObject::initialize()
{

  nChannels = 2;
  channelParameters = 2;
  sle_head=3;
  sle_ncha=4;
  sle_npar=9;
  committingFlag = 0;
  headerParameters = 8;
  runNumber = 0;
  bbcMean = 0;
  zdcMean = 0;
  zdcCounts = 0;
  bbcCounts = 0;
  slewingArray.resize(0,numberOfArms,0,numberOfModules+1,0,9-2); // arm, module, par
  slewingArray = 0;
  localBank = 0;
  slewBank = 0;
  stereoBank = 0;
}

//_______________________________________________________________________________
PHBoolean PHDchCalibrationObject::setFileName(const char* cal,
					      const char* slew,
					      const char* loc,
					      const char* stereo)
{
  calFile = cal;
  slewFile = slew;
  calLocalFile = loc;
  stereoFile = stereo;

  PHBoolean allFiles = True;
  if (!calFile)
  {
    cout << "PHDchCalibrationObject::setFileName - No calibration filename given." << endl;
    allFiles = False;
  } else cout << "PHDchCalibrationObject::setFileName - Going to read CALIBRATION from:" << calFile << endl;

  if (!slewFile)
  {
    cout << "PHDchCalibrationObject::setFileName - No slewing correction filename given." << endl;
    allFiles = False;
  } else cout << "PHDchCalibrationObject::setFileName - Going to read SLEW CALIBRATION from:" << slewFile << endl;

  if (!calLocalFile)
  {
    cout << "PHDchCalibrationObject::setFileName - No local wire calibration filename given." << endl;
    allFiles = False;
  } else cout << "PHDchCalibrationObject::setFileName - Going to read LOCAL WIRE CALIBRATION from:"  << calLocalFile << endl;

  if (!stereoFile)
  {
    cout << "PHDchCalibrationObject::setFileName - No stereo wire calibration filename given." << endl;
    allFiles = False;
  } else cout << "PHDchCalibrationObject::setFileName - Going to read STEREO WIRE CALIBRATION from:" << stereoFile << endl;

  return allFiles;
}


PHDchCalibrationObject::PHDchCalibrationObject(PHDchAddressObject *add) 
  : PHCalibrationObject(add)
{
  initialize();
  dchaddress = add;

  // We hard-coded "Monte Carlo" values here.
  // --> By construction, these will always be overwritten 
  // --> with ASCII and/or DATABASE values when processing 
  // --> real data...
  float deltaDis[numberOfArms][numberOfSides][numberOfPlanes]=
  {
    { //ARM 0
      { // SIDE 0
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,              // DC.inner/outer
	-0.1183, -0.0969, -0.0920, -0.0965, -0.0744, -0.0854, -0.0587, -0.0782,
	-0.0887, -0.0770, -0.0963, -0.0686,	-0.0805, -0.0899, -0.0855, -0.1004
      },
      { // SIDE 1
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	-0.1073, -0.0912, -0.0911, -0.0874,	-0.0730, -0.0905, -0.0698, -0.0861,
	-0.1306, -0.0975, -0.0899, -0.0434,	-0.0742, -0.0520, -0.1208, -0.1512
      }
    },
    { //ARM 1
      { // SIDE 0
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,              // DC.inner/outer
	-0.1210, -0.1282, -0.1016, -0.1308, -0.1109, -0.1271, -0.1350, -0.1514,
	-0.1315, -0.1223, -0.1227, -0.1163,	-0.1286, -0.1061, -0.1384, -0.1394
      },
      { //SIDE 1
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	-0.1159, -0.1282, -0.1102, -0.1235,	-0.1026, -0.1200, -0.1368, -0.1510,
	-0.1763, -0.1446, -0.1209, -0.0996,	-0.1025, -0.0857, -0.1997, -0.2523
      }
    }
  };

  binSize      = (1.0/0.038)/32.0;
  kinkLocation = 0.20;
  deltaRad[EAST]  = 0.19;
  deltaRad[WEST]  = 0.25;

  float tMonte = 10./binSize;
  float vMonte = 0.005*binSize;
  for (int iarm=0; iarm<numberOfArms; iarm++) {
    nominalT0[iarm]           =tMonte;
    nominalDriftVelocity[iarm]=vMonte;
    for (int iside=0; iside<numberOfSides; iside++) {
      for (int iplane=0; iplane<numberOfPlanes; iplane++) {
	deltaDist[iarm][iside][iplane]=deltaDis[iarm][iside][iplane];
	for (int icell=0; icell<numberOfCells; icell++) {
	  tZero[iarm][iside][iplane][icell]         = tMonte;
	  driftVelocity[iarm][iside][iplane][icell] = vMonte;
	}
      }
    }
  }
}   

PHDchCalibrationObject::~PHDchCalibrationObject()
{
  commit();
}

void 
PHDchCalibrationObject::setExplicitCalibration(PdbIndex *index, 
				               float t,
					       float v)
{
  // Overwrites Explicit calibration for one channel.
  int globalvalue = index->getValue();
  dchaddress->setGlobalIndex(globalvalue);

  int iarm  = dchaddress->getArm()->getValue();
  int iside = dchaddress->getSide()->getValue();
  int iplane= dchaddress->getPlane()->getValue();
  int icell = dchaddress->getCell()->getValue();

  tZero[iarm][iside][iplane][icell]         = t;
  driftVelocity[iarm][iside][iplane][icell] = v;
}

void  PHDchCalibrationObject::setExplicitT0(int iarm, int iside, int iplane, int icell,float t)
{
  tZero[iarm][iside][iplane][icell] = t;

}
void  PHDchCalibrationObject::setExplicitDriftVelocity(int iarm, int iside, int iplane, int icell ,float v)
{
 driftVelocity[iarm][iside][iplane][icell] = v;

}  



void 
PHDchCalibrationObject::setExplicitT0(PdbIndex *index, 
				      float t)
{
  // Overwrites Explicit tZero for one channel.
  int globalvalue = index->getValue();
  dchaddress->setGlobalIndex(globalvalue);

  int iarm  = dchaddress->getArm()->getValue();
  int iside = dchaddress->getSide()->getValue();
  int iplane= dchaddress->getPlane()->getValue();
  int icell = dchaddress->getCell()->getValue();

  tZero[iarm][iside][iplane][icell] = t;
}

void 
PHDchCalibrationObject::setExplicitDriftVelocity(PdbIndex *index, 
						 float v)
{
  // Overwrites Explicit driftVelocity for one channel.
  int globalvalue = index->getValue();
  dchaddress->setGlobalIndex(globalvalue);

  int iarm  = dchaddress->getArm()->getValue();
  int iside = dchaddress->getSide()->getValue();
  int iplane= dchaddress->getPlane()->getValue();
  int icell = dchaddress->getCell()->getValue();

  driftVelocity[iarm][iside][iplane][icell] = v;
}


float
PHDchCalibrationObject::getExplicitT0(int iarm, int iside, int iplane, int icell)
{
  return tZero[iarm][iside][iplane][icell];
}


float
PHDchCalibrationObject::getExplicitT0(PdbIndex *index)
{
  // Return Explicit T0 for one channel.
  int globalvalue = index->getValue();
  dchaddress->setGlobalIndex(globalvalue);

  int iarm  = dchaddress->getArm()->getValue();
  int iside = dchaddress->getSide()->getValue();
  int iplane= dchaddress->getPlane()->getValue();
  int icell = dchaddress->getCell()->getValue();

  return tZero[iarm][iside][iplane][icell];
}


float
PHDchCalibrationObject::getExplicitDriftVelocity(int iarm, int iside, int iplane, int icell)
{

  return driftVelocity[iarm][iside][iplane][icell];
}

float
PHDchCalibrationObject::getExplicitDriftVelocity(PdbIndex *index)
{
  // Return Explicit driftVelocity for one channel.
  int globalvalue = index->getValue();
  dchaddress->setGlobalIndex(globalvalue);

  int iarm  = dchaddress->getArm()->getValue();
  int iside = dchaddress->getSide()->getValue();
  int iplane= dchaddress->getPlane()->getValue();
  int icell = dchaddress->getCell()->getValue();

  return driftVelocity[iarm][iside][iplane][icell];
}

PHBoolean
PHDchCalibrationObject::update(PHTimeStamp &Tstart,
			       PHTimeStamp &Tstop,
                               const char *calibname,
                               PdbBankID bankID,
                               const char *descrip)
{
  // This routine takes the parameters from the calibration presently
  // in memory and writes them out into the database.
  char genName[50];
  char calName[50];
  char sleName[50];
  char locName[50];
  char steName[50];
  strcpy(genName,calibname);
  strcpy(calName,genName);
  strcpy(sleName,genName);
  strcat(sleName,"slew"); 
  strcpy(locName,genName);
  strcat(locName,"local");
  strcpy(steName,genName);
  strcat(steName,"stereo");


  // Check that Database is writeable...
  if (committed == 1) {
    if (!application->startUpdate()) {
      PHMessage("PHDchCalibrationObject",PHError, "Aborting ... Database not writable");
      application->abort();
    }
    else {
      committed = 0;
    }
  }

  start = Tstart;
  stop  = Tstop;

  int ih=0, length=0;
  PdbParameter *calibrationValue=0;

  // The first of the 4 banks to be filled is the calibrationBank.
  // It contains the following variables in order:
  //    /headerParameters  == a counter to self-describe header (usually 8).
  //   | RunNumber
  // h | BbcMean           == the mean time reported by the BBC while the DC
  // e |                      calibration was determined.  By storing this we 
  // a |                      make our calibration constants immune to
  // d |                      troubles from BBC calibration updates (cool, huh?)
  // e | ZdcMean           == as above
  // r | BccCounts         == Number of valid measurements for Mean
  //   | ZdcCounts         == Number of valid measurements for Mean
  //   | NumberOfCalibrations == number of Calib sets (presently==numberOfArms)
  //   \ BinSize           == TDC bin in nsec (related to RHIC clock)
  //
  // b / T0                == Nominal T0 for east detector.
  // o | Drift Velocity    == Nominal Drift Velocity for east.
  // d | T0                == Nominal T0 for west detector.
  // y \ Drift Velocity    == Nominal Drift Velocity for west.
  //
  if (committingFlag == 0 || committingFlag == 1) {
    cout << " Committing the T0, vd Nominal calibration " << endl;
    calibrationBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, calName);

    length = numberOfArms*channelParameters;  //to, vdrift,  temporary     
    calibrationBank->setLength(length+headerParameters); // + parameters

    ih=0;
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(headerParameters);
    calibrationValue->setName("headerParameters");
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(runNumber);
    calibrationValue->setName("RunNumber");
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(bbcMean);
    calibrationValue->setName("BbcMean");
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(zdcMean);
    calibrationValue->setName("ZdcMean");
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(bbcCounts);
    calibrationValue->setName("BbcCounts");
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(zdcCounts);
    calibrationValue->setName("ZdcCounts");
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(dchaddress->getArm()->getNumberOf());
    calibrationValue->setName("NumberOfCalibs"); // Less than 20 characters  TKH 1-4-2004 (4 years late)
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
    calibrationValue->setParameter(binSize);
    calibrationValue->setName("BinSize");
    if (headerParameters != ih) {
      PHMessage("PHDchCalibrationObject",
		PHWarning,
		"Not correct number of parameter in CalibrationInfo ");
    }

    for( int i=0; i< dchaddress->getArm()->getNumberOf();i++) { // calibration channels
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(i*channelParameters  + headerParameters);
      calibrationValue->setParameter(nominalT0[i]);
      calibrationValue->setName("T0");
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(i*channelParameters+1 + headerParameters);
      calibrationValue->setParameter(nominalDriftVelocity[i]);
      calibrationValue->setName("Drift Velocity");
    }
  }

  // The second of the 4 banks to be filled is the slewBank.
  // It contains the following variables in order:
  // h / headerParameters == a counter to self-describe header (usually 3).
  // d | NChannels        == number of slew correction sets (presently 4).
  // r \ nParameter       == the number of constants in each slew set (9).
  //
  //   / Arm              == Arm for these constants.
  //   | Module           == module for these constants (X1 == 1, V2 == 6)
  // b | Cutoff           == Boundary between Polynomials
  // o | p0a     -|
  // d | p1a       =- First polynomial
  // y | p2a     _|
  //   | p0b     -|
  //   | p1b       =- Second Polynomial
  //   \ p2b     _|
  //
  if (committingFlag == 0 || committingFlag == 2) {
    cout << "Committing the Slewing Correction Calibration " << endl;
    slewBank        = bankManager->createBank("PdbParameterBank",
					      bankID,descrip,Tstart,Tstop,sleName);
    length = sle_head + (sle_npar*sle_ncha);
    slewBank->setLength(length);

    ih=0;
    calibrationValue =  (PdbParameter*)&slewBank->getEntry(ih++);
    calibrationValue->setParameter(sle_head);
    calibrationValue->setName("headerParameters");
    calibrationValue =  (PdbParameter*)&slewBank->getEntry(ih++);
    calibrationValue->setParameter(sle_ncha);
    calibrationValue->setName("NChannels");
    calibrationValue =  (PdbParameter*)&slewBank->getEntry(ih++);
    calibrationValue->setParameter(sle_npar);
    calibrationValue->setName("nParameter");

    if (sle_head != ih) {
      PHMessage("PHDchCalibrationObject",
		PHWarning,
		"Not correct number of parameter in SlewInfo ");
    }

    int arm=0,module=1;
    for( int i=0; i< sle_ncha; i++) { // calibration channels

      if (i == 0) { arm = 0; module = 1;}  // 
      if (i == 1) { arm = 0; module = 4;}  // Presently only X1
      if (i == 2) { arm = 1; module = 1;}  // and X2 sections are slew corrected.
      if (i == 3) { arm = 1; module = 4;}  //
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar  + sle_head);
      calibrationValue->setParameter(arm);
      calibrationValue->setName("Arm");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+1 + sle_head);
      calibrationValue->setParameter(module);
      calibrationValue->setName("Module");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+2 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][0]);
      calibrationValue->setName("cutoff"); 
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+3 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][1]);
      calibrationValue->setName("p0a");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+4 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][2]);
      calibrationValue->setName("p1a");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+5 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][3]);
      calibrationValue->setName("p2a");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+6 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][4]);
      calibrationValue->setName("p0b");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+7 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][5]);
      calibrationValue->setName("p1b");
      calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+8 + sle_head);
      calibrationValue->setParameter(slewingArray[arm][module][6]);
      calibrationValue->setName("p2b");
    }
  }

  // The third of the 4 banks to be filled is the localBank.
  // It contains wire-by-wire deviations from the Nominal:
  // Nominal T0 and driftVelocity (contained in bank 1)...
  // 
  //   There is no header for this bank...
  //
  //   / Global Index  == DchAddress global index
  // b | t0            == Shift in t0
  // o | vd            == Shift in vd
  // d | tf  -|
  // y | a    |_ Presently
  //   | b    |  Unused
  //   \ c   _|
  //
  //  Convention: applied = nominal + delta
  //              delta   = applied - nominal
  if (committingFlag == 0 || committingFlag == 3) {
    cout << "Committing the local To, Vd calibration " << endl;
    localBank       = bankManager->createBank("PdbParameterBank",
					      bankID,descrip,Tstart,Tstop,locName);
    int localparameters = 7;
    int totalChannels = dchaddress->getGlobalIndex()->getNumberOf();
    length = (totalChannels)*localparameters; // t0,vd,tf,a,b,c 
    localBank->setLength(length); // + parameters

    float delta_t0=0,delta_vd=0,delta_tf=0;
    float alocal=0,blocal=0,clocal=0;
    for( int i=0; i< totalChannels; i++) { // calibration channels

      dchaddress->setGlobalIndex(i);
      int iarm   = dchaddress->getArm()->getValue();
      int iside  = dchaddress->getSide()->getValue();
      int iplane = dchaddress->getPlane()->getValue();
      int icell  = dchaddress->getCell()->getValue();
      delta_t0 = tZero[iarm][iside][iplane][icell] 
	- nominalT0[iarm];
      delta_vd = driftVelocity[iarm][iside][iplane][icell] 
	- nominalDriftVelocity[iarm];
      delta_tf = 0;
      alocal = 0;
      blocal = 0;
      clocal = 0;

      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 0);
      calibrationValue->setParameter(i);
      calibrationValue->setName("global index");
      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 1);
      calibrationValue->setParameter(delta_t0);
      calibrationValue->setName("t0");
      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 2);
      calibrationValue->setParameter(delta_vd);
      calibrationValue->setName("vd"); 
      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 3);
      calibrationValue->setParameter(delta_tf);
      calibrationValue->setName("tf");
      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 4);
      calibrationValue->setParameter(alocal);
      calibrationValue->setName("a");
      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 5);
      calibrationValue->setParameter(blocal);
      calibrationValue->setName("b");
      calibrationValue =  (PdbParameter*)&localBank->getEntry(i*localparameters + 6);
      calibrationValue->setParameter(clocal);
      calibrationValue->setName("c");
    }
  }

  // The fourth of the 4 banks to be filled is the stereoBank.
  // It contains back drift parameters for stereo wires'
  // "kinked" drift alley
  //
  // h / Scheme       == 1 and denotes the scheme version
  // d | kinkLocation == place at which drift direction turns
  // r | deltaRad[0]  == East Arm radial drift displacement
  //   \ deltaRad[1]  == West Arm radial drift displacement
  //
  // b / arm       == Arm number for next calib constant
  // o | plane     == Plane number for next calib const
  // d | radi      == Radius index (0==inner, 1==outer)
  // y \ deltaDist == distance shift parameter
  //  
  if (committingFlag == 0 || committingFlag == 4) {
    cout << "Committing the Stereo Wire Calibration " << endl;
    stereoBank       = bankManager->createBank("PdbParameterBank",
					       bankID,descrip,Tstart,Tstop,steName);
    int headerLength = 4;
    int stereoParameters = 4;
    int totalChannels = numberOfArms * numberOfPlanes * numberOfSides;
    length = totalChannels*stereoParameters + headerLength; 
    stereoBank->setLength(length); 

    ih=0;
    calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
    calibrationValue->setParameter(1); // Scheme coded below = 1.
    calibrationValue->setName("Scheme");
    calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
    calibrationValue->setParameter(kinkLocation);
    calibrationValue->setName("kinkLocation");
    calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
    calibrationValue->setParameter(deltaRad[EAST]);
    calibrationValue->setName("deltaRadEast");
    calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
    calibrationValue->setParameter(deltaRad[WEST]);
    calibrationValue->setName("deltaRadWest");

    for( int iarm=0; iarm<numberOfArms; iarm++) {
      for( int iplane=0; iplane<numberOfPlanes; iplane++) {
	for( int iside=0; iside<numberOfSides; iside++) {

	  calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
	  calibrationValue->setParameter(iarm);
	  calibrationValue->setName("Arm");
	  calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
	  calibrationValue->setParameter(iplane);
	  calibrationValue->setName("Plane");
	  calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
	  calibrationValue->setParameter(iside);
	  calibrationValue->setName("Radius");
	  calibrationValue =  (PdbParameter*)&stereoBank->getEntry(ih++);
	  calibrationValue->setParameter(deltaDist[iarm][iside][iplane]);
	  calibrationValue->setName("deltaDist");

	}
      }
    }
  }

  return True;
}


PHBoolean
PHDchCalibrationObject::updateValidityTimeForLastBank(PHTimeStamp& Tstart,
						      PHTimeStamp& Tstop,
						      PHTimeStamp& Tsearch,
						      const char *calibname,
						      PdbBankID bankID,
						      int force)
{
  if (!fetch(Tsearch, calibname, bankID))
    {
      return False;
    }

  cout << "Previous Tstop Time to: " << stop << endl;
  cout << "Updating Tstop Time to: " << Tstop << endl;
  if (stop == PHTimeStamp::PHFarFuture || force)
    {
      calibrationBank->setEndValTime(Tstop);
      calibrationBank->setStartValTime(Tstart);
      slewBank->setEndValTime(Tstop);
      slewBank->setStartValTime(Tstart);
      localBank->setEndValTime(Tstop);
      localBank->setStartValTime(Tstart);
      stereoBank->setEndValTime(Tstop);
      stereoBank->setStartValTime(Tstart);
      stop = Tstop;
    }
  else
    {
      PHMessage("PHDchCalibration::updateValidityTimeForLastBank", PHWarning, "Tstop not  in Far Future");
    }

  return True;
}

PHBoolean
PHDchCalibrationObject::fetch(PHTimeStamp &Tsearch,
			      const char *calibname, 
			      PdbBankID bankID)
{
  char genName[50];
  char calName[50];
  char sleName[50];
  char locName[50];
  char steName[50];

  strcpy(genName,calibname);
  strcpy(calName,genName);

  strcpy(sleName,genName);
  strcat(sleName,"slew"); 

  strcpy(locName,genName);
  strcat(locName,"local");

  strcpy(steName,genName);
  strcat(steName,"stereo");

  // This looks like an attempt to open the database
  // for reading...aborts on failure.
  if(committed == 1) {
    if(!application->startRead()) {
      PHMessage("PHDchCalibrationObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed =  0;
    }
  }

  // Fetch the headers from the bank
  cout << "*************************************************************" << endl;
  cout << "* DCH Calibration Banks                                     *" << endl;
  cout << "*************************************************************" << endl;

  PdbParameter *calibrationValue=0;

  // The first of the 4 banks to be read is the calibrationBank.
  // It contains the following variables in order:
  //    /headerParameters  == a counter to self-describe header (usually 8).
  //   | RunNumber
  // h | BbcMean           == the mean time reported by the BBC while the DC
  // e |                      calibration was determined.  By storing this we 
  // a |                      make our calibration constants immune to
  // d |                      troubles from BBC calibration updates (cool, huh?)
  // e | ZdcMean           == as above
  // r | BccCounts
  //   | ZdcCounts
  //   | NumberOfCalibrations == ??
  //   \ BinSize           == TDC bin in nsec (related to RHIC clock)
  //
  // b / T0                == Nominal T0 for east detector.
  // o | Drift Velocity    == Nominal Drift Velocity for east.
  // d | T0                == Nominal T0 for west detector.
  // y \ Drift Velocity    == Nominal Drift Velocity for west.
  //
  if (committingFlag==0 || committingFlag==1)
    {
      calibrationBank = bankManager->fetchBank("PdbParameterBank",bankID,calName,Tsearch);
      if (!calibrationBank ) {
	cout << "PHDchCalibrationObject:: ";
	cout << "ERROR...did not find CALIBRATION bank in database." << endl;
	return False;
      }
      cout << "*************************************************************" << endl;
      cout << "* Calibration Bank Summary (nominal dv and nominal t0                 *  " << endl;
      cout << "* Description:" << calibrationBank->getDescription() << endl;
      cout << "* Begin Vali :" << calibrationBank->getStartValTime() << endl;
      cout << "* End Valid  :" << calibrationBank->getEndValTime() << endl;
      cout << "* Inserted   :" << calibrationBank->getInsertTime() << endl;
      cout << "*************************************************************" << endl;
      
      unsigned int ih = 0;  // run info
      calibrationValue = (PdbParameter*) & calibrationBank->getEntry(ih++);
      headerParameters = (int)(calibrationValue->getParameter() + 0.5);
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      runNumber        = (int)(calibrationValue->getParameter() + 0.5);
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      bbcMean          = calibrationValue->getParameter();
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      zdcMean          = calibrationValue->getParameter();
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      bbcCounts        = (int)(calibrationValue->getParameter() + 0.5);
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      zdcCounts        = (int)(calibrationValue->getParameter() + 0.5);
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      nChannels        = (int)(calibrationValue->getParameter() + 0.5);
      calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(ih++);
      binSize          = calibrationValue->getParameter();
      
      // Fetch the data from the bank
      if (nChannels == numberOfArms) {
	for(int i=0; i < nChannels; i++) {
	  calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(i*channelParameters + headerParameters);
	  nominalT0[i]            = calibrationValue->getParameter();
	  calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(i*channelParameters+1 + headerParameters);
	  nominalDriftVelocity[i] = calibrationValue->getParameter();
	} 
      }
      else {
	cout << "PHDchCalibrationObject::Error ";
	cout << "The number of calibrations in the database = " << nChannels << endl;
	cout << "     The code expects and requires" << numberOfArms << endl;
      }
    }


  // The second of the 4 banks to be filled is the slewBank.
  // It contains the following variables in order:
  // h / headerParameters == a counter to self-describe header (usually 3).
  // d | NChannels        == number of slew correction sets (presently 4).
  // r \ nParameter       == the number of constants in each slew set (9).
  //
  //   / Arm              == Arm for these constants.
  //   | Module           == module for these constants (X1 == 1, V2 == 6)
  // b | Cutoff           == threshold beyond which to saturate slewing
  // o | p0a      -|
  // d | p1a       =- First polynomial
  // y | p2a      _|
  //   | p0b       |
  //   | p1b       =- Second Polynomial
  //   \ p2b      _|
  //
  if (committingFlag==0 || committingFlag==2)
    {
      slewBank        = bankManager->fetchBank("PdbParameterBank",bankID,sleName,Tsearch);
      if (!slewBank ) {
	cout << "PHDchCalibrationObject:: ";
	cout << "ERROR...did not find SLEW bank in database." << endl;
	return False;
      }
      cout << "*************************************************************" << endl;
      cout << "* Slew Bank Summary                                       *  " << endl;
      cout << "* Description:" << slewBank->getDescription() << endl;
      cout << "* Begin Vali :" << slewBank->getStartValTime() << endl;
      cout << "* End Valid  :" << slewBank->getEndValTime() << endl;
      cout << "* Inserted   :" << slewBank->getInsertTime() << endl;
      cout << "*************************************************************" << endl;
      
	int ih= 0;
	calibrationValue =  (PdbParameter*)&slewBank->getEntry(ih++);
	sle_head         = (int)(calibrationValue->getParameter() + 0.5);
	calibrationValue =  (PdbParameter*)&slewBank->getEntry(ih++);
	sle_ncha         = (int)(calibrationValue->getParameter() + 0.5);
	calibrationValue =  (PdbParameter*)&slewBank->getEntry(ih++);
	sle_npar         = (int)(calibrationValue->getParameter() + 0.5);
	slewingArray.resize(0,numberOfArms,0,numberOfModules+1,0,sle_npar-2); // arm, module, par
	slewingArray = 0;
	int module=0, arm=0;
	for(int i=0; i < sle_ncha; i++) {
	  for(int k=0; k < sle_npar; k++) {
	    calibrationValue =  (PdbParameter*)&slewBank->getEntry(i*sle_npar+k + sle_head);
	    if(k == 0) arm = (int)(calibrationValue->getParameter() +0.5);
	    if(k == 1) module = (int)(calibrationValue->getParameter() + 0.5);
	    if (k > 1) {
	      slewingArray[arm][module][k-2] = calibrationValue->getParameter() ;
	    } 
	  }
	}
    }
  
  // The third of the 4 banks to be filled is the localBank.
  // It contains wire-by-wire deviations from the Nominal:
  // Nominal T0 and driftVelocity (contained in bank 1)...
  // 
  //   There is no header for this bank...
  //
  //   / Global Index  == DchAddress global index
  // b | t0            == Shift in t0
  // o | vd            == Shift in vd
  // d | tf  .
  // y | a    ._ Presently
  //   | b    .  Unused
  //   \ c   .
  //
  //  Convention: applied = nominal + delta
  //              delta   = applied - nominal
  if (committingFlag==0 || committingFlag==3)
    {
      localBank       = bankManager->fetchBank("PdbParameterBank",bankID,locName,Tsearch);
      if (!localBank ) {
	cout << "PHDchCalibrationObject:: ";
	cout << "ERROR...did not find LOCAL bank in database." << endl;
	return False;
      }
      cout << "*************************************************************" << endl;
      cout << "* local Bank Summary (delta dv and delta t0                 *  " << endl;
      cout << "* Description:" << localBank->getDescription() << endl;
      cout << "* Begin Vali :" << localBank->getStartValTime() << endl;
      cout << "* End Valid  :" << localBank->getEndValTime() << endl;
      cout << "* Inserted   :" << localBank->getInsertTime() << endl;
      cout << "*************************************************************" << endl;
      
	int global=0;
	float delta_t0=0, delta_vd=0;
	int localparameters=0;
	int totalChannels = dchaddress->getGlobalIndex()->getNumberOf();
	int length = localBank->getLength();
	localparameters = (int) length/totalChannels;
	cout << "Fetching The local wire correction from DB " << endl;
	cout << " Number of parameters is " << localparameters << endl;
	
	for (int i = 0; i < totalChannels; i++) {
	  dchaddress->setGlobalIndex(i);
	  int iarm   = dchaddress->getArm()->getValue();
	  int iside  = dchaddress->getSide()->getValue();
	  int iplane = dchaddress->getPlane()->getValue();
	  int icell  = dchaddress->getCell()->getValue();
	  
	  calibrationValue = (PdbParameter*) & localBank->getEntry(i * localparameters + 0);
	  global = (int)(calibrationValue->getParameter() + 0.5);
	  calibrationValue = (PdbParameter*) & localBank->getEntry(i * localparameters + 1);
	  delta_t0 = calibrationValue->getParameter();
	  calibrationValue = (PdbParameter*) & localBank->getEntry(i * localparameters + 2);
	  delta_vd = calibrationValue->getParameter();
	  
	  if (global != i) cout << "Index miss-match " << endl;
	  
	  tZero[iarm][iside][iplane][icell]         = nominalT0[iarm] + delta_t0;
	  driftVelocity[iarm][iside][iplane][icell] = nominalDriftVelocity[iarm] + delta_vd;
	  
	}
      
    }

  // The fourth of the 4 banks to be filled is the stereoBank.
  // It contains back drift parameters for stereo wires'
  // "kinked" drift alley
  //
  // h / Scheme       == 1 and denotes the scheme version
  // d | kinkLocation == place at which drift direction turns
  // r | deltaRad[0]  == East Arm radial drift displacement
  //   \ deltaRad[1]  == West Arm radial drift displacement
  //
  // b / arm       == Arm number for next calib constant
  // o | plane     == Plane number for next calib const
  // d | radi      == Radius index (0==inner, 1==outer)
  // y \ deltaDist == distance shift parameter
  //  
  if (committingFlag==0 || committingFlag==4)
    {
      stereoBank      = bankManager->fetchBank("PdbParameterBank",bankID,steName,Tsearch);
      if (!stereoBank ) {
	cout << "PHDchCalibrationObject:: ";
	cout << "ERROR...did not find STEREO bank in database." << endl;
	return False;
      }
      cout << "*************************************************************" << endl;
      cout << "* Stereo Bank Summary                                       *  " << endl;
      cout << "* Description:" << stereoBank->getDescription() << endl;
      cout << "* Begin Vali :" << stereoBank->getStartValTime() << endl;
      cout << "* End Valid  :" << stereoBank->getEndValTime() << endl;
      cout << "* Inserted   :" << stereoBank->getInsertTime() << endl;
      cout << "*************************************************************" << endl;

	cout << "Fetching the stereo wire calibration from DB " << endl;
	unsigned int ih=0;
	calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	kinkLocation = calibrationValue->getParameter();
	calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	deltaRad[0]  = calibrationValue->getParameter();
	calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	deltaRad[1]  = calibrationValue->getParameter();

	while (ih<stereoBank->getLength()) {
	  calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	  int iarm   = (int)(calibrationValue->getParameter() + 0.5);
	  calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	  int iplane = (int)(calibrationValue->getParameter() + 0.5);
	  calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	  int iside  = (int)(calibrationValue->getParameter() + 0.5);
	  calibrationValue = (PdbParameter*) & stereoBank->getEntry(ih++);
	  deltaDist[iarm][iside][iplane] = calibrationValue->getParameter();
	}
    }

  // Set start/stop with best available...
  if (calibrationBank)
    {
      start = calibrationBank->getStartValTime();
      stop  = calibrationBank->getEndValTime();
    }
  else if (slewBank)
    {
      start = slewBank->getStartValTime();
      stop  = slewBank->getEndValTime();
    }
  else if (localBank)
    
    {
      start = localBank->getStartValTime();
      stop  = localBank->getEndValTime();
    }
  else if (stereoBank)
    {
      start = stereoBank->getStartValTime();
      stop  = stereoBank->getEndValTime();
    }
      delete calibrationBank;
      calibrationBank = 0;
      delete   slewBank;
      slewBank = 0;
      delete localBank;
      localBank = 0;
      delete stereoBank;
      stereoBank = 0;

  return True;
}


//______________________________________________________________________
PHBoolean PHDchCalibrationObject::fetchFromFile()
{
  
  if( calFile )
  {
    ifstream file;
    file.open(calFile);
    if (!file)
    {
      cout << " could not open input file " << calFile << " !!" << endl;
      return False;
    }
    cout << "PHDchCalibrationObject::fetchFromFile - Reading from file " << calFile << endl;
    cout << "PHDchCalibrationObject::fetchFromFile - Just 2 arms are implemented " << endl;
    
    float value;
    char parName[20];
    float tmp[20];
    memset(tmp,0,sizeof(tmp));
    int total = 0;
    while (!file.eof() && total < 20)
    {
      file >> value >> parName;
      tmp[total++] = value;
    }
    
    total = 0;
    headerParameters = (int) tmp[total++];
    runNumber = (int) tmp[total++];
    bbcMean = tmp[total++];
    zdcMean = tmp[total++];
    bbcCounts = (int) tmp[total++];
    zdcCounts = (int) tmp[total++];
    nChannels = (int) tmp[total++];
    binSize = tmp[total++];
    
    nominalT0[EAST]            = tmp[total++];
    nominalDriftVelocity[EAST] = tmp[total++];
    nominalT0[WEST]            = tmp[total++];
    nominalDriftVelocity[WEST] = tmp[total++];
    
  } else cout << "PHDchCalibrationObject::fetchFromFile - calibration file not set." << endl;
  
  fetchSlewFromFile();
  fetchLocalFromFile();
  fetchStereoFromFile();

  return True;
}

//_________________________________________________________________________
PHBoolean PHDchCalibrationObject::fetchLocalFromFile()
{
  
  // check file was set
  if( !calLocalFile ) return False;
  
  int totalChannels = dchaddress->getGlobalIndex()->getNumberOf();
  PdbIndex* index = dchaddress->getGlobalIndex();

  ifstream file;
  file.open(calLocalFile);

  if (!file) {
    cout << " could not open input file " << calLocalFile << " !!" << endl;
    return False;
  }
  cout << " Reading from file " << calLocalFile << endl;
  cout << "Arm, Side, Cell, Plane to and vd variations  are implemented " << endl;

  float delta_t0=0, delta_vd=0, delta_tf=0, a=0, b=0, c=0;
  int glo=0;
  int total = 0;

  while (!file.eof() && total < totalChannels) {
    file >> glo >> delta_t0 >> delta_vd >> delta_tf >> a >> b >> c;
    index->setValue(glo);
    dchaddress->setGlobalIndex(index);
    int iarm   = dchaddress->getArm()->getValue();
    int iside  = dchaddress->getSide()->getValue();
    int iplane = dchaddress->getPlane()->getValue();
    int icell  = dchaddress->getCell()->getValue();

    tZero[iarm][iside][iplane][icell] = nominalT0[iarm] + delta_t0;
    driftVelocity[iarm][iside][iplane][icell] = nominalDriftVelocity[iarm] + delta_vd;

    total++;
  }

  return True;
}

//_________________________________________________________________________
PHBoolean PHDchCalibrationObject::fetchSlewFromFile()
{
  
  // check filename was set
  if( !slewFile ) return false;
  
  ifstream file;
  file.open(slewFile);

  if (!file)
    {
      cout << " could not open input file " << slewFile << " !!" << endl;
      return False;
    }
  cout << " Reading from file " << slewFile << endl;
  cout << "Just 2 arms and 2 modules(x1 -x2)  are implemented " << endl;

  float value;
  char parName[50];
  float tmp[50];
  memset(tmp,0,sizeof(tmp));
  int total = 0;
  while (!file.eof() && total < 50)
    {
      file >> value >> parName;
      tmp[total++] = value;
    }

  total = 0;
  sle_head = (int) tmp[total++];
  sle_ncha = (int) tmp[total++];
  sle_npar = (int) tmp[total++];
  slewingArray.resize(0,numberOfArms, 0,numberOfModules+1, 0, sle_npar-2);
  slewingArray = 0.0;

  int module= 0;
  int  arm = 0;
  for (int i = 0; i < sle_ncha; i++)
    {
      for (int k = 0; k < sle_npar; k++)
        {
          value = tmp[i * sle_npar + k + sle_head];
          if (k == 0)
            {
	      arm = (int)value;
	    }
          if (k == 1)
            {
	      module = (int)value;
	    }
          if (k > 1)
            {
              slewingArray[arm][module][k - 2] = value; // check if correct
            }
        }
    }

  return True;
}

//_________________________________________________________________________
PHBoolean PHDchCalibrationObject::fetchStereoFromFile()
{

  // check filename was set.
  if( !stereoFile ) return False;
  
  ifstream file;
  file.open(stereoFile);

  if (!file) {
    cout << "   Could not open Stereo Calibration input ASCII file " << stereoFile << " !!" << endl;
    return False;
  }
  cout << " Reading stereo calibration from file " << stereoFile << endl;

  int Scheme=0;
  file >> Scheme;
  if (Scheme!=1) 
  {
    cout << "Stereo File contains unknown data scheme: aborting..." << endl;
    return False;
  }

  file >> kinkLocation;
  file >> deltaRad[EAST];
  file >> deltaRad[WEST];

  int iarm=0, iplane=0, iside=0, iread=0;
  float deltaDis=0;
  while (!file.eof()&&iread<numberOfArms*numberOfPlanes*numberOfSides) {
    file >> iarm >> iplane >> iside >> deltaDis;
    deltaDist[iarm][iside][iplane] = deltaDis;
    iread++;
  }

  return True;
}

PHVector
PHDchCalibrationObject::transformTimeToDistance(const PdbIndex* index,
						const float& time,
						const DchCalibMethod& method,
						const float ddt,
						const float ddv)
{
  if (method==kDchLinearCalib)      return transformTTDLinear(index, time, ddt, ddv);
  if (method==kDchInnerStereoCalib) return transformTTDInnerStereo(index, time, ddt, ddv);
  if (method==kDchOuterStereoCalib) return transformTTDOuterStereo(index, time, ddt, ddv);
  if (method==kDchRiabovCalib)      return transformTTDRiabov(index, time, ddt, ddv);

  cout << "PHDchCalibrationObject::Error Unrecognized calibration method = " << method << endl;
  cout << "                              Hit placed on the wire." << endl;
  PHVector mistake(0,0,0);
  return mistake;
}

PHVector
PHDchCalibrationObject::transformTTDLinear(const PdbIndex* index, const float& time, const float ddt, const float ddv)
{
  PHVector displacement(0,0,0);

  PdbIndex NDX = *index;
  dchaddress->setGlobalIndex(&NDX);
  int iarm   = dchaddress->getArm()->getValue();
  int iside  = dchaddress->getSide()->getValue();
  int iplane = dchaddress->getPlane()->getValue();
  int icell  = dchaddress->getCell()->getValue();

  float VD    = driftVelocity[iarm][iside][iplane][icell]*ddv;
  float TZERO = tZero[iarm][iside][iplane][icell]+ddt;

  displacement.setX( VD*(time-TZERO));
  displacement.setY( 0.0 );

  return displacement;
}

PHVector
PHDchCalibrationObject::transformTTDInnerStereo(const PdbIndex* index, const float& time, const float ddt, const float ddv)
{
  PHVector displacement(0,0,0);

  PdbIndex NDX = *index;
  dchaddress->setGlobalIndex(&NDX);
  int iarm   = dchaddress->getArm()->getValue();
  int iside  = dchaddress->getSide()->getValue();
  int iplane = dchaddress->getPlane()->getValue();
  int icell  = dchaddress->getCell()->getValue();

  float VD    = driftVelocity[iarm][iside][iplane][icell]*ddv;
  float TZERO = tZero[iarm][iside][iplane][icell]+ddt;
  float TIME = time-TZERO; // True time.
  float MagicTime = (kinkLocation-deltaDist[iarm][0][iplane])/VD; //0 for inner.

  if (TIME>=MagicTime) {
    displacement.setX( -(VD*TIME + deltaDist[iarm][0][iplane])); //0 for inner.
    displacement.setY(-deltaRad[iarm] );
    return displacement;
  }

  displacement.setX(  -kinkLocation*(TIME/MagicTime) );
  displacement.setY(-deltaRad[iarm]*(TIME/MagicTime) );
  return displacement;
}

PHVector
PHDchCalibrationObject::transformTTDOuterStereo(const PdbIndex* index, const float& time, const float ddt, const float ddv)
{
  PHVector displacement(0,0,0);

  PdbIndex NDX = *index;
  dchaddress->setGlobalIndex(&NDX);
  int iarm   = dchaddress->getArm()->getValue();
  int iside  = dchaddress->getSide()->getValue();
  int iplane = dchaddress->getPlane()->getValue();
  int icell  = dchaddress->getCell()->getValue();

  float VD    = driftVelocity[iarm][iside][iplane][icell]*ddv;
  float TZERO = tZero[iarm][iside][iplane][icell]+ddt;
  float TIME =  time-TZERO; // True time.
  float MagicTime = (kinkLocation-deltaDist[iarm][1][iplane])/VD; //1 for outer.

  if (TIME>=MagicTime) {
    displacement.setX( -(VD*TIME + deltaDist[iarm][1][iplane]) ); //1 for outer.
    displacement.setY(  deltaRad[iarm] );
    return displacement;
  }

  displacement.setX(   -kinkLocation*(TIME/MagicTime) );
  displacement.setY(  deltaRad[iarm]*(TIME/MagicTime) );
  return displacement;
}

PHVector
PHDchCalibrationObject::transformTTDRiabov(const PdbIndex* index, const float& time, const float ddt, const float ddv)
{
  // This routine is written so that precise X,Y drift trajectories
  // as calculated from GARFIELD can be implemented in the offline
  // code.  This version is simply a placeholder and applies a linear 
  // calibration.

  PHVector displacement(0,0,0);

  PdbIndex NDX = *index;
  dchaddress->setGlobalIndex(&NDX);
  int iarm   = dchaddress->getArm()->getValue();
  int iside  = dchaddress->getSide()->getValue();
  int iplane = dchaddress->getPlane()->getValue();
  int icell  = dchaddress->getCell()->getValue();

  float VD    = driftVelocity[iarm][iside][iplane][icell]*ddv;
  float TZERO = tZero[iarm][iside][iplane][icell]+ddt;
  float sh = 0.;
  float t_c;

  displacement.setX( VD*(time-TZERO));
  displacement.setY( 0.0 );

  t_c = (time-TZERO)/1000.0*binSize;  //time in microsec
  // kinked drift alley impmlemented
  if (iarm==0&&t_c>0.01)
    {
      if (iplane==0) sh = t_c*(3.014-3.976*t_c)*exp(-0.3098/t_c);
      if (iplane==1) sh = t_c*(0.6371-0.2611*t_c)*exp(-0.1052/t_c);
      if (iplane==2) sh = t_c*(0.2225-0.0224*t_c)*exp(-0.1228/t_c);
      if (iplane==10) sh = t_c*(-0.107-0.1956*t_c)*exp(-0.1519/t_c);
      if (iplane==11) sh = t_c*(0.7228-2.297*t_c)*exp(-0.1123/t_c);
    }

  // to be adjusted for the west , good to the 1st order
  if (iarm==1&&t_c>0.01)
    {
      if (iplane==0) sh = t_c*(3.014-3.976*t_c)*exp(-0.3098/t_c);
      if (iplane==1) sh = t_c*(0.6371-0.2611*t_c)*exp(-0.1052/t_c);
      if (iplane==2) sh = t_c*(0.2225-0.0224*t_c)*exp(-0.1228/t_c);
      if (iplane==10) sh = t_c*(-0.107-0.1956*t_c)*exp(-0.1519/t_c);
      if (iplane==11) sh = t_c*(0.7228-2.297*t_c)*exp(-0.1123/t_c);
    }

  displacement.setY( sh );

  return displacement;
}

float
PHDchCalibrationObject::transformNominalTimeToDistance(const long& time)
{
  float VD    = nominalDriftVelocity[EAST];
  float TZERO = nominalT0[EAST];

  return (VD*((float)time-TZERO));
}

long
PHDchCalibrationObject::transformDistanceToNominalTime (const float& distance,
							const short& edge, 
							const float ddt,
							const float ddv)
{
  // Warning!!  Uses Nominal Calibrations!
  // Use this for MC only!!!

  float VD    = nominalDriftVelocity[EAST]*ddv;
  float TZERO = nominalT0[EAST]+ddt;

  long answer = (long) (distance/VD +TZERO);
  return answer;
}

void
PHDchCalibrationObject::printCalibration(PdbIndex* index)
{
  // Prints calibration for one channel.
  int globalvalue = index->getValue();
  dchaddress->setGlobalIndex(globalvalue);

  int iarm  = dchaddress->getArm()->getValue();
  int iside = dchaddress->getSide()->getValue();
  int iplane= dchaddress->getPlane()->getValue();
  int icell = dchaddress->getCell()->getValue();

  cout << "Printing Calibration for:" << endl;
  cout << "    arm: " << iarm << endl;
  cout << "   side: " << iside << endl;
  cout << "  plane: " << iplane << endl;
  cout << "   cell: " << icell << endl;

  cout << "Nominal Tzero: "  << nominalT0[iarm] << endl;
  cout << "Nominal Drift: "  << nominalDriftVelocity[iarm] << endl;
  cout << "Explicit Tzero: " << tZero[iarm][iside][iplane][icell] << endl;
  cout << "Explicit Drift: " << driftVelocity[iarm][iside][iplane][icell] << endl;
}

void
PHDchCalibrationObject::printStereoCalibration()
{
  cout << "     Using the following Stereo constants:" << endl;
  cout << 1 << endl;
  cout << kinkLocation << endl;
  cout << deltaRad[EAST] << endl;
  cout << deltaRad[WEST] << endl;
  for (int iarm = 0; iarm < numberOfArms; iarm++)
    {
      for (int iplane = 0; iplane < numberOfPlanes; iplane++)
        {
          for ( int iside = 0; iside < numberOfSides; iside++)
            {
              cout << iarm << ", ";
              cout << iplane << ", ";
              cout << iside << ", ";
              cout << deltaDist[iarm][iside][iplane] << endl;
            }
        }
    }
}

