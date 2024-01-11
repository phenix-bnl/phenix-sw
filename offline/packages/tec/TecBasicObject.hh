#ifndef TECBASICOBJECT_H
#define TECBASICOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 01/24/00
//                                                                
// Description: Header for TecBasicObject class
//                                                                
//----------------------------------------------------------------

#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbCoordinate.hh"
#include "PdbADCChan.hh"

const int TECMAXSECT = 4;
const int TECMAXSIDE = 2;
const int TECMAXPLANE = 6;
const int TECMAXWIRE = 468;
const int TECMAXCRATE = 32;
const int TECMAXSLOT = 25;
const int TECMAXCHANNEL = 64;
const int TECMAXINDEX = 48;
const int TECMAXTIMEBIN = 80;
const int TECMAPDBMAXENT = 1328;
const int TECMAXHVTYP = 2; // Max # of HV types (AN and DW for now)
const float TECYWIDTH = 3.7032;   // TEC plane width in GEANT
const float TECTRDWIDTH = 7.065;  // TRD plane width in GEANT
const float TECWIREDIST = 0.3;    // distance from outer plane edge to the wire
const float TECMIDPOS = 8.9166;   // distance from the middle of the TEC plane
				  // to inner TEC plane edge
const float TECPHIBOTE = 213.75;
const float TECPHITOPE = 123.75;
const float TECPHIBOTW = -33.75;
const float TECPHITOPW = 56.25;
const float TECZWIDTH1 = 310.945;
const float TECZWIDTH2 = 318.655; 
const float TECZWIDTH3 = 326.365;
const float TECZWIDTH4 = 334.075;
const float TECZWIDTH5 = 341.785;
const float TECZWIDTH6 = 349.495;
const int TECNUMWIRES1 = 415;
const int TECNUMWIRES2 = 415;
const int TECNUMWIRES3 = 436;
const int TECNUMWIRES4 = 447;
const int TECNUMWIRES5 = 446;
const int TECNUMWIRES6 = 468;
const float TECWIRESPACING1 = 0.405;
const float TECWIRESPACING2 = 0.415;
const float TECWIRESPACING3 = 0.405;
const float TECWIRESPACING4 = 0.405;
const float TECWIRESPACING5 = 0.415;
const float TECWIRESPACING6 = 0.405;
const float TECREFERENCERADIUS = 450.0;
const float TECRADIUS1 = 433.1166;
const float TECRADIUS2 = 443.8848;
const float TECRADIUS3 = 454.6530;
const float TECRADIUS4 = 465.4212;
const float TECRADIUS5 = 476.1894;
const float TECRADIUS6 = 486.9576;
const int TECMAPBANK = 5100;
const int TECGEOMBANK = 5200;
const int TECCAL1BANK = 5300;
const int TECCAL2BANK = 5400;
const int TECCALTBANK = 5500;
const int TECHVBANK = 5600;
const int TECMAXPACKETS = 800;
const int TECBASEPACKETID = 5001;
const int TECPACKETLENGTH = 1352;
const int TECDCMHEADERSIZE = 5;
const int TECDCMDATASIZE = 1280;
const int TECDCMLISTSUMSIZE = 64;
const int TECDCMTRAILERSIZE = 3;
const int TECDCMSIZE = 1352;
const int TECDCMFEMSIZE = 20;           // 20 data words per FEM
const int TECMAXNUMHOT = 1000;

/**
This is a Basic TEC Object class.
All other TEC classes inherit from it. <br>
Created: 01/24/00.
@author Sasha Lebedev (ISU) 
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo TEC Basic Object Class
*/
class TecBasicObject { 

 public:

/// Constructor
  TecBasicObject(); 
/// Destructor
  virtual ~TecBasicObject(); 

// public member functions
// Fetch information from Objectivity Database
//  virtual PHBoolean Fetch() {}
// Fetch information from an ASCII file
//  virtual PHBoolean FetchFromFile() {}
// Get object name
//  virtual const char* getName() {}

/// Check if this object is valid at data member time Tsearch
  PHBoolean Validate();
/// Check if this object is valid at time TS
  PHBoolean Validate(PHTimeStamp* TS);
/// Get start validity time
  PHTimeStamp getStartValidity();
/// Get end validity time
  PHTimeStamp getEndValidity();

///
  void setDebugLevel(int debug) { Debug = debug; }
///
  void setCalibName(const char* );
///
  void setBankID(int bankid) { BankID.setInternalValue(bankid); }
///
  void setBankNumber(int number) { BankNumber=number; }
///
  void setTimeStamp(PHTimeStamp TS) { Tsearch = TS; }
///
  void setRunNumber(int);
///
  PHBoolean SetRunNumber(int);
///
  const char* getCalibName() { return CalibName; }
///
  int getBankID() { return BankID.getInternalValue(); }
///
  int getBankNumber() { return BankNumber; }
///
  const char* getDescription() { return Description; }
///
  void setDescription(const char* desc) { Description = desc; }
///
  PHTimeStamp getTimeStamp() { return Tsearch; }

 protected:

/// Calibration bank name (location)
  const char* CalibName;  
  char CalibNameA1[25];
  char CalibNameA2[25];
  char CalibNameT[25];
  char CalibNameHV[25];

/// Time Stamp
  PHTimeStamp Tsearch;

/// Bank description
  const char* Description;

/// Calibration bank ID
  PdbBankID BankID; 

/// Calibration bank number
  int BankNumber; 

// Status of the object (0 = ok)
  int iFlag;

/// Debug flag
  int Debug;
}; 

#endif /* TECBASICOBJECT_H */ 


