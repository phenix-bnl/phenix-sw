#ifndef PADBASICOBJECT_H
#define PADBASICOBJECT_H

// Created by: David Silvermyr
// Description: Header for PadBasicObject class

#include <PdbBankID.hh>
#include <PdbCalBank.hh>

#include <cstdlib>
#include <iostream>
#include <cstdio>

/**
   This is a Basic PAD Object class.
   All other PAD classes that want to access some sort of database, 
   inherit from it. <br>
   @memo PAD Basic Object Class
*/
class PadBasicObject { 
  
public:
  
  // Constructor
  PadBasicObject();
  // Destructor
  virtual ~PadBasicObject() {} 
  
  // public member functions

  // Check if this object is valid at data member time Tsearch
  PHBoolean Validate();
  // Check if this object is valid at time TS
  PHBoolean Validate(PHTimeStamp* TS);
  // Get start validity time
  PHTimeStamp getStartValidity();
  // Get end validity time
  PHTimeStamp getEndValidity();

  void setDebugLevel(int debug) { Debug = debug; }
  void setCalibName(PHString string) { CalibName = string; }
  void setBankID(int bankid) { BankID.setInternalValue(bankid); }
  void setBankNumber(int number) { BankNumber=number; }
  void setTimeStamp(PHTimeStamp TS) { Tsearch = TS; }

  int getiFlag() { return iFlag; }

  void setTimeStampUpdateStart(PHTimeStamp TS) { TUpdateStart = TS; }
  void setTimeStampUpdateStop(PHTimeStamp TS) { TUpdateStop = TS; }

 protected:

  // Time Stamp
  PHTimeStamp Tsearch;

  // validity ranges when updating the Objy database
  PHTimeStamp TUpdateStart,TUpdateStop;

  // Calibration name, ex "calib.pad.badch". 
  // Provides hint for where to find the calibration information
  PHString CalibName;  

  // Calibration bank ID
  PdbBankID BankID; 
  
  // Calibration bank number, ex 4100 for BadCh bank
  int BankNumber; 
  
  // Status of the object (0 = ok/initialized)
  int iFlag;

  // Debug flag (0 = do not debug, 
  // implications of other values is decided by user of PadBasicObject)
  int Debug;
}; 

#endif /* PADBASICOBJECT_H */ 


