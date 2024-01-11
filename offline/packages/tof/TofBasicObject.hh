//-----------------------------------------------------------------------------
//  Declaration of class TofBasicObject
//
//  Purpose: Basic Object for Time-of-Flight
//
//  Description: TOF Basic Object
//
//  Author: Akio Kiyomichi (Univ.of Tsukuba)
//
//  History: 04/10/00  A.Kiyomichi  First Version
//
//-----------------------------------------------------------------------------
#ifndef __TOFBASICOBJECT_HH__
#define __TOFBASICOBJECT_HH__

#include "PHString.h"
#include "PHTimeStamp.h"
#include "PdbBankID.hh"

//---------------------------------------------------------------------
//  This is a Basic TOF Object class. 
//  All other TOF classes inherit from it. <br> 
//  Created: 04/10/00. 
//  @author Akio Kiyomichi (Univ. of Tsukuba)
//  <a href="mailto:kiyo@bnl.gov">kiyo@bnl.gov</a> 
//  @memo TOF Basic Object Class 
//---------------------------------------------------------------------


class TofBasicObject{
public:
  // Constructor
  TofBasicObject();
  // Destructor
  virtual ~TofBasicObject();

  // public member functions 
 
  // Check if this object is valid at data member time Tsearch 
  PHBoolean Validate();
  // Check if this object is valid at time TS 
  PHBoolean Validate(PHTimeStamp& TS);

  // Get start validity time
  PHTimeStamp getStartValidity();
  // Get end validity time
  PHTimeStamp getEndValidity();

  PHTimeStamp getTimeStamp(const int run);

  // Get
  int getDebugLevel() const { return Debug; }
  PHString getCalibName() const { return CalibName; }
  PHString getGeom0Name() const { return Geom0Name; }
  PHString getGeom1Name() const { return Geom1Name; }
  int getBankID() const { return BankID.getInternalValue(); }
  int getBankNumber() const { return BankNumber; }
  PHTimeStamp getTimeStamp() const { return Tsearch; }

  PHString getUserName() const { return UserName; }


  // Set
  void setDebugLevel(const int debug) { Debug = debug; }
  void setCalibName(const PHString& string) { CalibName = string; }
  void setGeom0Name(const PHString& string) { Geom0Name = string; }
  void setGeom1Name(const PHString& string) { Geom1Name = string; }
  void setBankID(const int bankid) { BankID.setInternalValue(bankid); }
  void setBankNumber(const int number) { BankNumber=number; }
  void setTimeStamp(const PHTimeStamp& TS) { Tsearch = TS; }

  void setUserName(const PHString& string) { UserName = string; }

  void Print();

protected:
  static const int TOFMAPBANK;
  static const int TOFGEOMBANK;
  static const int TOFCALIBBANK;

  // Time Stamp
  PHTimeStamp Tsearch;
 
  // Calibration name, ex "calib.tof.badch".  
  // Provides hint for where to find the calibration information 
  PHString CalibName;
  PHString Geom0Name;   // ex "geom.tof.panel"
  PHString Geom1Name;   // ex "geom.tof.slatoffset"
  PHString UserName;

  // Calibration bank ID
  PdbBankID BankID;

  // Calibration bank number, ex 7100 for TOF FEM map bank
  int BankNumber;

  // Status of the object (0 = ok/initialized)
  int iFlag;

  // Debug flag (0 = do not debug,
  // implications of other values is decided by user of TofBasicObject) 
  int Debug;

private:

};

#endif /* __TOFBASICOBJECT_HH__ */
