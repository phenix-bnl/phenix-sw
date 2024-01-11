#ifndef TECCALIBRATIONOBJECT_H
#define TECCALIBRATIONOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 01/24/00
//                                                                
// Description: Header for TecCalibrationObject class
//                                                                
//----------------------------------------------------------------

#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbCoordinate.hh"
#include "PdbADCChan.hh"
#include "PdbTecPedestal.hh"

#include "PHPoint.h"
#include "TecBasicObject.hh"
#include "TecAddressObject.hh"
#include "TecGeometryObject.hh"

/**
This is a TEC Calibration Object class. It is used to calibrate
TEC Raw Data in dTecRaw table. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/software/calib/index.html}
@author Sasha Lebedev (ISU) 
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo TEC Calibration Class
Included noise calibration Cesar(USP) 11/14/2004
*/
class TecCalibrationObject : public TecBasicObject
{ 

 public:

/// Constructor
  TecCalibrationObject(); 
/// Destructor
  virtual ~TecCalibrationObject() {} 

// member functions

///
  void UseSimulationDatabase();
///
  void UseRealDatabase();

/// Fetch calibration constants from a Database
  PHBoolean Fetch();
  PHBoolean Fetch(int runnumber);
/// Fetch Relative gains from a Database
  PHBoolean FetchRelGain();
  PHBoolean FetchRelGain(int runnumber);
/// Fetch Absolute gains from a Database
  PHBoolean FetchAbsGain();
  PHBoolean FetchAbsGain(int runnumber);
/// Fetch Timing calibration constants from a Database
  PHBoolean FetchTimingGain();
  PHBoolean FetchTimingGain(int runnumber);

/// Fetch calibration constants from default ASCII file
  PHBoolean FetchFromFile();

/// Fetch calibration constants from an ASCII file "filename"
  PHBoolean FetchRelGainFromFile(const char* tecdb);

/// Fetch calibration constants from an ASCII file "filename"
  PHBoolean FetchAbsGainFromFile(const char* tecdb);

/// Fetch calibration constants from an ASCII file "filename"
  PHBoolean FetchTimingGainFromFile(const char* tecdb);

/// Fetch a list of hot and dead wires from the Database
  PHBoolean FetchHotDead();
  PHBoolean FetchHotDead(int runnumber);

/// Fetch a list of hot and dead wires from an ASCII file "filename"
  PHBoolean FetchHotDeadFromFile(const char* tecdb);

/// Update absolute gain database from current TCO object in memory 
  PHBoolean UpdateAbsGain(PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  PHBoolean UpdateAbsGain(int runnumber);

/// Update relative gain database from current TCO object in memory 
  PHBoolean UpdateRelGain(PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  PHBoolean UpdateRelGain(int runnumber);

/// Update list of hot/dead wires in the  database from current TCO object in memory 
  PHBoolean UpdateHotDead(PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  PHBoolean UpdateHotDead(int runnumber);

/// Update timing calibration database from current TCO object in memory 
  PHBoolean UpdateTimeCalib(PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  PHBoolean UpdateTimeCalib(int runnumber);

/// Get object name
  const char* getName() {return "TEC Calibration Object";}

/// Get relative gain
  float getRelativeGain(TecAddressObject* );

/// Set relative gain
  void setRelativeGain(TecAddressObject* , float );

/// Get relative gain
  float getRelativeGain(int index, int iwire);

/// Set relative gain
  void setRelativeGain(int index, int iwire, float gain);

/// Get absolute gain
  float getAbsoluteGain(TecAddressObject* );

/// Set absolute gain
  void setAbsoluteGain(TecAddressObject* , float gain);

/// Get absolute gain
  float getAbsoluteGain(int index);

/// Set absolute gain
  void setAbsoluteGain(int index, float gain);

/// Get timing calibration constant
  float getTimingGain(TecAddressObject* , int );

/// Set timing calibration constant
  void setTimingGain(TecAddressObject* , int , float);

/// Get timing calibration constant
  float getTimingGain(int index, int timebin);

/// Set timing calibration constant
  void setTimingGain(int index, int timebin, float gain);

///
  void setFirstTimeBin(int index, int value) {FirstTimeBin[index] = value;}

///
  void setLastTimeBin(int index, int value) {LastTimeBin[index] = value;}

///
  int getFirstTimeBin(int index) {return FirstTimeBin[index];}

///
  int getLastTimeBin(int index) {return LastTimeBin[index];}

/// 
  int getHotDeadList(int* hotlist);

/// Set number of noise hits per timebin for adc value and glindex wire
  void setNoise(int glindex, int adc, float noise);
  
/// Set number of noise hits per timebin for adc value and TAO object
  void setNoise(TecAddressObject* TAO, int adc, float noise);
  
  ///Get number of noise hits per timebin for adc value in plane index and iwire
  float getNoise(int index, int iwire, int adc);

/// Update noise in database for runnumber
  int UpdateNoise(int runnumber);

/// Update noise in database during Tbeg and Tend time stamps
  int UpdateNoise(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

/// Fetch noise from database
  PHBoolean FetchNoise();

private:

///
 float RelativeGain[TECMAXINDEX][TECMAXWIRE];

///
 float AbsoluteGain[TECMAXINDEX];

///
 float TimingGain[TECMAXINDEX][TECMAXTIMEBIN];

///
 int FirstTimeBin[TECMAXINDEX];

///
 int LastTimeBin[TECMAXINDEX];

///
 int HotDeadList[TECMAXNUMHOT];

///
 float Noise[TECMAXINDEX][TECMAXWIRE][32];

}; 

#endif /* TECCALIBRATIONOBJECT_H */ 


