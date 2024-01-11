#ifndef TECHVOBJECT_H
#define TECHVOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 01/24/00
//                                                                
// Description: Header for TecHVObject class
//                                                                
//----------------------------------------------------------------

#include <TObject.h>

#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PdbCoordinate.hh>
#include <PdbADCChan.hh>

#include <PHPoint.h>
#include <TecBasicObject.hh>
#include <TecAddressObject.hh>
#include <TecGeometryObject.hh>

class TecHVObject : public TecBasicObject, public TObject { 

 public:

/// Constructor
  TecHVObject(); 
/// Destructor
  ~TecHVObject(); 

// member functions

///
  void UseSimulationDatabase();
///
  void UseRealDatabase();

/// Fetch high voltage constants from a Database
  PHBoolean Fetch();
  PHBoolean Fetch(int runnumber);
/// Fetch HV values from a Database
  PHBoolean FetchHVVal();
  PHBoolean FetchHVVal(int runnumber);

/// Fetch calibration constants from default ASCII file
  PHBoolean FetchFromFile();

/// Fetch calibration constants from an ASCII file "filename"
  PHBoolean FetchHVValFromFile(const char* filename);

/// Update absolute gain database from current TCO object in memory 
  PHBoolean UpdateHVVal(PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  PHBoolean UpdateHVVal(int runnumber);

/// Get object name
  const char* getName() {return "TEC Calibration Object";}

/// Get absolute gain
  float getHVVal(TecAddressObject* , int j);

/// Set absolute gain
  void setHVVal(TecAddressObject* , int j, float gain);

/// Get absolute gain
  float getHVVal(int index, int j);

/// Set absolute gain
  void setHVVal(int index, int j, float gain);

private:

///
 float HighVolt[TECMAXINDEX][TECMAXHVTYP];

}; 

#endif /* TECHVOBJECT_H */ 


