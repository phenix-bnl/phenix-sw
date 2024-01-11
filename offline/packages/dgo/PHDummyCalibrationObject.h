#ifndef PHDUMMYCALIBRATIONOBJECT_H
#define PHDUMMYCALIBRATIONOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDummyCalibrationObject.h                       
//                                                                
// Created by: Federica Ceretto                                      
//                                                                
// Description:
//
// Purpose: Retrieve (and write) Calibration info from the Database  (Dummy)
//
// Last update:                                        
//                                                                
//----------------------------------------------------------------

#include "DummyBasicCalibration.h"
#include "PHDummyAddressObject.h"
#include "PHCalibrationObject.h"
class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHDummyCalibrationObject : public PHCalibrationObject { 
  
public:
  
  PHDummyCalibrationObject(PHDummyAddressObject *add); 
  virtual ~PHDummyCalibrationObject();
  
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);
  virtual PHBoolean update(PHTimeStamp&,PHTimeStamp&,const char *,PdbBankID, char * );

  DummyBasicCalibration getBasicCalibration(const PdbIndex *index);
  
  float calculateAmplitude(float amp);
  float calculateAmplitude(const PdbIndex* index, float amp);
  
  void  printCalibration(PdbIndex* index);
   
protected:

  void initialize();
  
private:
  int nChannels;
  int channelParameters;
  DummyBasicCalibration channelCalibration;

}; 

#endif /* PHDUMMYCALIBRATIONOBJECT_H */ 
