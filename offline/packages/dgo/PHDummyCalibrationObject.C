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
// Purpose: Write in/Read from the DB the Calibration Values                       
//                                                                
// Last update:                                            
//                                                                
//----------------------------------------------------------------
 
#include "PdbParameter.hh"
#include "PHDummyCalibrationObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

void PHDummyCalibrationObject::initialize()
{
  nChannels = 1;
  channelParameters = 2;
}

PHDummyCalibrationObject::PHDummyCalibrationObject(PHDummyAddressObject *add) : PHCalibrationObject(add)
{
  initialize();
}   

PHDummyCalibrationObject::~PHDummyCalibrationObject()
{
  commit();
}

float PHDummyCalibrationObject::calculateAmplitude(float amp)
{
  return channelCalibration.calculateAmplitude(amp);
}

float PHDummyCalibrationObject::calculateAmplitude(const PdbIndex* index, float amp)
{
  channelCalibration = getBasicCalibration(index);
  return channelCalibration.calculateAmplitude(amp); 
}


DummyBasicCalibration PHDummyCalibrationObject::getBasicCalibration(const PdbIndex* index)                
{
  // not doing anything now ! but the purpose is to return the calibration for the
  // correct channel of PdbIndex  index.
  
  PHMessage("PHDummyCalibrationObject::getBasicCalibration",PHWarning,"Nothing implemented");
  DummyBasicCalibration newCalibration;

  return channelCalibration;
} 

void PHDummyCalibrationObject::printCalibration(PdbIndex* index)
{
   getBasicCalibration(index).print(); 
}



PHBoolean PHDummyCalibrationObject::update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                  const char *calibname, PdbBankID bankID, char *descrip )
{
  if (committed == 1) {                       
    if(!application->startUpdate()) {
      PHMessage("PHDummyCalibrationObject",PHError, "Aborting ... Database not writable");
      application->abort();
    }else{
      committed = 0;
    }
  }

  calibrationBank = bankManager->createBank("PdbParameterBank",bankID,descrip,Tstart,Tstop,calibname);
  
  int  length = nChannels*channelParameters;  //to, vdrift, binsize  temporary     
  calibrationBank->setLength(length);
  
  PdbParameter *calibrationValue;
  for( int i=0; i< calibrationBank->getLength();i++) {
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(i);
    if ((i%channelParameters) == 0 ) {
      calibrationValue->setParameter(channelCalibration.getGain());
      calibrationValue->setName("Gain");
    }
    if ((i%channelParameters) == 1 ) {
      calibrationValue->setParameter(channelCalibration.getPedestal());
      calibrationValue->setName("Pedestal");
    }
  }
  return True;
}
   
   
PHBoolean PHDummyCalibrationObject::fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID )
{
                               
  if(committed == 1) {
    if(!application->startRead()) {
      PHMessage("PHDummyCalibrationObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed =  0;
    }
  }
   
  calibrationBank = bankManager->fetchBank("PdbParameterBank",bankID,calibname,Tsearch);
  
  PdbParameter *calibrationValue;
  for(int i=0; i < calibrationBank->getLength(); i++) {
    calibrationValue =  (PdbParameter*)&calibrationBank->getEntry(i);
    if ( (i%channelParameters) == 0 ) channelCalibration.setGain(calibrationValue->getParameter());
    if ( (i%channelParameters) == 1 ) channelCalibration.setPedestal(calibrationValue->getParameter());
  }
  
  return True;
  
}   



