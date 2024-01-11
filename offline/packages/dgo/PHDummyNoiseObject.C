//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDummyNoiseObject.h                      
//                                                                
// Created by: Federica Messer at Mon Dec 27 13:11:37 1999                                      
//
//                                                                
//----------------------------------------------------------------

#include "PHDummyNoiseObject.h"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"

PHDummyNoiseObject::PHDummyNoiseObject():PHNoiseObject()
{
  initialize();
}

PHDummyNoiseObject::PHDummyNoiseObject(PHDummyAddressObject *add):PHNoiseObject(add)
{
  initialize(); 
}

void PHDummyNoiseObject::initialize()
{
  committed = 1;

  // dummy parameters
  noiseParameters = 3; 
  noiseThreshold = 45;

}

PHDummyNoiseObject::~PHDummyNoiseObject()
{
  commit();
}


PHBoolean PHDummyNoiseObject::update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                  const char *calibname,PdbBankID bankID, char *descrip )
{

  if(committed == 1) {
    if(!application->startUpdate()) {
      PHMessage("PHNoiseObject",PHError, "Aborting ... Database not writable");
      application->abort();
    }else{
      committed = 0;
    }
  }
  noiseBank = bankManager->createBank("PdbParameterBank",bankID,descrip,Tstart,Tstop,calibname);

  int  length = (numberOfNoisyChannels)*noiseParameters; 
  noiseBank->setLength(length);
  print();

  PdbParameter *noiseValue;
  float count_event;
  
  for( int i=0; i < noiseBank->getLength();i++) {
       noiseValue =  (PdbParameter*)&noiseBank->getEntry(i);
       if ((i%noiseParameters) == 0 ) {
	 noiseValue->setParameter(i);
	 noiseValue->setName("Global Index");
       }       
       if ((i%noiseParameters) == 1 ) {
	 count_event = (i-1)/10.;
	 noiseValue->setParameter(count_event);
	 noiseValue->setName("Counts/Event");
       }       
       if ((i%noiseParameters) == 2 ) {
	 if (count_event >= noiseThreshold) {
	   noiseValue->setParameter(1.);
	   noiseValue->setName("NOISY");
	 }else {
	   noiseValue->setParameter(0.);
	   noiseValue->setName("NOISY");
	 }
       }
    }
  
  return True;
}
 
PHBoolean PHDummyNoiseObject::fetch(PHTimeStamp &Tsearch, const char *calibname,PdbBankID bankID )
{
  if(committed == 1) {
    if(!application->startRead()) {
      PHMessage("PHNoiseObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed =  0;
    }
  }
  noiseBank = bankManager->fetchBank("PdbParameterBank",bankID,calibname,Tsearch);
  
  DummyBasicNoise noise;
  PdbParameter *noiseValue;

  numberOfNoisyChannels = noiseBank->getLength()/noiseParameters;
    
  for(int i=0; i < noiseBank->getLength(); i++) {
    noiseValue =  (PdbParameter*)&noiseBank->getEntry(i);
    if ((i%noiseParameters) == 0) noise.setGlobalIndex(noiseValue->getParameter());
    if ((i%noiseParameters) == 1) noise.setCountsPerEvent(noiseValue->getParameter());
    if ((i%noiseParameters) == 2) noise.isNoisy(noiseValue->getParameter());
  }
   
  return True;
  
}   







