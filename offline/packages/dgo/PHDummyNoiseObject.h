#ifndef PHDUMMYNOISEOBJECT_H
#define PHDUMMYNOISEOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDummyNoiseObject.h                      
//                                                                
// Created by: Federica Messer                                   
//                                                                                                                  
// Purpose: Retrieve (and write) info on Noisy Channels from the DB
//
// Last update:                                   
//                                                                
//----------------------------------------------------------------

#include "PHDummyAddressObject.h"
#include "PHNoiseObject.h"
#include "DummyBasicNoise.h"

class PHTimeStamp;

class PHDummyNoiseObject: public PHNoiseObject { 

public: 
  PHDummyNoiseObject();
  PHDummyNoiseObject(PHDummyAddressObject *add);  
  virtual ~PHDummyNoiseObject();
 
  virtual void initialize();

  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);  
  virtual PHBoolean update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                                 const char *calibname, PdbBankID , char *descrip );

private:
  DummyBasicNoise basicNoise;
  
}; 

#endif /* PHDUMMYNOISEOBJECT_H */ 
