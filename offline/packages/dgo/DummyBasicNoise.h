#ifndef DUMMYBASICNOISE_H
#define DUMMYBASICNOISE_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: DummyBasicNoise.h                       
//                                                                
// Created by: Federica Messer at Mon Dec 27 15:19:41 1999                                      
//
// Purpose: The noise calibration Object for the Drift Chamber 
//
// Last update: Mon Dec 27 15:19:41 1999                                              
//                                                                
//----------------------------------------------------------------

#include "phool.h"
#include "PdbIndex.hh"

class DummyBasicNoise {
  
public:
  
  DummyBasicNoise();
  DummyBasicNoise(int, float, PHBoolean);
  virtual ~DummyBasicNoise(); 
    
  float     getCountsPerEvent() { return countsPerEvent;}
  int       getGlobalIndex()    { return globalIndex;   } 
  PHBoolean isNoisy()           { return noisy;       }

  void setGlobalIndex(int val)                { globalIndex    = val;}
  void setGlobalIndex(const  PdbIndex& index) { globalIndex    = index.getValue();}
  void setCountsPerEvent(float value)         { countsPerEvent = value;}
  void isNoisy(PHBoolean val)                 { noisy          = val;}
 
  void  print();
  
private:
  int globalIndex;
  float countsPerEvent;
  PHBoolean  noisy;
    
}; 

#endif /* DCHBASICNOISE_H */ 
