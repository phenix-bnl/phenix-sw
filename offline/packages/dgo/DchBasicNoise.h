#ifndef DCHBASICNOISE_H
#define DCHBASICNOISE_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: DchBasicNoise.h                       
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

class DchBasicNoise {
  
public:
  
  DchBasicNoise();
  DchBasicNoise(int, float, PHBoolean);
  DchBasicNoise(int, float, short);
  virtual ~DchBasicNoise(); 
    
  float     getCountsPerEvent() { return countsPerEvent;}
  int       getGlobalIndex()    { return globalIndex;   } 
  PHBoolean getStatus()         { return status;        } // status == 0  if bad channel
 

  void setGlobalIndex(int val)                { globalIndex    = val;}
  void setGlobalIndex(const  PdbIndex& index) { globalIndex    = index.getValue();}
  void setCountsPerEvent(float value)         { countsPerEvent = value;}
  void setStatus(PHBoolean val)               { status         = val;}
 
  void  print();
  
private:
  int globalIndex;
  float countsPerEvent;
  PHBoolean  status;
      
}; 

#endif /* DCHBASICNOISE_H */ 
