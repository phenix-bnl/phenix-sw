#ifndef PHRECONSTRUCTIONOBJECT_H
#define PHRECONSTRUCTIONOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHReconstructionObject.h                      
//                                                                
// Created by: Federica Messer at Mon Dec 27 13:11:37 1999                                      
//                                                                                                                  
// Purpose: Retrieve (and write) info on Noisy Channels from the DB
//
// Last update:Mon Dec 27 13:11:37 1999                                      
//                                                                
//----------------------------------------------------------------

#include "PdbParameter.hh"
#include "PHTimeStamp.h"
#include "phool.h"
#include "PdbBankID.hh"

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHReconstructionObject { 

public: 
  PHReconstructionObject();  
  virtual ~PHReconstructionObject();

  virtual void print();
  virtual void initialize() = 0;

  const PHTimeStamp getStartValTime() const;
  const PHTimeStamp getEndValTime()   const;

  virtual PHBoolean commit();
  virtual PHBoolean validate(PHTimeStamp &Tsearch); 
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID) = 0;  
  virtual PHBoolean update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                                 const char *calibname, PdbBankID , char *descrip ) = 0;
  
protected:
  
  short committed;
  int recoParameters;

  PdbBankManager *bankManager;
  PdbApplication *application;
  PdbCalBank     *recoBank;   

  PHTimeStamp start,stop;
}; 

#endif /* PHRECONSTRUCTIONOBJECT_H */ 
