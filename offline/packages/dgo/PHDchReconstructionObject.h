#ifndef PHDCHRECONSTRUCTIONOBJECT_H
#define PHDCHRECONSTRUCTIONOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDchReconstructionObject.h                      
//                                                                
// Created by: Federica Messer at Mon Dec 27 13:11:37 1999                                      
//                                                                                                                  
// Purpose: Retrieve (and write) info on Noisy Channels from the DB
//
// Last update:Mon Dec 27 13:11:37 1999                                      
//                                                                
//----------------------------------------------------------------

#include "PHReconstructionObject.h"
#include "DchBasicReconstruction.h"

class PHDchReconstructionObject: public PHReconstructionObject { 

public: 
  PHDchReconstructionObject();
  virtual ~PHDchReconstructionObject();

  virtual void initialize();
  
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);  
  virtual PHBoolean update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,
                                 const char *calibname, PdbBankID , char *descrip );


  virtual PHBoolean fetchReconstructionFromFile();
  
private:
   DchBasicReconstruction* basicReco;
}; 

#endif /* PHDCHRECONSTRUCTIONOBJECT_H */ 
