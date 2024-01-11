#ifndef PHDUMMYADDRESSOBJECT_H
#define PHDUMMYADDRESSOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDummyAddressObject.h                       
//                                                                   
// Created by: Federica Messer                                 
//                                                                                                    
// Purpose: Channel Mapping organization  for the Dummy Detector                     
//
// Last update:                                          
//                                                                
//----------------------------------------------------------------

#include "PHAddressObject.h"


class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHDummyAddressObject : public PHAddressObject {

public:

  enum  { ARM, MODULE, SUPERMODULE, BOARD,CHANNEL};
 
  PHDummyAddressObject(); 
  ~PHDummyAddressObject(); 

  virtual PdbIndex* getArm()         { return arm;}
  virtual PdbIndex* getModule()      { return module;}  
  virtual PdbIndex* getSuperModule() { return superModule;}
  virtual PdbIndex* getBoard()       { return board;}
  virtual PdbIndex* getChannel()     { return channel;}
  
  virtual PdbIndex* getDetectorIndex(int ind, PdbIndex* global);// given a globalIndex, get a local one 
  virtual PHBoolean setDetectorIndex(int ind, PdbIndex* global ); 
  virtual PHBoolean setGlobalIndex(PdbIndex *ind);// set all the indices corresponding to a GlobalIndex
  virtual PHBoolean setGlobalIndex(int ind);      // set all the indices corresponding to a GlobalIndex
  virtual void initialize();                 // to initialize the indices 


  virtual PHBoolean setSoft(int arm, int mod, int smod);
  virtual PHBoolean setHard(int arm, int boa, int chan);

  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID);
  virtual PHBoolean update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                 const char *name, PdbBankID bankID, char *descrip );

private:

  PHBoolean fromSoftToHard();
  PHBoolean fromHardToSoft();


 private:
 
  PdbIndex* arm;
  PdbIndex* module;
  PdbIndex* superModule;
  PdbIndex* board;
  PdbIndex* channel;
 
 
}; 

#endif /* PHDUMMYADDRESSOBJECT_H */ 
