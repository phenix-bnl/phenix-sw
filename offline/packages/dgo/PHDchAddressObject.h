#ifndef PHDCHADDRESSOBJECT_H
#define PHDCHADDRESSOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDchAddressObject.h                       
//                                                                   
// Created by: Federica Messer at Wed Feb 17 15:21:39 1999                                      
//                                                                                                    
// Purpose: Channel Mapping organization  for Drift Chamber                     
//
// Last update: Tue Dec 21 15:21:39 1999                                              
//                                                                
//----------------------------------------------------------------

#include "DchDgoPar.h"
#include "PHAddressObject.h"

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHDchAddressObject : public PHAddressObject {

public:

  enum  { ARM, SIDE, KEYSTONE, PLANE, CELL, PAIR, BOARD, BCHAN, CHANNEL};
 
  PHDchAddressObject(); 
  PHDchAddressObject(short); 
  virtual ~PHDchAddressObject(); 

  virtual PdbIndex* getArm()      {return arm;}
  virtual PdbIndex* getSide()     {return side;}
  virtual PdbIndex* getKeystone() {return key;}
  virtual PdbIndex* getPlane()    {return plane;}
  virtual PdbIndex* getCell()     {return cell;}
  virtual PdbIndex* getPair()     {return pair;}
  virtual PdbIndex* getBoard()    {return board;}
  virtual PdbIndex* getBChan()    {return bchan;}
  virtual PdbIndex* getChannel()  {return channel;}

  virtual PdbIndex* getDetectorIndex(int ind, PdbIndex* global);  // given a global index, get a local one 
  virtual PHBoolean setDetectorIndex(int ind, PdbIndex* global ); // to implement
  virtual PHBoolean setGlobalIndex(PdbIndex *ind);                // set all the indices corresponding to a GlobalIndex
  virtual PHBoolean setGlobalIndex(int ind);                      // set all the indices corresponding to a GlobalIndex
  virtual void initialize();                                 // to initialize the indices 

  short   getFlagMC() { return flagMC;}
  void    setFlagMC(short val) { flagMC = val;}
  virtual void setSoft(int arm, int side, int plane, int cell);
  virtual void setHard(int arm, int side, int key,  int pair, int channel);
  virtual void decodePacketID(int packetId);

  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID);
  virtual PHBoolean update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                 const char *name, PdbBankID bankID, const char *descrip );

  virtual PHBoolean updateValidityTimeForLastBank(PHTimeStamp&, PHTimeStamp&, PHTimeStamp&,
						  const char *, PdbBankID, int force = 0);


private:

  void fromSoftToHard();
  void fromHardToSoft();

 private:
 
  PdbIndex* arm;
  PdbIndex* side;
  PdbIndex* key;
  PdbIndex* plane;
  PdbIndex* cell;
  PdbIndex* pair;
  PdbIndex* board;
  PdbIndex* bchan;
  PdbIndex* channel;

  short flagMC;
 
}; 

#endif /* PHDCHADDRESSOBJECT_H */ 
