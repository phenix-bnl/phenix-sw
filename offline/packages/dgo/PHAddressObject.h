#ifndef PHADDRESSOBJECT_H
#define PHADDRESSOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHAddressObject.h                       
//                                                                
// Created by: Federica Messer at Wed Feb 17 12:55:46 1999
//                                                                
// Purpose: Channel Mapping organization
//
// Last update: Tue Dec 21 12:55:46 1999                                                                                                      
//----------------------------------------------------------------

#include <phool.h>
#include <PdbIndex.hh>
#include <PHPointerList.h>
#include <PdbBankID.hh>
#include <PHTimeStamp.h>

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHAddressObject { 


public: 
 
  PHAddressObject();
  virtual ~PHAddressObject(); 
  
  const PHTimeStamp getStartValTime() const;
  const PHTimeStamp getEndValTime()   const;
  
  virtual PHBoolean  commit();
  virtual PHBoolean  validate(PHTimeStamp &Tsearch);
  virtual PHBoolean  fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID) =0 ;
  virtual PHBoolean  update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,const char *name, PdbBankID, const char *descrip ) =0 ;
  
  int                 getLength() const {return DetectorIndex.length();}       // number of indices
  PdbIndex*           getGlobalIndex() const {return index;}                   // return the global index
  PdbIndex*           getDetectorIndex(int ind) { return DetectorIndex[ind];}  // return a particular index

  virtual PdbIndex*   getDetectorIndex(int ind, PdbIndex* index) = 0;          // SubSystem dependent 
  virtual PHBoolean   setDetectorIndex(int ind, PdbIndex* value) = 0;          // SubSystem dependent 
  virtual PHBoolean   setGlobalIndex(PdbIndex* ind) = 0;                       // SubSystem dependent  
  virtual PHBoolean   setGlobalIndex(int ind) = 0;                             // SubSystem dependent  
  virtual void   initialize() = 0;                                        // SubSystem dependent                 

  virtual void        print();
 

protected:
  
  virtual const PdbIndex*  get(int value) const ; 
  virtual int              getBankLength() const;

 protected:
 
  short committed;
  PdbIndex* index;
  PHPointerList<PdbIndex> DetectorIndex;
  
  PdbBankManager  *bankManager;
  PdbApplication  *application;
  PdbCalBank      *addressBank;
  PHTimeStamp     start;
  PHTimeStamp     stop;
  
}; 


#endif /* PHADDRESSOBJECT_H */ 
