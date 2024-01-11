
#ifndef PHCALIBRATIONOBJECT_H
#define PHCALIBRATIONOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHCalibrationObject.h                       
//                                                                
// Created by: Federica Ceretto at Wed Feb 17 12:55:46 1999                                      
//                                                                
// Purpose: Retrieve (and write) Calibration info from the Database
//                                                                
// Last update: Fri Juli 9 12:55:46 1999                                              
//                                                                
//----------------------------------------------------------------

#include "PHAddressObject.h"
class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PdbParameter;

class PHCalibrationObject { 
public: 

  PHCalibrationObject(PHAddressObject *add);   
  virtual ~PHCalibrationObject();

  virtual PHAddressObject* getAddress() const { return address;}
  virtual void print();

  const PHTimeStamp getStartValTime() const;
  const PHTimeStamp getEndValTime() const;

  virtual PHBoolean commit();
  virtual PHBoolean validate(PHTimeStamp &Tsearch); 
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID) = 0;  
  virtual PHBoolean update(PHTimeStamp &Tstart, PHTimeStamp &Tstop,const char *calibname, PdbBankID, const char *descrip ) =0 ;   


protected:  
  virtual const PdbParameter*  get(int value) const ;
  virtual int                  getBankLength() const ;
    
protected: 

  short committed;
  PHAddressObject *address;  
  
  PdbBankManager *bankManager ;
  PdbApplication *application;
  PdbCalBank     *calibrationBank;   
  PHTimeStamp    start, stop;
  
}; 


#endif /* PHCALIBRATIONOBJECT_H */ 
