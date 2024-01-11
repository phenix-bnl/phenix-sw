#ifndef PHGEOMETRYOBJECT_H
#define PHGEOMETRYOBJECT_H
 
// Implementation of class file: PHGeometryObject.h                      
// Created by: Federica Messer at Tue Feb 16 13:11:37 1999
// Purpose: Retrieve (and write) Geometry info from the DB

#include <phool.h>
#include <PHTimeStamp.h>
#include <PHString.h>
#include <PHFrame.h>
#include <PdbBankID.hh>

class PdbCalChan;
class PHAddressObject;
class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHGeometryObject 
{ 
public: 
  PHGeometryObject();
  PHGeometryObject(PHAddressObject *add);  
  virtual ~PHGeometryObject();
 

  virtual PHAddressObject* getAddress() const { return address;}
  virtual void print();

  
  const PHTimeStamp getStartValTime() const;
  const PHTimeStamp getEndValTime()   const;

  virtual PHBoolean commit();
  virtual PHBoolean validate(PHTimeStamp &Tsearch); 
  virtual PHBoolean fetch(PHTimeStamp &Tsearch,
			  const char *calibname,
			  PdbBankID) = 0;  

  virtual PHBoolean update(PHTimeStamp &Tstart,
			   PHTimeStamp &Tstop,
			   const char *calibname,
			   PdbBankID,
			   const char *descrip ) = 0;
  
  virtual PHBoolean rotateAndTranslate(PHTimeStamp &Tsearch,
				       const char *calibname,
				       PdbBankID) = 0;

  virtual PHBoolean rotateAndTranslate(PHFrame initialE,
				       PHFrame finalE,
				       PHFrame initialW,
				       PHFrame finalW) = 0;


protected:  
  virtual const PdbCalChan* get(int index) const;
  virtual int  getBankLength();
  
protected: 
  short committed;
  PHAddressObject *address;
  
  PdbBankManager *bankManager;
  PdbApplication *application;
  PdbCalBank     *geometryBank;   
  PHTimeStamp    start;
  PHTimeStamp    stop;
  
}; 




#endif /* PHGEOMETRYOBJECT_H */ 
