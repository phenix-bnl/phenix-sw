#ifndef DCHRAWTABLE_H
#define DCHRAWTABLE_H

#include <iostream>
#include "PHObject.h"

class DchRawOutv1;
class TClonesArray;

class DchRawTable : public PHObject
{
 public:

  DchRawTable() {}
  virtual ~DchRawTable() { }

  virtual void identify (std::ostream& os = std::cout) const;
 
  virtual int Entries() { return -1;}
  virtual int Size() {return -1;}
  virtual TClonesArray* getRaws(){return 0;}
  virtual int AddRaw(DchRawOutv1* hit) {return 0;}
  virtual DchRawOutv1* getRaw(int i) {return 0;}
  //--------------- Accessing members --------------
 
  virtual int   getGlobal(int i)     { return -1;}
  virtual short getId(int i)         { return -1;}
  virtual short getArm(int i)        { return -1;}			       
  virtual short getPlane(int i)	     { return -1;}			       
  virtual short getCell(int i)	     { return -1;}			
  virtual short getSide(int i)       { return -1;}		
  virtual short getEdge(int i)       { return -1;}
  virtual int   getTime(int i)      { return -999;}

  virtual void  setGlobal(int i, int val)     {}
  virtual void  setId(int i, short val)         {}
  virtual void  setArm(int i, short val)        {}			       
  virtual void  setPlane(int i, short val)	     {}			       
  virtual void  setCell(int i,short val)	     {}			
  virtual void  setSide(int i, short val)       {}		
  virtual void  setEdge(int i,short val)       {}
  virtual void  setTime(int i, int val)      {}
 //--------------------------------

  ClassDef(DchRawTable,1)
};
#endif

