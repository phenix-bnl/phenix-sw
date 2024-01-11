#ifndef __DCHHITLINETABLE_HH
#define __DCHHITLINETABLE_HH

#include <iostream>
#include "PHObject.h"
#include "PHPoint.h"
#include "PHVector.h"
#include "TClonesArray.h"
#include "DchHitLineOut.hh"

class DchHitLineTable : public PHObject
{
 public:
  DchHitLineTable() {}
  virtual ~DchHitLineTable() { }

  virtual int   Expand(int){return -1;}
  virtual void  Clear(Option_t *option=""){}
  virtual void  Reset();
  virtual int isValid() const;

  virtual void identify (std::ostream& os = std::cout) const {
    os << "virtual DchHitLineTable object"  << std::endl; 
    return;
  } 
  virtual int Entries() { return -1;}
  virtual int Size() {return -1;}
  virtual int AddHit(DchHitLineOut* hit){return -1;}

  virtual DchHitLineOut* getHit(int i) {return 0;}
  virtual TClonesArray*  getHits() {return 0;}
  //--------------- Accessing members --------------

  virtual short getUsed(int i) { return -1;}
  virtual int   getId(int i)           { return -1;}
  virtual int   getIdmirror(int i)     { return -1;} 	   
  virtual short getArm(int i)        { return -1;}
  virtual short getPlane(int i)	     { return -1;}				
  virtual short getCell(int i)	     { return -1;}			
  virtual short getSide(int i)       { return -1;}		
  
  virtual short getAssociatedCandidate(int i) { return -1;}
  
  virtual float getDistance(int i) { return -999;}
  virtual float getWidth(int i)    { return -999;}
  virtual short getIdraw1(int i)   { return -1;}
  virtual short getIdraw2(int i)   { return -1;}
  virtual int getTime1(int i)      { return -999;}
  virtual int getTime2(int i)      { return -999;}
  virtual PHPoint getXYZ(int i)  {PHPoint  null;return null;}
  virtual PHPoint getEXYZ(int i) {PHPoint  null;return null;}
  virtual PHVector getVXYZ(int i){PHVector null;return null;}

  virtual void  setUsed(int i, short val)  {}
  virtual void  setId(int i, int val)      {}
  virtual void  setIdmirror(int i,int val) {} 	   
  virtual void  setArm(int i,  short val)  {}					
  virtual void  setPlane(int i,short val)  {}				
  virtual void  setCell(int i, short val)  {}			
  virtual void  setSide(int i, short val)  {}		
  
  virtual void  setAssociatedCandidate(int i, short val) {}
  
  virtual void  setDistance(int i, float val) {}
  virtual void  setWidth(int i, float val)    {}
  virtual void  setIdraw1(int i, short val)   {}
  virtual void  setIdraw2(int i, short val)   {}
  virtual void  setTime1(int i,int val)      {}
  virtual void  setTime2(int i,int val)      {}
  virtual void  setXYZ(int i,PHPoint val) {}
  virtual void  setEXYZ(int i, PHPoint val) {}
  virtual void  setVXYZ(int i, PHVector val) {}
  //--------------------------------
  virtual void  setStatus(int val){}
  virtual int   getStatus()       {return 0;}

 ClassDef(DchHitLineTable,1)

};

#endif

