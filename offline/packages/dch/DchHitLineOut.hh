#ifndef DCHHITLINEOUT_H
#define DCHHITLINEOUT_H

#include <iostream>
#include "PHObject.h"
#include "PHPoint.h"
#include "PHVector.h"

// With the following we get the line number of the virtual function
// we called with PHWHERE 
#define DCH_VIRTUAL_WARNING std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl

class DchHitLineOut:public TObject
{
protected:
  virtual void Init() {}
public:
  DchHitLineOut(){}
  virtual ~DchHitLineOut(){}
  virtual void print(){DCH_VIRTUAL_WARNING;}
  virtual void setUsed(short val)   { DCH_VIRTUAL_WARNING;}
  virtual void setId(int val)       { DCH_VIRTUAL_WARNING;}
  virtual void setIdmirror(int val) { DCH_VIRTUAL_WARNING;}
  virtual void setArm(short val)    { DCH_VIRTUAL_WARNING;}
  virtual void setPlane(short val)  { DCH_VIRTUAL_WARNING;}
  virtual void setCell(short val)   { DCH_VIRTUAL_WARNING;}
  virtual void setSide(short val)   { DCH_VIRTUAL_WARNING;}
  virtual void setAssociatedCandidate(int val) { DCH_VIRTUAL_WARNING;}

  virtual void setDistance(float val) { DCH_VIRTUAL_WARNING; }
  virtual void setWidth(float val)    { DCH_VIRTUAL_WARNING; }
  virtual void setIdraw1(short val)   { DCH_VIRTUAL_WARNING; }
  virtual void setIdraw2(short val)   { DCH_VIRTUAL_WARNING; }
  virtual void setTime1(int val)      { DCH_VIRTUAL_WARNING; }
  virtual void setTime2(int val)      { DCH_VIRTUAL_WARNING; }
  virtual void setXYZ(PHPoint val)    { DCH_VIRTUAL_WARNING; }
  virtual void setEXYZ(PHPoint val)   { DCH_VIRTUAL_WARNING; } 
  virtual void setVXYZ(PHPoint val)   { DCH_VIRTUAL_WARNING; }

  virtual short getUsed() { DCH_VIRTUAL_WARNING;return -999;}
  virtual int   getId() { DCH_VIRTUAL_WARNING;return -1;}
  virtual int   getIdmirror() { DCH_VIRTUAL_WARNING;return -1;}
  virtual short getArm() { DCH_VIRTUAL_WARNING;return -1;}
  virtual short getPlane() { DCH_VIRTUAL_WARNING;return -1;}
  virtual short getCell() { DCH_VIRTUAL_WARNING;return -1;}
  virtual short getSide() { DCH_VIRTUAL_WARNING;return -1;}
  virtual int   getAssociatedCandidate() { DCH_VIRTUAL_WARNING;return -1;}
  virtual float getDistance() {DCH_VIRTUAL_WARNING;return -1;}
  virtual float getWidth() { DCH_VIRTUAL_WARNING;return -1;}
  virtual short getIdraw1() { DCH_VIRTUAL_WARNING;return -1;}
  virtual short getIdraw2() { DCH_VIRTUAL_WARNING;return -1;}
  virtual int   getTime1() { DCH_VIRTUAL_WARNING;return -1;}
  virtual int   getTime2() { DCH_VIRTUAL_WARNING;return -1;}
  virtual float getX(){ DCH_VIRTUAL_WARNING;return -999;}
  virtual float getY(){ DCH_VIRTUAL_WARNING;return -999;}
  virtual float getZ(){ DCH_VIRTUAL_WARNING;return -999;}
  virtual PHPoint getXYZ() { DCH_VIRTUAL_WARNING;
  PHPoint val(-999,-999,-999); return val;}
  virtual PHPoint getEXYZ() { DCH_VIRTUAL_WARNING;
  PHPoint val(-999,-999,-999);return val;}
  virtual PHVector getVXYZ() { DCH_VIRTUAL_WARNING;
  PHVector val(-999,-999,-999);return val;}
  ClassDef(DchHitLineOut,1)
};

#undef DCH_VIRTUAL_WARNING

#endif /*DCHHITLINEOUT_H*/

