#ifndef __DCHRAWOUT_H__
#define __DCHRAWOUT_H__

#include "phool.h"
#include "PHObject.h"

class DchRawOut : public TObject {
public:
  DchRawOut();
  virtual ~DchRawOut(){}

  virtual int   getGlobal() { return -1; }
  virtual short getId()     { return -1; }
  virtual short getArm()    { return -1; }
  virtual short getSide()   { return -1; }
  virtual short getPlane()  { return -1; }
  virtual short getCell()   { return -1; }
  virtual short getEdge()   { return -1; }
  virtual long  getTime()   { return -1; }

  virtual void setGlobal( int val) {}
  virtual void setId(short val)    {}
  virtual void setArm(short val)   {}
  virtual void setSide(short val)  {}
  virtual void setPlane(short val) {}
  virtual void setCell(short val)  {}
  virtual void setEdge(short val)  {}
  virtual void setTime(long val)   {}

   ClassDef(DchRawOut,1) 
};

#endif /*__DCHRAWOUT_H__ */



