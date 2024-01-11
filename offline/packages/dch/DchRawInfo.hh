#ifndef __DCHRAWINFO_H__
#define __DCHRAWINFO_H__

#include "phool.h"

class DchRawInfo {
public:
  DchRawInfo();
  virtual ~DchRawInfo();

  int   getGlobal() { return global;}
  short getId()    { return id; }
  short getArm()   { return arm; }
  short getSide()  { return side; }
  short getPlane() { return plane; }
  short getCell()  { return cell; }
  short getEdge()  { return edge;}
  long  getTime()  { return time;}

  void setGlobal( int val) { global = val;}
  void setId(short val)    { id = val;}
  void setArm(short val)   { arm = val;}
  void setSide(short val)  { side = val;}
  void setPlane(short val) { plane = val;}
  void setCell(short val)  { cell = val;}
  void setEdge(short val)  { edge = val;}
  void setTime(long val)   { time = val;}

private:
  int global;
  short id;
  short arm;
  short plane;
  short cell;
  short side;
  short edge;  // 0=leading edge, 1=trailing edge 
  long  time;  // TDC value of the leading or trailing edge 
};

#endif /*__DCHHITINFO_H__ */



