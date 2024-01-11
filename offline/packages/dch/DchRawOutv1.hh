#ifndef __DCHRAWOUTv1_H__
#define __DCHRAWOUTv1_H__

#include "TObject.h"

class DchRawOutv1 : public TObject {
protected:
  void Init();
public:
  DchRawOutv1();
  DchRawOutv1(DchRawOutv1*raw);
  virtual ~DchRawOutv1();

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

protected:
  int global;
  short id;
  short arm;
  short plane;
  short cell;
  short side;
  short edge;  // 0=leading edge, 1=trailing edge 
  long  time;  // TDC value of the leading or trailing edge 
  ClassDef(DchRawOutv1,1)
};

#endif /*__DCHRAWOUTv1_H__ */



