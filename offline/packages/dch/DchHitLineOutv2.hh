#ifndef __DCHHITLINEOUTV2_HH
#define __DCHHITLINEOUTV2_HH

#include "PHObject.h"
#include "PHPoint.h"
#include "PHVector.h"
#include "DchHitLineOut.hh"

class DchHitLineOutv2 : public  DchHitLineOut
{

public:
  enum{
    IDMIRRORMASK   = 3*2*2*40*80,    
    ARMMASK        = 2*2*40*80,
    SIDEMASK       = 2*40*80,
    PLANEMASK      = 40*80,
    CELLMASK       = 80,
    MAXUNSIGNED    = 65535,
    MAXDIGITIZE    = 0x7fff
 };

  DchHitLineOutv2() ;
  DchHitLineOutv2(DchHitLineOut*hit);
  virtual ~DchHitLineOutv2(){}
  void print();

  void setId(int val);
  void setIdmirror(int val);
  void setArm(short val);
  void setSide(short val);
  void setPlane(short val);
  void setCell(short val);
  void setWidth(float val) { width = static_cast<short> (val); }
  void setTime1(int val) { time1 = val; }
  void setXYZ(PHPoint val);

  int   getId();
  int   getIdmirror();
  short getArm();
  short getPlane();
  short getCell();
  short getSide();

  float getWidth() { return width;}
  int   getTime1() { return time1;}

  PHPoint getXYZ() ;
  float getX();
  float getY();
  float getZ();

  float          short2float(short val);
  short          float2short(float val);

protected:
  void Init();
  unsigned short id;
  unsigned short bitindex;
  short          width;
  short          time1;
  short          xyz_x,xyz_y,xyz_z;
  ClassDef(DchHitLineOutv2,1)
};

#endif /*__DCHHITLINEOUTV2_H*/

