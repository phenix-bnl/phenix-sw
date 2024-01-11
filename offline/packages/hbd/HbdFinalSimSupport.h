#ifndef __HBDFINALSIMSUPPORT_H__
#define __HBDFINALSIMSUPPORT_H__
#include <iostream>

class HbdModule
{
 public:
   HbdModule(float X, float Y, float Angle, int Sect, int Side);
   virtual ~HbdModule();
   float DistanceFromPoint(float x,float y, float z);
   int sect;
   int side;
   float angle;
   float width;
   float height;
   float vertex_x;
   float vertex_y;
   float vertex_z;
};

class HbdFinalSimSupport
{
 public:
   HbdFinalSimSupport();
   virtual ~HbdFinalSimSupport();
   int Init();
   float get_pad_center(const int i, const int k) {return pad_center[i][k];}
   float * loc2glob(float X,float Y, int Sect, int Side);
   float * glob2loc(float X,float Y, float Z);
   float DistanceFromPoint(float x1,float y1,float x2, float y2,float x0, float y0);
   int FindPad(float X,float Y, int Sect, int Side);
   float pad_center[2304][2];
   short pad_type[192];

 protected:
   float apx[12],apy[12],apa[12];
   HbdModule * hbdm[12][2];
   short nvertex;
   float xn[7];
   float yn[7];
   float local[4];
   float global[3];
};

#endif
