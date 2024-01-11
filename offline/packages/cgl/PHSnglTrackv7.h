#ifndef __PHSNGLTRACKV7_HH__
#define __PHSNGLTRACKV7_HH__

#include "PHSnglTrackv4.h"

// v7 adds hbd.  Inherit from v4, the base detector without mrpc (v5) and svx (v6).
// Only put hbd-specific code here and inherit the rest from v4.
class PHSnglTrackv7:public PHSnglTrackv4
{
 public:
  PHSnglTrackv7();
  virtual ~PHSnglTrackv7(){}

  void set_projectionHbd(const short d0, const float v){projectionHbd[d0] = v;}
  float get_projectionHbd(const short d0) const  {return projectionHbd[d0];}
  void set_directionHbd(const short d0,const float v)  {directionHbd[d0] = v;}
  float get_directionHbd(const short d0) const   {return directionHbd[d0];}

 protected:
  float projectionHbd[3];
  float directionHbd[3];

  ClassDef(PHSnglTrackv7,1) 

};
#endif
