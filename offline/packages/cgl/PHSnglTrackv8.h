#ifndef __PHSNGLTRACKV8_HH__
#define __PHSNGLTRACKV8_HH__

#include "PHSnglTrackv7.h"

// v8 adds tofw.  Inherit from v7,
class PHSnglTrackv8:public PHSnglTrackv7
{
 public:
  PHSnglTrackv8();
  virtual ~PHSnglTrackv8(){}

  void set_projectionTofw(const short d0,const float v){projectionTofw[d0]= v;}
  float get_projectionTofw(const short d0) const {return projectionTofw[d0];}
  void set_directionTofw(const short d0,const float v) {directionTofw[d0] = v;}
  float get_directionTofw(const short d0) const  {return directionTofw[d0];}
  void set_tofwPathLength(const float v) {tofwPathLength = v;}
  float  get_tofwPathLength() const {return tofwPathLength;}

 protected:
  float projectionTofw[3];
  float directionTofw[3];
  float tofwPathLength;

  ClassDef(PHSnglTrackv8,1) 

};
#endif
