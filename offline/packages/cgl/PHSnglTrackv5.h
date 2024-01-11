#ifndef __PHSNGLTRACKV5_HH__
#define __PHSNGLTRACKV5_HH__

#include "PHSnglTrackv4.h"

class PHSnglTrackv5: public PHSnglTrackv4
{
 public:
  PHSnglTrackv5();
  virtual ~PHSnglTrackv5(){}

  void set_projectionMrpc(const short d0,const float v){projectionMrpc[d0]= v;}
  float get_projectionMrpc(const short d0) const {return projectionMrpc[d0];}
  void set_directionMrpc(const short d0,const float v) {directionMrpc[d0] = v;}
  float get_directionMrpc(const short d0) const  {return directionMrpc[d0];}
  void set_mrpcPathLength(const float v) {mrpcPathLength = v;}
  float  get_mrpcPathLength() const {return mrpcPathLength;}

 protected:
  float projectionMrpc[3];
  float directionMrpc[3];
  float mrpcPathLength;

  ClassDef(PHSnglTrackv5,1) 

};
#endif
