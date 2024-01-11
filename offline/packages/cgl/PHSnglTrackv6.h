#ifndef __PHSNGLTRACKV6_HH__
#define __PHSNGLTRACKV6_HH__

#include "PHSnglTrackv5.h"

#define SVXLAYERNUMBER 4

class PHSnglTrackv6:public PHSnglTrackv5
{
 public:
  PHSnglTrackv6();
  virtual ~PHSnglTrackv6(){}

  void set_projectionSvx(const short ilayer, const short d0, const float v){projectionSvx[ilayer][d0] = v;}
  float get_projectionSvx(const short ilayer, const short d0) const  {return projectionSvx[ilayer][d0];}
  void set_directionSvx(const short ilayer, const short d0,const float v)  {directionSvx[ilayer][d0] = v;}
  float get_directionSvx(const short ilayer, const short d0) const   {return directionSvx[ilayer][d0];}

 protected:
  float projectionSvx[SVXLAYERNUMBER][3];
  float directionSvx[SVXLAYERNUMBER][3];

  ClassDef(PHSnglTrackv6,1) 

};
#endif
