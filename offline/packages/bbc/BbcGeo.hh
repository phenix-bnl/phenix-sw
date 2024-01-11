#ifndef PHENIX_BBCGEO_HH
#define PHENIX_BBCGEO_HH

#include "PHPoint.h"

class BbcGeo {
public:
  BbcGeo();
  ~BbcGeo(){} 
  double getX( int ipmt ) { return PmtPosition[ipmt].getX(); } 
  double getY( int ipmt ) { return PmtPosition[ipmt].getY(); } 
  double getZ( int ipmt ) { return PmtPosition[ipmt].getZ(); } 
private:
  PHPoint PmtPosition[128];

};

#endif 
