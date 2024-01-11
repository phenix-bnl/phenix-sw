#include "SvxTrackMapEntry.h"
#include <iostream>
#include <string.h>

using namespace std;

SvxTrackMapEntry::SvxTrackMapEntry()
{
  charge = 0;
  quality = -9999.;
  dca2d = -9999.;
  dca3d = -9999.;
  x = -9999.;
  y = -9999.;
  z = -9999.;
  px = -9999.;
  py = -9999.;
  pz = -9999.;
  livePercentage[0] = -9999.;
  livePercentage[1] = -9999.;
  livePercentage[2] = -9999.;
  livePercentage[3] = -9999.;
  segmentQuality = -9999.;
  segmentScore   = -9999.;
  dedx[0] = dedx[1] = 0.0;
  memset(clusGoodFrac, 0, sizeof(clusGoodFrac));
  return;
}

void SvxTrackMapEntry::identify(ostream &os) const
{
  os << "SvxTrackMapEntry: charge: " << charge
     << ", quality: " << quality
     << ", dca: " << dca2d << " " << dca3d  
     << endl;
  return;
}

