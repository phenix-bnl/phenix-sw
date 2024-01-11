#include "SvxCentralTrackMapEntry.h"
#include <iostream>

using namespace std;

SvxCentralTrackMapEntry::SvxCentralTrackMapEntry() :
  DchIndex(-1),
  HitPattern(0),
  Unique(0),
  DCA2D(32000),
  DCAZ(32000),
  ChisquarePhi(32000),
  ChisquareZ(32000),
  Chisquare(32000),
  Chisquare2(32000),
  NClusters(0),
  LinkQuality(-1),
  LinkScore(-1),
  DCA2Dprimary(32000),
  DCAZprimary(32000)
{

  for(int i=0; i<4; i++) {
    LivePercent[i] = -1;
  }

  for(int i=0; i<3; i++) {
    ClosestApproach[i] = 32000;
    Momentum[i]        = 32000;
  }

  for(int i=0; i<8; i++) {
    ClusterID[i] 	= -1;
    ClusterDPhi[i]  	= 32000;
    ClusterDZ[i] 	= 32000;
  }
  return;
}

void SvxCentralTrackMapEntry::identify(ostream &os) const
{
  os << "SvxCentralTrackMapEntry." << endl;
  return;
}


