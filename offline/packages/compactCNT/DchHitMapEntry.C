#include "DchHitMapEntry.h"
#include <iostream>

using namespace std;

DchHitMapEntry::DchHitMapEntry()
{
  id = -1;
  arm = -1;
  side = -1;
  quality = 0;
  nx1 = -1;
  nx2 = -1;

  zed = -9999.;
  phi = -9999.;
  alpha = -9999.;
  beta = -9999.;
  phi0 = -9999.;
  theta0 = -9999.;
  momentum = -9999.;
  return;
}

void
DchHitMapEntry::identify(ostream &os) const
{
  os << "DchHitMapEntry: id: " << id 
     << ", arm: " << arm
     << ", side: " << side
     << ", quality: " << quality
     << ", nx1: " << nx1
     << ", nx2: " << nx2
     << endl;
  return;
}
