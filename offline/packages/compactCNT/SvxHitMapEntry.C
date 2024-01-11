#include "SvxHitMapEntry.h"
#include <iostream>

using namespace std;

SvxHitMapEntry::SvxHitMapEntry()
{
  id = -1;
  adcandsize = 0;
  x = -9999;
  y = -9999;
  z = -9999;
  return;
}

void SvxHitMapEntry::identify(ostream &os) const
{
  os << "SvxHitMapEntry: id: " << id
     //<< ", size: " << size
     << ", x,y,z: " << x << " " << y << " " << z 
     << endl;
  return;
}
