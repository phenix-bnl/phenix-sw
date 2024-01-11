#include "TofwHitMapEntry.h"

#include <cstring>
#include <iostream>

using namespace std;

TofwHitMapEntry::TofwHitMapEntry()
{
  id = -1;
  memset(xyz,0,sizeof(xyz));
  stripid = -1;
  qtofw = -9999;
  ttofw = -9999;
  for (int i=0; i<2;i++)
    {
      adc[i] = -9999.;
      tdc[i] = -9999.;
    }
  return;
}

void
TofwHitMapEntry::identify(ostream &os) const
{
  os << "TofwHitMapEntry: id: " << id 
     << ", stripid: " << stripid
     << ", x: " << xyz[0]
     << ", y: " << xyz[1]
     << ", z: " << xyz[2]
     << ", ttofw: " << ttofw
     << ", qtofw: " << qtofw
     << endl;
  return;
}
