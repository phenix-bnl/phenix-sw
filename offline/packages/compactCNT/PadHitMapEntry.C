#include "PadHitMapEntry.h"

#include <cstring>
#include <iostream>

ClassImp(PadHitMapEntry)

using namespace std;

PadHitMapEntry::PadHitMapEntry()
{
  id = -1;
  memset(xyz,0,sizeof(xyz));
  return;
}

void
PadHitMapEntry::identify(ostream &os) const
{
  os << "PadHitMapEntry: id: " << id 
     << ", x: " << xyz[0]
     << ", y: " << xyz[1]
     << ", z: " << xyz[2]
     << endl;
  return;
}
