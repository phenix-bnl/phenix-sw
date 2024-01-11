#include "TofeHitMapEntry.h"

#include <cstring>
#include <iostream>

using namespace std;

TofeHitMapEntry::TofeHitMapEntry()
{
  id = -1;
  memset(xyz,0,sizeof(xyz));
  slatid = -1;
  etof = -9999;
  ttof = -9999;
  return;
}

void
TofeHitMapEntry::identify(ostream &os) const
{
  os << "TofeHitMapEntry: id: " << id 
     << ", x: " << xyz[0]
     << ", y: " << xyz[1]
     << ", z: " << xyz[2]
     << ", etof: " << etof
     << ", ttof: " << ttof
     << endl;
  return;
}
