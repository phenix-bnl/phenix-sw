#include "TecHitMapEntry.h"
#include <iostream>

using namespace std;

TecHitMapEntry::TecHitMapEntry()
{
  id = -1;
  index = -1;
  wire = -9999;
  ntimebins = -9999;
  avgtime = -9999;
  charge = -999.9;
  return;
}

void
TecHitMapEntry::identify(ostream &os) const
{
  os << "TecHitMapEntry: id: " << id 
     << ", index: " << index
     << ", wire: " << wire
     << ", ntimebins: " << ntimebins
     << ", avgtime: " << avgtime
     << ", charge: " << charge
     << endl;
  return;
}
