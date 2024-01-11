#include "HbdHitMapEntry.h"

#include <cstring>
#include <iostream>

ClassImp(HbdHitMapEntry)

using namespace std;

HbdHitMapEntry::HbdHitMapEntry()
{
  adcch = -9998;
  charge = -9998;
  return;
}

void
HbdHitMapEntry::identify(ostream &os) const
{
  os << "HbdHitMapEntry: adcch: " << adcch 
     << ", charge: " << charge
     << endl;
  return;
}
