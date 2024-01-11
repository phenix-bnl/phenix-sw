#include "CrkHitMapEntry.h"
#include <iostream>

ClassImp(CrkHitMapEntry)

using namespace std;

CrkHitMapEntry::CrkHitMapEntry()
{
  id = -1;
  pmtid = -1;
  npe = -9999;
  time = -9999;
  return;
}

void
CrkHitMapEntry::identify(ostream &os) const
{
  os << "CrkHitMapEntry: id: " << id
     << ", pmtid: " << pmtid
     << ", npe: " << npe
     << ", time: " << time
     << endl;
  return;
}
