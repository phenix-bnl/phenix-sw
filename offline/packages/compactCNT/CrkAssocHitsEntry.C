#include "CrkAssocHitsEntry.h"
#include <iostream>

ClassImp(CrkAssocHitsEntry)

using namespace std;

CrkAssocHitsEntry::CrkAssocHitsEntry()
{
  for(int i=0;i<HITSMAX;i++)
    {
      pmtid[i] = -1;
      npe[i] = -9999;
      time[i] = -9999;
      rpmt[i] = -9999;
    }

  trackid = -1;
  npmts = 0;
  swapped = false;

  return;
}

void
CrkAssocHitsEntry::identify(ostream &os) const
{
  os << "CrkAssocHitsEntry: nhits: " <<  npmts  << endl;

  return;
}
