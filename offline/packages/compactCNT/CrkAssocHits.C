#include "CrkAssocHits.h"
#include <iostream>

ClassImp(CrkAssocHits)

using namespace std;

CrkAssocHits::CrkAssocHits()
{
  Nentries = 0;
}
void
CrkAssocHits::identify(ostream& os) const
{
  //  os << "identify yourself: CrkAssocHits Object" << endl;
  //  os << "entries: " << crkhitmap.size() << endl;
  map<short int, CrkAssocHitsEntry>::const_iterator iter;
  for (iter = crkassochitmap.begin(); iter != crkassochitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
CrkAssocHits::AddHit(const short int index, const CrkAssocHitsEntry &hit)
{
  crkassochitmap[index] = hit;
  Nentries++;
  
  return;
}

int
CrkAssocHits::GetNentries()
{
  return Nentries;
}

void
CrkAssocHits::Reset()
{
  crkassochitmap.clear();
  Nentries = 0;
  return;
}

const CrkAssocHitsEntry *
CrkAssocHits::GetHit(const short int index) const
{
  map<short int, CrkAssocHitsEntry>::const_iterator iter = crkassochitmap.find(index);
  if (iter != crkassochitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
