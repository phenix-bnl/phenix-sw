#include "CrkHitMap.h"
#include <iostream>

ClassImp(CrkHitMap)

using namespace std;

CrkHitMap::CrkHitMap()
{
  Nentries = 0;
}
void
CrkHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: CrkHitMap Object" << endl;
  //  os << "entries: " << crkhitmap.size() << endl;
  map<short int, CrkHitMapEntry>::const_iterator iter;
  for (iter = crkhitmap.begin(); iter != crkhitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
CrkHitMap::AddHit(const short int index, const CrkHitMapEntry &hit)
{
  crkhitmap[index] = hit;
  Nentries++;
  return;
}

int
CrkHitMap::GetNentries()
{
  return Nentries;
}

void
CrkHitMap::Reset()
{
  crkhitmap.clear();
  Nentries = 0;
  return;
}

const CrkHitMapEntry *
CrkHitMap::GetHit(const short int index) const
{
  map<short int, CrkHitMapEntry>::const_iterator iter = crkhitmap.find(index);
  if (iter != crkhitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
