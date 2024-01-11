#include "AccHitMap.h"
#include <iostream>

ClassImp(AccHitMap)

using namespace std;

AccHitMap::AccHitMap()
{
  Nentries = 0;
}
void
AccHitMap::identify(ostream& os) const
{
  os << "identify yourself: AccHitMap Object" << endl;
  os << "entries: " << acchitmap.size() << endl;
  map<short int, AccHitMapEntry>::const_iterator iter;
  for (iter = acchitmap.begin(); iter != acchitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
AccHitMap::AddHit(const short int index, const AccHitMapEntry &hit)
{
  acchitmap[index] = hit;
  Nentries++;
  return;
}

int
AccHitMap::GetNentries()
{
  return Nentries;
}

void
AccHitMap::Reset()
{
  acchitmap.clear();
  Nentries = 0;
  return;
}

const AccHitMapEntry *
AccHitMap::GetHit(const short int index) const
{
  map<short int, AccHitMapEntry>::const_iterator iter = acchitmap.find(index);
  if (iter != acchitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
