#include "TecHitMap.h"
#include <iostream>

using namespace std;

TecHitMap::TecHitMap()
{
  Nentries = 0;
}

void
TecHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: TecHitMap Object" << endl;
  //  os << "entries: " << techitmap.size() << endl;
  map<short int, TecHitMapEntry>::const_iterator iter;
  for (iter = techitmap.begin(); iter != techitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TecHitMap::AddHit(const short int index, const TecHitMapEntry &hit)
{
  techitmap[index] = hit;
  Nentries++;
  return;
}

int
TecHitMap::GetNentries()
{
  return Nentries;
}

void
TecHitMap::Reset()
{
  techitmap.clear();
  Nentries=0;
  return;
}

const TecHitMapEntry *
TecHitMap::GetHit(const short int index) const
{
  map<short int, TecHitMapEntry>::const_iterator iter = techitmap.find(index);
  if (iter != techitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
