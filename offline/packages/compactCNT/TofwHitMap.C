#include "TofwHitMap.h"
#include <iostream>

using namespace std;

void
TofwHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: TofwHitMap Object" << endl;
  //  os << "entries: " << tofwhitmap.size() << endl;
  map<short int, TofwHitMapEntry>::const_iterator iter;
  for (iter = tofwhitmap.begin(); iter != tofwhitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TofwHitMap::AddHit(const short int index, const TofwHitMapEntry &hit)
{
  tofwhitmap[index] = hit;
  return;
}

void
TofwHitMap::Reset()
{
  tofwhitmap.clear();
  return;
}

const TofwHitMapEntry *
TofwHitMap::GetHit(const short int index) const
{
  map<short int, TofwHitMapEntry>::const_iterator iter = tofwhitmap.find(index);
  if (iter != tofwhitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
