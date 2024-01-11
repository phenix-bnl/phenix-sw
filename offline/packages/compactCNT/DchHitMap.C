#include "DchHitMap.h"
#include <iostream>

using namespace std;

void
DchHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: DchHitMap Object" << endl;
  //  os << "entries: " << dchhitmap.size() << endl;
  map<short int, DchHitMapEntry>::const_iterator iter;
  for (iter = dchhitmap.begin(); iter != dchhitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
DchHitMap::AddHit(const short int index, const DchHitMapEntry &hit)
{
  dchhitmap[index] = hit;
  return;
}

void
DchHitMap::Reset()
{
  dchhitmap.clear();
  return;
}

const DchHitMapEntry *
DchHitMap::GetHit(const short int index) const
{
  map<short int, DchHitMapEntry>::const_iterator iter = dchhitmap.find(index);
  if (iter != dchhitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
