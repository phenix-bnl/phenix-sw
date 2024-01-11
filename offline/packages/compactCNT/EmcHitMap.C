#include "EmcHitMap.h"
#include <iostream>

using namespace std;

void
EmcHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: EmcHitMap Object" << endl;
  //  os << "entries: " << emchitmap.size() << endl;
  map<short int, EmcHitMapEntry>::const_iterator iter;
  for (iter = emchitmap.begin(); iter != emchitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
EmcHitMap::AddHit(const short int index, const EmcHitMapEntry &hit)
{
  emchitmap[index] = hit;
  return;
}

void
EmcHitMap::Reset()
{
  emchitmap.clear();
  return;
}

const EmcHitMapEntry *
EmcHitMap::GetHit(const short int index) const
{
  map<short int, EmcHitMapEntry>::const_iterator iter = emchitmap.find(index);
  if (iter != emchitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
