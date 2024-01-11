#include "TofeHitMap.h"
#include <iostream>

using namespace std;

void
TofeHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: TofeHitMap Object" << endl;
  //  os << "entries: " << tofehitmap.size() << endl;
  map<short int, TofeHitMapEntry>::const_iterator iter;
  for (iter = tofehitmap.begin(); iter != tofehitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TofeHitMap::AddHit(const short int index, const TofeHitMapEntry &hit)
{
  tofehitmap[index] = hit;
  return;
}

void
TofeHitMap::Reset()
{
  tofehitmap.clear();
  return;
}

const TofeHitMapEntry *
TofeHitMap::GetHit(const short int index) const
{
  map<short int, TofeHitMapEntry>::const_iterator iter = tofehitmap.find(index);
  if (iter != tofehitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
