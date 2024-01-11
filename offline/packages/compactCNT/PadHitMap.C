#include "PadHitMap.h"

#include <iostream>

ClassImp(PadHitMap)

using namespace std;

void
PadHitMap::identify(ostream& os) const
{
  //  os << "identify yourself: PadHitMap Object" << endl;
  //  os << "entries: " << padhitmap.size() << endl;
  map<short int, PadHitMapEntry>::const_iterator iter;
  for (iter = padhitmap.begin(); iter != padhitmap.end(); iter++)
    {
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
PadHitMap::AddHit(const short int index, const PadHitMapEntry &hit)
{
  padhitmap[index] = hit;
  return;
}

void
PadHitMap::Reset()
{
  padhitmap.clear();
  return;
}

const PadHitMapEntry *
PadHitMap::GetHit(const short int index) const
{
  map<short int, PadHitMapEntry>::const_iterator iter = padhitmap.find(index);
  if (iter != padhitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}
