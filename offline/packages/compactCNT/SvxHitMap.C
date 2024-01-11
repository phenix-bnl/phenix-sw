#include "SvxHitMap.h"
#include <iostream>

using namespace std;

ClassImp(SvxHitMap);

SvxHitMap::SvxHitMap()
{
  Nentries = 0;
}

void SvxHitMap::identify(ostream& os) const
{
  map<short int, SvxHitMapEntry>::const_iterator iter;
  for (iter = svxhitmap.begin(); iter != svxhitmap.end(); iter++)
    {
      (iter->second).identify(os);
    }
  return;
}

void SvxHitMap::AddHit(const short int index, const SvxHitMapEntry &hit)
{
  svxhitmap[index] = hit;
  Nentries++;
  return;
}

int SvxHitMap::GetNentries()
{
  return Nentries;
}

void SvxHitMap::Reset()
{
  svxhitmap.clear();
  Nentries = 0;
  return;
}

const SvxHitMapEntry * SvxHitMap::GetHit(const short int index) const
{
  map<short int, SvxHitMapEntry>::const_iterator iter = svxhitmap.find(index);
  if (iter != svxhitmap.end())
    {
      return &(iter->second);
    }
  return 0;
}



