#include "SvxTrackMap.h"
#include <iostream>

using namespace std;

ClassImp(SvxTrackMap);

SvxTrackMap::SvxTrackMap()
{
  Nentries = 0;
}

void SvxTrackMap::identify(ostream& os) const
{
  map<short int, SvxTrackMapEntry>::const_iterator iter;
  for (iter = svxtrackmap.begin(); iter != svxtrackmap.end(); iter++)
    {
      (iter->second).identify(os);
    }
  return;
}

void SvxTrackMap::AddHit(const short int index, const SvxTrackMapEntry &hit)
{
  svxtrackmap[index] = hit;
  Nentries++;
  return;
}

int SvxTrackMap::GetNentries()
{
  return Nentries;
}

void SvxTrackMap::Reset()
{
  svxtrackmap.clear();
  Nentries = 0;
  return;
}

const SvxTrackMapEntry * SvxTrackMap::GetHit(const short int index) const
{
  map<short int, SvxTrackMapEntry>::const_iterator iter = svxtrackmap.find(index);
  if (iter != svxtrackmap.end())
    {
      return &(iter->second);
    }
  return 0;
}



