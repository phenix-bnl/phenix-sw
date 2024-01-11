#include "SvxCentralTrackMap.h"
#include <iostream>

using namespace std;

ClassImp(SvxCentralTrackMap);

SvxCentralTrackMap::SvxCentralTrackMap()
{
  Nentries = 0;
}

void SvxCentralTrackMap::identify(ostream& os) const
{
  map<short int, SvxCentralTrackMapEntry>::const_iterator iter;
  for (iter = svxtrackmap.begin(); iter != svxtrackmap.end(); iter++)
    {
      (iter->second).identify(os);
    }
  return;
}

void SvxCentralTrackMap::AddHit(const short int index, const SvxCentralTrackMapEntry &hit)
{
  svxtrackmap[index] = hit;
  Nentries++;
  return;
}

int SvxCentralTrackMap::GetNentries()
{
  return Nentries;
}

void SvxCentralTrackMap::Reset()
{
  svxtrackmap.clear();
  Nentries = 0;
  return;
}

const SvxCentralTrackMapEntry * SvxCentralTrackMap::GetHit(const short int index) const
{
  map<short int, SvxCentralTrackMapEntry>::const_iterator iter = svxtrackmap.find(index);
  if (iter != svxtrackmap.end())
    {
      return &(iter->second);
    }
  return 0;
}



