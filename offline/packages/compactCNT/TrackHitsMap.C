#include "TrackHitsMap.h"
#include <iostream>

using namespace std;

void
TrackHitsMap::identify(ostream& os) const
{
  //  os << "identify yourself: TrackHitsMap Object" << endl;
  //  os << "entries: " << trackhitsmap.size() << endl;
  map<short int, TrackHitsMapEntry>::const_iterator iter;
  for (iter = trackhitsmap.begin(); iter != trackhitsmap.end(); iter++)
    {
      os << "Track: " << iter->first << endl;
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TrackHitsMap::AddHits(const short int index, const TrackHitsMapEntry &hits)
{
  trackhitsmap[index] = hits;
  return;
}

int
TrackHitsMap::isValid() const
{
  if (trackhitsmap.size() > 0)
    {
      return 1;
    }
  return 0;
}

void
TrackHitsMap::Reset()
{
  map<short int,TrackHitsMapEntry>::iterator iter;
  for (iter = trackhitsmap.begin(); iter != trackhitsmap.end(); iter++)
      {
	(iter->second).Reset();
      }
  trackhitsmap.clear();
  return;
}

TrackHitsMapEntry *
TrackHitsMap::GetTrack(const int index)
{
  map<short int,TrackHitsMapEntry>::iterator iter = trackhitsmap.find(index);
  if (iter != trackhitsmap.end())
    {
      return &iter->second;
    }
  return 0;
}
