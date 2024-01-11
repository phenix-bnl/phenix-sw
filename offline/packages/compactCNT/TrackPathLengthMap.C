#include "TrackPathLengthMap.h"
#include <iostream>

using namespace std;

void
TrackPathLengthMap::identify(ostream& os) const
{
  //  os << "identify yourself: TrackPathLengthMap Object" << endl;
  //  os << "entries: " << trackpathlengthmap.size() << endl;
  map<short int, TrackPathLengthMapEntry>::const_iterator iter;
  for (iter = trackpathlengthmap.begin(); iter != trackpathlengthmap.end(); iter++)
    {
      os << "Track: " << iter->first << endl;
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TrackPathLengthMap::AddPathLength(const short int index, const TrackPathLengthMapEntry &pathlength)
{
  trackpathlengthmap[index] = pathlength;
  return;
}

int
TrackPathLengthMap::isValid() const
{
  if (trackpathlengthmap.size() > 0)
    {
      return 1;
    }
  return 0;
}

void
TrackPathLengthMap::Reset()
{
  map<short int,TrackPathLengthMapEntry>::iterator iter;
  for (iter = trackpathlengthmap.begin(); iter != trackpathlengthmap.end(); iter++)
      {
	(iter->second).Reset();
      }
  trackpathlengthmap.clear();
  return;
}

TrackPathLengthMapEntry *
TrackPathLengthMap::GetTrack(const int index)
{
  map<short int,TrackPathLengthMapEntry>::iterator iter = trackpathlengthmap.find(index);
  if (iter != trackpathlengthmap.end())
    {
      return &iter->second;
    }
  return 0;
}
