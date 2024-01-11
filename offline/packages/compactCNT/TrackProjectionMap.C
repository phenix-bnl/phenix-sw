#include "TrackProjectionMap.h"
#include <iostream>

using namespace std;

void
TrackProjectionMap::identify(ostream& os) const
{
  //  os << "identify yourself: TrackProjectionMap Object" << endl;
  //  os << "entries: " << trackprojectionmap.size() << endl;
  map<short int, TrackProjectionMapEntry>::const_iterator iter;
  for (iter = trackprojectionmap.begin(); iter != trackprojectionmap.end(); iter++)
    {
      os << "Track: " << iter->first << endl;
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TrackProjectionMap::AddProjection(const short int index, const TrackProjectionMapEntry &projection)
{
  trackprojectionmap[index] = projection;
  return;
}

int
TrackProjectionMap::isValid() const
{
  if (trackprojectionmap.size() > 0)
    {
      return 1;
    }
  return 0;
}

void
TrackProjectionMap::Reset()
{
  map<short int,TrackProjectionMapEntry>::iterator iter;
  for (iter = trackprojectionmap.begin(); iter != trackprojectionmap.end(); iter++)
      {
	(iter->second).Reset();
      }
  trackprojectionmap.clear();
  return;
}

TrackProjectionMapEntry *
TrackProjectionMap::GetTrack(const int index)
{
  map<short int,TrackProjectionMapEntry>::iterator iter = trackprojectionmap.find(index);
  if (iter != trackprojectionmap.end())
    {
      return &iter->second;
    }
  return 0;
}
