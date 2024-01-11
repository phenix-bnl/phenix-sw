#include "TrackLineProjectionMap.h"
#include <iostream>

using namespace std;

void
TrackLineProjectionMap::identify(ostream& os) const
{
  //  os << "identify yourself: TrackLineProjectionMap Object" << endl;
  //  os << "entries: " << tracklineprojectionmap.size() << endl;
  map<short int, TrackLineProjectionMapEntry>::const_iterator iter;
  for (iter = tracklineprojectionmap.begin(); iter != tracklineprojectionmap.end(); iter++)
    {
      os << "TrackLine: " << iter->first << endl;
      //      os << "id : " << iter->first << endl;
      (iter->second).identify(os);
    }
  return;
}

void
TrackLineProjectionMap::AddProjection(const short int index, const TrackLineProjectionMapEntry &projection)
{
  tracklineprojectionmap[index] = projection;
  return;
}

int
TrackLineProjectionMap::isValid() const
{
  if (tracklineprojectionmap.size() > 0)
    {
      return 1;
    }
  return 0;
}

void
TrackLineProjectionMap::Reset()
{
  map<short int,TrackLineProjectionMapEntry>::iterator iter;
  for (iter = tracklineprojectionmap.begin(); iter != tracklineprojectionmap.end(); iter++)
      {
	(iter->second).Reset();
      }
  tracklineprojectionmap.clear();
  return;
}

TrackLineProjectionMapEntry *
TrackLineProjectionMap::GetTrackLine(const int index)
{
  map<short int,TrackLineProjectionMapEntry>::iterator iter = tracklineprojectionmap.find(index);
  if (iter != tracklineprojectionmap.end())
    {
      return &iter->second;
    }
  return 0;
}
