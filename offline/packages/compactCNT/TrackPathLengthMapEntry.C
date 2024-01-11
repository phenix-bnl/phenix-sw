#include "TrackPathLengthMapEntry.h"
#include <phool.h>

#include <cmath>
#include <iostream>

using namespace std;

void
TrackPathLengthMapEntry::identify(ostream &os) const
{
  map<short int, float >::const_iterator iter;
  for (iter = alldets.begin(); iter != alldets.end(); iter++)
    {
      os << "id: " << iter->first << ", pl: " << iter->second << endl;
    }
  return;
}

void
TrackPathLengthMapEntry::AddPathLength(const short int det_id, const float pl)
{
  if (alldets.find(det_id) != alldets.end())
    {
      cout << PHWHERE << " pathlength for id = " << det_id << " is overwritten" << endl;
    }
  alldets[det_id] = pl;
  return;
}

void
TrackPathLengthMapEntry::Reset()
{
  alldets.clear();
  return;
}

float
TrackPathLengthMapEntry::get_pathlength(const short int det_id) const
{
  map<short int, float >::const_iterator iter = alldets.find(det_id);
  if (iter != alldets.end())
    {
      return iter->second;
    }
  return NAN;
}
