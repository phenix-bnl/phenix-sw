#include "TrackLineProjectionMapEntry.h"
#include <phool.h>

#include <cmath>
#include <iostream>

using namespace std;

void
TrackLineProjectionMapEntry::identify(ostream &os) const
{
  //  os << "TrackLineProjectionMapEntry: no of projections: " << alldets.size();
  string coo[6] = {",x1: ", ",y1: ",",z1: ",",x2: ",",y2: ",",z2: "};
  map<short int, std::vector<float> >::const_iterator iter;
  for (iter = alldets.begin(); iter != alldets.end(); iter++)
    {
      os << "id: " << iter->first << endl;
      for (int i=0; i<6;i++)
	{
	  os << coo[i] << (iter->second)[i];
	}
//      << ", x: " << xyz[0]
//      << ", y: " << xyz[1]
//      << ", z: " << xyz[2]
     os << endl;
    }
  return;
}

void
TrackLineProjectionMapEntry::AddProjection(const short int det_id, vector<float> &fvec)
{
  if (alldets.find(det_id) != alldets.end())
    {
      cout << PHWHERE << " projections for id = " << det_id << " are overwritten" << endl;
    }
  alldets[det_id] = fvec;
  return;
}

void
TrackLineProjectionMapEntry::Reset()
{
  alldets.clear();
  return;
}

float
TrackLineProjectionMapEntry::get_projection(const short int det_id, const short int i) const
{
  map<short int, vector<float> >::const_iterator iter = alldets.find(det_id);
  if (iter != alldets.end())
    {
      return (iter->second)[i];
    }
  return NAN;
}
