#include "TrackProjectionMapEntry.h"
#include <phool.h>
#include <cmath>
#include <iostream>
#include <cstdlib>

using namespace std;

static const short int NDETSMAX=20;

TrackProjectionMapEntry::TrackProjectionMapEntry() 
{
  for(short int i=0;i<NDETSMAX;i++)
    for(int j=0;j<3;j++)
      alldets_array[i][j] = -9999.0;

}

void
TrackProjectionMapEntry::identify(ostream &os) const
{
  string coo[3] = {" x: ", ",y: ",",z: "};
  
  for (short int i = 0;i < NDETSMAX; i++)
    {
      if(alldets_array[i][0] < -9000.0)
	continue;

      os << "id: " << i << endl;
      for (int j=0; j<3;j++)
	{
	  os << coo[j] << alldets_array[i][j];
	}
     os << endl;
    }
  return;
}

void
TrackProjectionMapEntry::AddProjection(const short int det_id, float xyz[])
{
  if(det_id >= NDETSMAX)
    {
      cout << PHWHERE << "Maximum number of detectors exceeded -  Quit!" << endl;
      exit(1);
    }

  for(int j=0;j<3;j++)
    {
      alldets_array[det_id][j] =  xyz[j];
    }

  return;
}

void
TrackProjectionMapEntry::Reset()
{
  for(int i=0;i<NDETSMAX;i++)
    for(int j=0;j<3;j++)
      alldets_array[i][j] = -9999.0;

  return;
}

float
TrackProjectionMapEntry::get_projection(const short int det_id, const short int i) const
{
  return alldets_array[det_id][i];
}

short int
TrackProjectionMapEntry::getSize()
{
  return NDETSMAX;
}

float *
TrackProjectionMapEntry::getMap()
{
  return &alldets_array[0][0];
}
