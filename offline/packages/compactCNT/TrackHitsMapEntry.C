#include "TrackHitsMapEntry.h"
#include <phool.h>

#include <iostream>
#include <cstdlib>

using namespace std;

static const short int NDETSMAX=20;

TrackHitsMapEntry::TrackHitsMapEntry()
{
  for(short int i=0;i<NDETSMAX;i++)
      alldets_array[i] = -9999;

}

void
TrackHitsMapEntry::identify(ostream &os) const
{
  for (short int i = 0;i < NDETSMAX; i++)
    {
      if(alldets_array[i] < -9000)
        continue;

      os << "id: " << i << " hitid: " << alldets_array[i] << endl;
    }

  return;
}

void
TrackHitsMapEntry::AddHit(const short int det_id, const short int hitid)
{
  if(det_id >= NDETSMAX)
    {
      cout << PHWHERE << "Maximum number of detectors exceeded -  Quit!" << endl;
      exit(1);
    }

  alldets_array[det_id] =  hitid;

  return;
}

short int
TrackHitsMapEntry::get_hitid(const short int det_id) const
{
  return alldets_array[det_id];
}

short int
TrackHitsMapEntry::getSize()
{
  return NDETSMAX;
}

void
TrackHitsMapEntry::Reset()
{
  for(int i=0;i<NDETSMAX;i++)
      alldets_array[i] = -9999;

  return;
}
