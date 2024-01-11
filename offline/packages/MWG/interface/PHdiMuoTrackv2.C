#include "PHdiMuoTrackv2.h"

#include <phool.h>

#include <cstdlib>

ClassImp(PHdiMuoTrackv2)

  PHdiMuoTrackv2::PHdiMuoTrackv2()
    :mass(0),charge(0),px(0),py(0),pz(0)
{
  for (int i=0; i<2; i++) trkIndex[i]=-1;
}

void PHdiMuoTrackv2::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors
int PHdiMuoTrackv2::get_trkIndex(short arrayid) const
{
  return (0<=arrayid && arrayid<2) ? trkIndex[arrayid] : -1;
}

//===== Mutators
void PHdiMuoTrackv2::set_trkIndex(short arrayid, int newVal)
{
  if (arrayid<0 || arrayid>=2)
    {
      std::cout<<PHWHERE<<" set_trkIncex id out of range. \n";
      exit(1);
    }
  trkIndex[arrayid] = newVal;
}


