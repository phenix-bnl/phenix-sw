#include "PHdiMuoTrackv1.h"
#include "phool.h"

#include <cstdlib>

ClassImp(PHdiMuoTrackv1)

  PHdiMuoTrackv1::PHdiMuoTrackv1()
    :mass(0),charge(0)
{
  for (int i=0; i<_maxpoints; i++){
    px[i]=0; py[i]=0; pz[i]=0; 
  }
  for (int i=0; i<2; i++) trkIndex[i]=-1;
}

void PHdiMuoTrackv1::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors
float PHdiMuoTrackv1::get_px(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? px[arrayid] : 0;
}
float PHdiMuoTrackv1::get_py(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? py[arrayid] : 0;
}
float PHdiMuoTrackv1::get_pz(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? pz[arrayid] : 0;
}
int PHdiMuoTrackv1::get_trkIndex(short arrayid) const
{
  return (0<=arrayid && arrayid<2) ? trkIndex[arrayid] : -1;
}

//===== Mutators
void PHdiMuoTrackv1::set_px(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" set_px id out of range.\n";
      exit(1);
    }
  px[arrayid] = newVal;
}
void PHdiMuoTrackv1::set_py(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" set_py id out of range.\n";
      exit(1);
    }
  py[arrayid] = newVal;
}
void PHdiMuoTrackv1::set_pz(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" set_pz id out of range.\n";
      exit(1);
    }
  pz[arrayid] = newVal;
}
void PHdiMuoTrackv1::set_trkIndex(short arrayid, int newVal)
{
  if (arrayid<0 || arrayid>=2)
    {
      std::cout<<PHWHERE<<" set_trkIncex id out of range. \n";
      exit(1);
    }
  trkIndex[arrayid] = newVal;
}


