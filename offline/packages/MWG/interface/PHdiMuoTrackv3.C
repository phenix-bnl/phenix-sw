#include "PHdiMuoTrackv3.h"

#include <phool.h>

#include <cstdlib>

ClassImp(PHdiMuoTrackv3)

  PHdiMuoTrackv3::PHdiMuoTrackv3()
    :mass(0),charge(0),px(0),py(0),pz(0),vtx_chisquare(0)
{
  for (int i=0; i<2; i++) trkIndex[i]=-1;
  for (int i=0; i<9; i++) for (int j=0; j<9; j++) vtx_cov[i][j]=0;
}

void PHdiMuoTrackv3::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors

int PHdiMuoTrackv3::get_trkIndex(short arrayid) const
{
  return (0<=arrayid && arrayid<2) ? trkIndex[arrayid] : -1;
}
float PHdiMuoTrackv3::get_vtx_cov(short arrayid1, short arrayid2) const
{
  return (0<=arrayid1 && arrayid1<9 && 
	  0<=arrayid2 && arrayid2<9 ) ? vtx_cov[arrayid1][arrayid2] : 0;
}

//===== Mutators

void PHdiMuoTrackv3::set_trkIndex(short arrayid, int newVal)
{
  if (arrayid<0 || arrayid>=2)
    {
      std::cout<<PHWHERE<<" set_trkIncex id out of range. \n";
      exit(1);
    }
  trkIndex[arrayid] = newVal;
}


void PHdiMuoTrackv3::set_vtx_cov(short arrayid1, short arrayid2, float newVal)
{
  if (arrayid1<0 || arrayid1>=9 ||
      arrayid2<0 || arrayid2>=9)
    {
      std::cout<<PHWHERE<<" set_vtx_cov id out of range. \n";
      exit(1);
    }
  vtx_cov[arrayid1][arrayid2] = newVal;
}






