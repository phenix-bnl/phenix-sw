#include "PHMuoTrackv1.h"
#include <phool.h>

#include <cstdlib>

ClassImp(PHMuoTrackv1)

PHMuoTrackv1::PHMuoTrackv1()
:charge(0),PID(0),MuonConfidence(0),
  PionConfidence(0),chisquare(0),ghostflag(0)
{
  for (int i=0; i<_maxpoints; i++){
    px[i]=0; py[i]=0; pz[i]=0; xpos[i]=0; ypos[i]=0; zpos[i]=0;
  }
  for (int i=0; i<3; i++) st1_bp_P[i]=0;
  for (int i=0; i<_covdim; i++) 
    for (int j=0; j<_covdim; j++) cov[i][j]=0;
}

void PHMuoTrackv1::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors
float PHMuoTrackv1::get_px(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? px[arrayid] : 0;
}
float PHMuoTrackv1::get_py(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? py[arrayid] : 0;
}
float PHMuoTrackv1::get_pz(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? pz[arrayid] : 0;
}
float PHMuoTrackv1::get_st1_bp_P(short arrayid) const
{
  return (0<=arrayid && arrayid<3) ? st1_bp_P[arrayid] : 0;
}
float PHMuoTrackv1::get_xpos(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? xpos[arrayid] : 0;
}
float PHMuoTrackv1::get_ypos(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? ypos[arrayid] : 0;
}
float PHMuoTrackv1::get_zpos(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? zpos[arrayid] : 0;
}
float PHMuoTrackv1::get_cov(short arrayid1, short arrayid2) const 
{
  return (0<=arrayid1 && arrayid1<_covdim && 
	  0<=arrayid2 && arrayid2<_covdim) ?  
    cov[arrayid1][arrayid2] : 0.0;
}

//===== Mutators
void PHMuoTrackv1::set_px(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_px id out of range.\n";
      exit(1);
    }
  px[arrayid] = newVal;
}
void PHMuoTrackv1::set_py(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_py id out of range.\n";
      exit(1);
    }
  py[arrayid] = newVal;
}
void PHMuoTrackv1::set_pz(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_pz id out of range.\n";
      exit(1);
    }
  pz[arrayid] = newVal;
}
void PHMuoTrackv1::set_st1_bp_P(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_P id out of range.\n";
      exit(1);
    }
  st1_bp_P[arrayid] = newVal;
}
void PHMuoTrackv1::set_xpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_xpos id out of range.\n";
      exit(1);
    }
  xpos[arrayid] = newVal;
}
void PHMuoTrackv1::set_ypos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_ypos id out of range.\n";
      exit(1);
    }
  ypos[arrayid] = newVal;
}
void PHMuoTrackv1::set_zpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_zpos id out of range.\n";
      exit(1);
    }
  zpos[arrayid] = newVal;
}
void PHMuoTrackv1::set_cov(short arrayid1, short arrayid2, float newVal)
{
  if(arrayid1<0 || arrayid1 >= _covdim || 
     arrayid2<0 || arrayid2 >= _covdim)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv1::set_cov id out of range. \n";
      exit(1);
    }
  cov[arrayid1][arrayid2] = newVal;
}
