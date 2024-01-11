#include "PHMuoTrackv9.h"

#include <phool.h>

#include <cmath>
#include <cstdlib>

ClassImp(PHMuoTrackv9)

PHMuoTrackv9::PHMuoTrackv9()
:charge(0),PID(0),MuonConfidence(0),
  PionConfidence(0), ndf( 0 ), chisquare( 0 ),ghostflag(0),
  muTRhits(0),muIDhits(0), TMutTrk_status(0)
{
  for (int i=0; i<_maxpoints; i++) {
    px[i]=0; 
    py[i]=0; 
    pz[i]=0; 
    xpos[i]=0; 
    ypos[i]=0; 
    zpos[i]=0;
  }
  
  for (int i=0; i<3; i++) st1_bp_P[i]=0;
  for (int i=0; i<3; i++) st1_bp_pos[i]=0;
  
  for (int i=0; i<_covdim; i++) 
    for (int j=0; j<_covdim; j++) cov[i][j]=0;
  
  for(int i = 0; i < _maxroads; i++) muIDOOhits[i] = 0;
  for(int i = 0; i < _maxroads; i++) muIDOOchi[i] = 0;
  
  for (int i= 0; i < 5; i++)
    for(int j = 0; j < _maxroads; j++)
      muIDOO_gap0[i][j] = 0;

  for (int i= 0; i < 5; i++) muID_gap0[i] = 0;

  for (int ipa=0; ipa<5; ipa++) 
    for (int ior=0; ior<2; ior++) 
      for (int ihit=0; ihit<_max_mui_hits; ihit++) {
	muID_proj_hit_dist[ipa][ior][ihit] = -9999;
	muID_proj_hit_size[ipa][ior][ihit] = -1;
      }
  
  for( unsigned int i_coord = 0; i_coord < _max_gap_coords; i_coord++ )
    {
      _delta_q[ i_coord ] = -9999;
      _delta_q_error[ i_coord ] = -1;
    }
  
}

void PHMuoTrackv9::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors
float PHMuoTrackv9::get_px(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? px[arrayid] : 0;
}
float PHMuoTrackv9::get_py(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? py[arrayid] : 0;
}
float PHMuoTrackv9::get_pz(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? pz[arrayid] : 0;
}
float PHMuoTrackv9::get_st1_bp_P(short arrayid) const
{
  return (0<=arrayid && arrayid<3) ? st1_bp_P[arrayid] : 0;
}
float PHMuoTrackv9::get_st1_bp_pos(short arrayid) const
{
  return (0<=arrayid && arrayid<3) ? st1_bp_pos[arrayid] : 0;
}
float PHMuoTrackv9::get_xpos(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? xpos[arrayid] : 0;
}
float PHMuoTrackv9::get_ypos(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? ypos[arrayid] : 0;
}
float PHMuoTrackv9::get_zpos(short arrayid) const
{
  return (0<=arrayid && arrayid<_maxpoints) ? zpos[arrayid] : 0;
}
float PHMuoTrackv9::get_cov(short arrayid1, short arrayid2) const 
{
  return (0<=arrayid1 && arrayid1<_covdim && 
	  0<=arrayid2 && arrayid2<_covdim) ?  
    cov[arrayid1][arrayid2] : 0.0;
}

float PHMuoTrackv9::get_muIDOO_gap0(const short arrayid, const short iroad) const
{
  return ((0<=arrayid && arrayid<5)&&(0<=iroad && iroad < _maxroads)) ? muIDOO_gap0[arrayid][iroad] : 0;
}
float PHMuoTrackv9::get_muID_proj_hit_dist(short gap, short orient, short hit) const
{
  return ((0<=gap && gap<5) && (0<=orient && orient<2) && (0<=hit && hit<_max_mui_hits)) ?
    muID_proj_hit_dist[gap][orient][hit] : -9999;
}
short PHMuoTrackv9::get_muID_proj_hit_size(short gap, short orient, short hit) const
{
  return ((0<=gap && gap<5) && (0<=orient && orient<2) && (0<=hit && hit<_max_mui_hits)) ?
    muID_proj_hit_size[gap][orient][hit] : -1;
}

float PHMuoTrackv9::get_muID_gap0(const short arrayid) const
{
  return (0<=arrayid && arrayid<5) ? muID_gap0[arrayid] : 0;
}

//===== Mutators
void PHMuoTrackv9::set_px(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_px id out of range.\n";
      exit(1);
    }
  px[arrayid] = newVal;
}
void PHMuoTrackv9::set_py(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_py id out of range.\n";
      exit(1);
    }
  py[arrayid] = newVal;
}
void PHMuoTrackv9::set_pz(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_pz id out of range.\n";
      exit(1);
    }
  pz[arrayid] = newVal;
}
void PHMuoTrackv9::set_st1_bp_P(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_P id out of range.\n";
      exit(1);
    }
  st1_bp_P[arrayid] = newVal;
}
void PHMuoTrackv9::set_st1_bp_pos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_pos id out of range.\n";
      exit(1);
    }
  st1_bp_pos[arrayid] = newVal;
}
void PHMuoTrackv9::set_xpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_xpos id out of range.\n";
      exit(1);
    }
  xpos[arrayid] = newVal;
}
void PHMuoTrackv9::set_ypos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_ypos id out of range.\n";
      exit(1);
    }
  ypos[arrayid] = newVal;
}
void PHMuoTrackv9::set_zpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_zpos id out of range.\n";
      exit(1);
    }
  zpos[arrayid] = newVal;
}
void PHMuoTrackv9::set_cov(short arrayid1, short arrayid2, float newVal)
{
  if(arrayid1<0 || arrayid1 >= _covdim || 
     arrayid2<0 || arrayid2 >= _covdim)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv9::set_cov id out of range. \n";
      exit(1);
    }
  cov[arrayid1][arrayid2] = newVal;
}

void PHMuoTrackv9::set_muIDOO_gap0(const short arrayid, const short iroad, float newVal)
{
  if (arrayid<0 || arrayid>=5 || iroad < 0 || iroad >= _maxroads ) {
    std::cout<<PHWHERE<<" PHMuoTrackv9::set_muID_gap0 id out of range.\n";
    return ;
  }
  muIDOO_gap0[arrayid][iroad] = newVal;
}
void PHMuoTrackv9::set_muID_proj_hit_dist(short gap, short orient, short hit, float newVal)
{
  if (gap<0 || gap>=5 || orient<0 || orient >=2 || hit<0 || hit>=_max_mui_hits) {
    std::cout<<PHWHERE<<" PHMuoTrackv9::set_muID_proj_hit_dist id out of range.\n";
    return ;
  }
  muID_proj_hit_dist[gap][orient][hit] = newVal;
}
void PHMuoTrackv9::set_muID_proj_hit_size(short gap, short orient, short hit, short newVal)
{
  if (gap<0 || gap>=5 || orient<0 || orient >=2 || hit<0 || hit>=_max_mui_hits) {
    std::cout<<PHWHERE<<" PHMuoTrackv9::set_muID_proj_hit_size id out of range.\n";
    return ;
  }
  muID_proj_hit_size[gap][orient][hit] = newVal;
}

void PHMuoTrackv9::set_muID_gap0(const short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=5 ) {
    std::cout<<PHWHERE<<" PHMuoTrackv9::set_muID_gap0 id out of range.\n";
    return ;
  }
  muID_gap0[arrayid] = newVal;
}
