#include "PHMuoTrackv13.h"

#include <phool.h>

#include <cmath>
#include <cstdlib>

ClassImp(PHMuoTrackv13)

//________________________________________________________________
PHMuoTrackv13::PHMuoTrackv13(): 
uid(0),
  charge(0), 
  ndf( 0 ),
  chisquare( 0 ),
  muTRhits(0),
  TMutTrk_status(0)
{
  
  for (int i=0; i<_maxpoints; i++) 
    {
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
    { for (int j=0; j<_covdim; j++) cov[i][j]=0; }
  
  for(int i = 0; i < _maxroads; i++) muIDOOhits[i] = 0;
  for(int i = 0; i < _maxroads; i++) muIDOOchi[i] = 0;
  
  for (int i= 0; i < 5; i++)
    { for(int j = 0; j < _maxroads; j++) muIDOO_gap0[i][j] = 0; }

  for (int ipa=0; ipa<5; ipa++) 
    {
      _muid_hit_x[ipa] = -9999;
      _muid_hit_y[ipa] = -9999;
    }
  
  for( unsigned int i_coord = 0; i_coord < _max_gap_coords; i_coord++ )
    {
      _delta_q[ i_coord ] = -9999;
      _delta_q_error[ i_coord ] = -1;
    }

  fvtx_cluster_size = 0;
  fvtx_dphi = -100.;
  fvtx_dtheta = -100.0;
  fvtx_dr = -100.0;
  fvtx_chi2 = -1.0;
  
  for (size_t i_coord = 0; i_coord < 3; i_coord++)
    {
      fvtx_p[i_coord] = 0;
      fvtx_vtx[i_coord] = -100.;
      fvtxmutr_p[i_coord] = 0;
      fvtxmutr_vtx[i_coord] = -100.;
    }
  for (size_t i_coord = 0; i_coord < 5; i_coord++)
    for (size_t j_coord = 0; j_coord < 5; j_coord++)
      {
	fvtx_cov[i_coord][j_coord] = -100.0;
	fvtxmutr_cov[i_coord][j_coord] = -100.0;
      }
  for (int i_fvtx_station = 0; i_fvtx_station < 4; i_fvtx_station++)
    {
      fvtx_cluster_charge[i_fvtx_station] = 0;
      fvtx_w[i_fvtx_station] = 100.;
      fvtx_strip[i_fvtx_station] = 2000;
    }
}

//________________________________________________________________
void PHMuoTrackv13::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors
float PHMuoTrackv13::get_px(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? px[arrayid] : 0; }

float PHMuoTrackv13::get_py(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? py[arrayid] : 0; }

float PHMuoTrackv13::get_pz(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? pz[arrayid] : 0; }

float PHMuoTrackv13::get_st1_bp_P(short arrayid) const
{ return (0<=arrayid && arrayid<3) ? st1_bp_P[arrayid] : 0; }

float PHMuoTrackv13::get_st1_bp_pos(short arrayid) const
{ return (0<=arrayid && arrayid<3) ? st1_bp_pos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv13::get_xpos(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? xpos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv13::get_ypos(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? ypos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv13::get_zpos(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? zpos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv13::get_cov(short arrayid1, short arrayid2) const 
{ 
  return 
    ( 0<=arrayid1 && arrayid1<_covdim && 
      0<=arrayid2 && arrayid2<_covdim) ?  
    cov[arrayid1][arrayid2] : 0.0;
}

//_____________________________________________________
float PHMuoTrackv13::get_muIDOO_gap0(const short arrayid, const short iroad) const
{ return ((0<=arrayid && arrayid<5)&&(0<=iroad && iroad < _maxroads)) ? muIDOO_gap0[arrayid][iroad] : 0; }

//_____________________________________________________
float PHMuoTrackv13::get_muid_hit_x(short gap) const
{ return (0<=gap && gap<5) ? _muid_hit_x[gap] : -8888; }

//_____________________________________________________
float PHMuoTrackv13::get_muid_hit_y(short gap) const
{ return (0<=gap && gap<5) ? _muid_hit_y[gap] : -8888; }

//================================ Mutators ===================================
void PHMuoTrackv13::set_px(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_px id out of range.\n";
      exit(1);
    }
  px[arrayid] = newVal;
}
void PHMuoTrackv13::set_py(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_py id out of range.\n";
      exit(1);
    }
  py[arrayid] = newVal;
}
void PHMuoTrackv13::set_pz(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_pz id out of range.\n";
      exit(1);
    }
  pz[arrayid] = newVal;
}
void PHMuoTrackv13::set_st1_bp_P(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_P id out of range.\n";
      exit(1);
    }
  st1_bp_P[arrayid] = newVal;
}
void PHMuoTrackv13::set_st1_bp_pos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_pos id out of range.\n";
      exit(1);
    }
  st1_bp_pos[arrayid] = newVal;
}
void PHMuoTrackv13::set_xpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_xpos id out of range.\n";
      exit(1);
    }
  xpos[arrayid] = newVal;
}
void PHMuoTrackv13::set_ypos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_ypos id out of range.\n";
      exit(1);
    }
  ypos[arrayid] = newVal;
}
void PHMuoTrackv13::set_zpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_zpos id out of range.\n";
      exit(1);
    }
  zpos[arrayid] = newVal;
}
void PHMuoTrackv13::set_cov(short arrayid1, short arrayid2, float newVal)
{
  if(arrayid1<0 || arrayid1 >= _covdim || 
     arrayid2<0 || arrayid2 >= _covdim)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv13::set_cov id out of range. \n";
      exit(1);
    }
  cov[arrayid1][arrayid2] = newVal;
}

void PHMuoTrackv13::set_muIDOO_gap0(const short arrayid, const short iroad, float newVal)
{
  if (arrayid<0 || arrayid>=5 || iroad < 0 || iroad >= _maxroads ) {
    std::cout<<PHWHERE<<" PHMuoTrackv13::set_muID_gap0 id out of range.\n";
    return ;
  }
  muIDOO_gap0[arrayid][iroad] = newVal;
}

//
void PHMuoTrackv13::set_muid_hit_x(short gap, float newVal)
{
  if(0>gap && gap>=5) {
    std::cout << PHWHERE << " PHMuoTrackv13::set_muid_hit_x gap out of range.\n";
    return ;
  }
  _muid_hit_x[gap] = newVal;
}

void PHMuoTrackv13::set_muid_hit_y(short gap, float newVal)
{
  if(0>gap && gap>=5) {
    std::cout << PHWHERE << " PHMuoTrackv13::set_muid_hit_y gap out of range.\n";
    return ;
  }
  _muid_hit_y[gap] = newVal;
}
