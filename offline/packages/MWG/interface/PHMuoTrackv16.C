#include "PHMuoTrackv16.h"

#include <phool.h>

#include <algorithm>
#include <cmath>
#include <cstdlib>

ClassImp(PHMuoTrackv16)

//________________________________________________________________
PHMuoTrackv16::PHMuoTrackv16():
  uid(0),
  nhits(0),
  charge(0), 
  ndf( 0 ),
  chisquare( 0 ),
  muTRhits(0),
  TMutTrk_status(0),
  clusters_size1(0),
  fvtxmutr_chi2(-1.0),
  fvtx_cluster_size(0),
  fvtx_dphi(-100.),
  fvtx_dtheta(-100.0),
  fvtx_dr(-100.0),
  fvtx_chi2(-1.0),
  fvtx_tracklets_cone(0),
  fvtx_clusters_cone(0),
  nfvtx_tracklets(0),
  fvtx_hits(0)
{
  memset(px,0,sizeof(px));
  memset(py,0,sizeof(py));
  memset(pz,0,sizeof(pz));
  memset(xpos,0,sizeof(xpos));
  memset(ypos,0,sizeof(ypos));
  memset(zpos,0,sizeof(zpos));

  memset(st1_bp_P,0,sizeof(st1_bp_P));
  memset(st1_bp_pos,0,sizeof(st1_bp_pos));

  memset(cov,0,sizeof(cov));

  memset(muIDOOhits,0,sizeof(muIDOOhits));
  memset(muIDOOchi,0,sizeof(muIDOOchi));
  
  memset(muIDOO_gap0,0,sizeof(muIDOO_gap0));

  std::fill(_muid_hit_x,_muid_hit_x+sizeof(_muid_hit_x)/sizeof(float),-9999);
  std::fill(_muid_hit_y,_muid_hit_y+sizeof(_muid_hit_y)/sizeof(float),-9999);

  std::fill( _delta_q,_delta_q+sizeof(_delta_q)/sizeof(float),-9999);
  std::fill(_delta_q_error,_delta_q_error+sizeof(_delta_q_error)/sizeof(float),-1);

  
  memset(fvtx_p,0,sizeof(fvtx_p));
  std::fill(fvtx_vtx,fvtx_vtx+sizeof(fvtx_vtx)/sizeof(float),-100);
  memset(fvtxmutr_p,0,sizeof(fvtxmutr_p));
  std::fill(fvtxmutr_vtx,fvtxmutr_vtx+sizeof(fvtxmutr_vtx)/sizeof(float),-100);
  std::fill(&fvtx_cov[0][0],&fvtx_cov[0][0]+sizeof(fvtx_cov)/sizeof(float),-100);
  std::fill(&fvtxmutr_cov[0][0],&fvtxmutr_cov[0][0]+sizeof(fvtxmutr_cov)/sizeof(float),-100);
  memset(fvtx_cluster_charge,0,sizeof(fvtx_cluster_charge));
  std::fill(fvtx_w,fvtx_w+sizeof(fvtx_w)/sizeof(float),100);
  std::fill(fvtx_strip,fvtx_strip+sizeof(fvtx_strip)/sizeof(unsigned long),2000);

}

//________________________________________________________________
void PHMuoTrackv16::identify(std::ostream & out) const
{
  out << "identify yourself: I am a mutr track object" << std::endl;
}

//===== accessors
float PHMuoTrackv16::get_px(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? px[arrayid] : 0; }

float PHMuoTrackv16::get_py(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? py[arrayid] : 0; }

float PHMuoTrackv16::get_pz(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? pz[arrayid] : 0; }

float PHMuoTrackv16::get_st1_bp_P(short arrayid) const
{ return (0<=arrayid && arrayid<3) ? st1_bp_P[arrayid] : 0; }

float PHMuoTrackv16::get_st1_bp_pos(short arrayid) const
{ return (0<=arrayid && arrayid<3) ? st1_bp_pos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv16::get_xpos(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? xpos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv16::get_ypos(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? ypos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv16::get_zpos(short arrayid) const
{ return (0<=arrayid && arrayid<_maxpoints) ? zpos[arrayid] : 0; }

//_____________________________________________________
float PHMuoTrackv16::get_cov(short arrayid1, short arrayid2) const
{ 
  return 
    ( 0<=arrayid1 && arrayid1<_covdim && 
      0<=arrayid2 && arrayid2<_covdim) ?  
    cov[arrayid1][arrayid2] : 0.0;
}

//_____________________________________________________
float PHMuoTrackv16::get_muIDOO_gap0(const short arrayid, const short iroad) const
{ return ((0<=arrayid && arrayid<5)&&(0<=iroad && iroad < _maxroads)) ? muIDOO_gap0[arrayid][iroad] : 0; }

//_____________________________________________________
float PHMuoTrackv16::get_muid_hit_x(short gap) const
{ return (0<=gap && gap<5) ? _muid_hit_x[gap] : -8888; }

//_____________________________________________________
float PHMuoTrackv16::get_muid_hit_y(short gap) const
{ return (0<=gap && gap<5) ? _muid_hit_y[gap] : -8888; }

//================================ Mutators ===================================
void PHMuoTrackv16::set_px(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_px id out of range.\n";
      exit(1);
    }
  px[arrayid] = newVal;
}
void PHMuoTrackv16::set_py(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_py id out of range.\n";
      exit(1);
    }
  py[arrayid] = newVal;
}
void PHMuoTrackv16::set_pz(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_pz id out of range.\n";
      exit(1);
    }
  pz[arrayid] = newVal;
}
void PHMuoTrackv16::set_st1_bp_P(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_P id out of range.\n";
      exit(1);
    }
  st1_bp_P[arrayid] = newVal;
}
void PHMuoTrackv16::set_st1_bp_pos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=3) 
    {
      std::cout<<PHWHERE<<" set_st1_bp_pos id out of range.\n";
      exit(1);
    }
  st1_bp_pos[arrayid] = newVal;
}
void PHMuoTrackv16::set_xpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_xpos id out of range.\n";
      return;
    }
  xpos[arrayid] = newVal;
}
void PHMuoTrackv16::set_ypos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_ypos id out of range.\n";
      return;
    }
  ypos[arrayid] = newVal;
}
void PHMuoTrackv16::set_zpos(short arrayid, float newVal)
{
  if (arrayid<0 || arrayid>=_maxpoints)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_zpos id out of range.\n";
      return;
    }
  zpos[arrayid] = newVal;
}
void PHMuoTrackv16::set_cov(short arrayid1, short arrayid2, float newVal)
{
  if(arrayid1<0 || arrayid1 >= _covdim || 
     arrayid2<0 || arrayid2 >= _covdim)
    {
      std::cout<<PHWHERE<<" PHMuoTrackv16::set_cov id out of range. \n";
      return;
    }
  cov[arrayid1][arrayid2] = newVal;
}

void PHMuoTrackv16::set_muIDOO_gap0(const short arrayid, const short iroad, float newVal)
{
  if (arrayid<0 || arrayid>=5 || iroad < 0 || iroad >= _maxroads ) {
    std::cout<<PHWHERE<<" PHMuoTrackv16::set_muID_gap0 id out of range.\n";
    return ;
  }
  muIDOO_gap0[arrayid][iroad] = newVal;
}

//
void PHMuoTrackv16::set_muid_hit_x(short gap, float newVal)
{
  if(0>gap || gap>=5) {
    std::cout << PHWHERE << " PHMuoTrackv16::set_muid_hit_x gap out of range.\n";
    return ;
  }
  _muid_hit_x[gap] = newVal;
}

void PHMuoTrackv16::set_muid_hit_y(short gap, float newVal)
{
  if(0>gap || gap>=5) {
    std::cout << PHWHERE << " PHMuoTrackv16::set_muid_hit_y gap out of range.\n";
    return ;
  }
  _muid_hit_y[gap] = newVal;
}

void PHMuoTrackv16::set_nfvtx_tracklets_conerange(size_t dRbin, size_t ntracklets)
{
  if (dRbin>=8) return; // out of range
  if (ntracklets>0xf) ntracklets = 0xf; //can store up to 16 cluster counts in each  bin
  fvtx_tracklets_cone &= ~(((uint32_t)0xf) << (dRbin*4));
  fvtx_tracklets_cone |= ((ntracklets&0xf) << (dRbin*4));
}

size_t PHMuoTrackv16::get_nfvtx_tracklets_conerange(size_t dRbin) const
{
  if (dRbin>=8) return 0;

  return (fvtx_tracklets_cone >> dRbin*4)&0xf;
}


void PHMuoTrackv16::set_nfvtx_clusters_conerange(size_t dRbin, size_t nclusters)
{
  if (dRbin>=8) return; // out of range
  if (nclusters>0xf) nclusters = 0xf; //can store up to 16 cluster counts in each  bin
  fvtx_clusters_cone &= ~(((uint32_t)0xf) << (dRbin*4));
  fvtx_clusters_cone |= ((nclusters&0xf) << (dRbin*4));
}

size_t PHMuoTrackv16::get_nfvtx_clusters_conerange(size_t dRbin) const
{
  if (dRbin>=8) return 0;

  return (fvtx_clusters_cone >> dRbin*4)&0xf;
}
