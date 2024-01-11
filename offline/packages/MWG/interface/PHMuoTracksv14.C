#include "MWG.h"
#include "PHMuoTracksv14.h"

ClassImp(PHMuoTracksv14)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv14::PHMuoTracksv14()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv14", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv14::PHMuoTracksv14(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv14::~PHMuoTracksv14(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv14::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv14::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv14::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv14::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv14::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv14::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_muid_hit_x(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_x(gap) : -8888);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_muid_hit_y(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_y(gap) : -8888);
}

//_______________________________________________________________________
int PHMuoTracksv14::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv14::get_nclusters_size1(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nclusters_size1() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtxmutr_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtxmutr_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtxmutr_chi2(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_chi2() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv14::get_fvtx_cluster_size_word(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_size_word() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv14::get_fvtx_cluster_size(const unsigned int itrk, const size_t plane) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_size(plane) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_dphi(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dphi() : 0);
}


//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_dtheta(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dtheta() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_dr(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dr() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_chi2(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_chi2() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_w(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_w(station) : 0);
}

//_______________________________________________________________________
unsigned long PHMuoTracksv14::get_fvtx_global_strip(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_global_strip(station) : 0);
}

//_______________________________________________________________________
short unsigned int PHMuoTracksv14::get_fvtx_strip(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_strip(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv14::get_fvtx_column(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_column(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv14::get_fvtx_sector(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_sector(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv14::get_fvtx_cage(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cage(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv14::get_fvtx_arm(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_arm() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv14::get_fvtx_cluster_charge(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_charge(station) : 0);
}

//_______________________________________________________________________
unsigned long PHMuoTracksv14::get_fvtx_tracklets_cone(const unsigned int itrk) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_tracklets_cone() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv14::get_nfvtx_tracklets_conerange(const unsigned int itrk, size_t dtheta_bin) const
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nfvtx_tracklets_conerange(dtheta_bin) : 0);
}


//============================================== Particle Mutators
//____________________________________________________________________________________
void PHMuoTracksv14::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_x(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_y(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv14::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHMuoTracksv14::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv14::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv14::set_nclusters_size1(const unsigned int itrk, const short n)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nclusters_size1(n);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtxmutr_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtxmutr_chi2(const unsigned int itrk, const float x)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_chi2(x);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_cluster_size(const unsigned int itrk, const int word)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_size(word);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_cluster_size(const unsigned int itrk, const size_t plane, const size_t cluster_size)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_size(plane, cluster_size);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_dphi(const unsigned int itrk, const float dphi)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dphi(dphi);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_dtheta(const unsigned int itrk, const float dtheta)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dtheta(dtheta);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_dr(const unsigned int itrk, const float dr)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dr(dr);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_chi2(const unsigned int itrk, const float chi2)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_chi2(chi2);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_w(const unsigned int itrk, const size_t station, const float proj)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_w(station, proj);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_strip(const unsigned int itrk, 
				    const size_t station,
				    const bool arm,
				    const bool cage,
				    const unsigned short sector,
				    const bool column,
				    const unsigned short strip)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_strip(station, arm, cage, sector, column, strip);
}


//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_fvtx_cluster_charge(const unsigned int itrk, const size_t station, const float charge)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_charge(station, charge);
}

//_______________________________________________________________________
void PHMuoTracksv14::set_nfvtx_tracklets_conerange(const unsigned int itrk, const size_t dthetabin, const size_t ntracklets)
{
  PHMuoTrackv14* trk = (PHMuoTrackv14*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_tracklets_conerange(dthetabin, ntracklets);
}


//================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv14::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

//____________________________________________________________________________________
void PHMuoTracksv14::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv14 *destination = static_cast<PHMuoTrackv14*>(GetSingleParticle(ipart));
  PHMuoTrackv14 *source      = static_cast<PHMuoTrackv14*>(o);
  *destination = *source;
}

//____________________________________________________________________________________
PHMuoTracksv14* PHMuoTracksv14::clone() const
{
  PHMuoTracksv14 *MyClone = new PHMuoTracksv14;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
