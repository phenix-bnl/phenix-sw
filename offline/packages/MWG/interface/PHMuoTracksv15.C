#include "MWG.h"
#include "PHMuoTracksv15.h"

ClassImp(PHMuoTracksv15)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv15::PHMuoTracksv15()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv15", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv15::PHMuoTracksv15(TClonesArray *MuT)
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv15::~PHMuoTracksv15(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv15::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv15::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv15::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv15::get_ndf(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_chisquare(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv15::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv15::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_muid_hit_x(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_x(gap) : -8888);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_muid_hit_y(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_y(gap) : -8888);
}

//_______________________________________________________________________
int PHMuoTracksv15::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//_______________________________________________________________________
unsigned short PHMuoTracksv15::get_clusters_size1(short itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_clusters_size1() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtxmutr_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtxmutr_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtxmutr_chi2(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_chi2() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv15::get_fvtx_cluster_size_word(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_size_word() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv15::get_fvtx_cluster_size(const unsigned int itrk, const size_t plane) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_size(plane) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_dphi(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dphi() : 0);
}


//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_dtheta(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dtheta() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_dr(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dr() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_chi2(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_chi2() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_w(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_w(station) : 0);
}

//_______________________________________________________________________
unsigned long PHMuoTracksv15::get_fvtx_global_strip(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_global_strip(station) : 0);
}

//_______________________________________________________________________
short unsigned int PHMuoTracksv15::get_fvtx_strip(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_strip(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv15::get_fvtx_column(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_column(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv15::get_fvtx_sector(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_sector(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv15::get_fvtx_cage(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cage(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv15::get_fvtx_arm(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_arm() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv15::get_fvtx_cluster_charge(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_charge(station) : 0);
}

//_______________________________________________________________________
unsigned long  PHMuoTracksv15::get_fvtx_tracklets_cone(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_tracklets_cone() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv15::get_nfvtx_tracklets_conerange(const unsigned int itrk, size_t dtheta_bin) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nfvtx_tracklets_conerange(dtheta_bin) : 0);
}

//_______________________________________________________________________
unsigned long  PHMuoTracksv15::get_fvtx_clusters_cone(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_clusters_cone() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv15::get_nfvtx_clusters_conerange(const unsigned int itrk, size_t dtheta_bin) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nfvtx_clusters_conerange(dtheta_bin) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv15::get_nfvtx_tracklets(const unsigned int itrk) const
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  return  (unsigned int)((trk) ? trk->get_nfvtx_tracklets() : 0);
}


//============================================== Particle Mutators
//____________________________________________________________________________________
void PHMuoTracksv15::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_x(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_y(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv15::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHMuoTracksv15::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv15::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv15::set_clusters_size1(const unsigned int itrk, const unsigned short n)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_clusters_size1(n);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtxmutr_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtxmutr_chi2(const unsigned int itrk, const float x)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_chi2(x);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_cluster_size(const unsigned int itrk, const int word)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_size(word);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_cluster_size(const unsigned int itrk, const size_t plane, const size_t cluster_size)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_size(plane, cluster_size);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_dphi(const unsigned int itrk, const float dphi)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dphi(dphi);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_dtheta(const unsigned int itrk, const float dtheta)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dtheta(dtheta);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_dr(const unsigned int itrk, const float dr)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dr(dr);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_chi2(const unsigned int itrk, const float chi2)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_chi2(chi2);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_w(const unsigned int itrk, const size_t station, const float proj)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_w(station, proj);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_strip(const unsigned int itrk,
				    const size_t station,
				    const bool arm,
				    const bool cage,
				    const unsigned short sector,
				    const bool column,
				    const unsigned short strip)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_strip(station, arm, cage, sector, column, strip);
}


//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_fvtx_cluster_charge(const unsigned int itrk, const size_t station, const float charge)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_charge(station, charge);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_nfvtx_tracklets_conerange(const unsigned int itrk, const size_t dthetabin, const size_t ntracklets)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_tracklets_conerange(dthetabin, ntracklets);
}

//_______________________________________________________________________
void PHMuoTracksv15::set_nfvtx_clusters_conerange(const unsigned int itrk, const size_t dthetabin, const size_t nclusters)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_clusters_conerange(dthetabin, nclusters);
}

//_______________________________________________________________________
//!overall number of cluster, which point to vertex of this track
void PHMuoTracksv15::set_nfvtx_tracklets(const unsigned int itrk, const unsigned int a)
{
  PHMuoTrackv15* trk = (PHMuoTrackv15*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_tracklets(a);
}

//================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv15::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

//____________________________________________________________________________________
void PHMuoTracksv15::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv15 *destination = static_cast<PHMuoTrackv15*>(GetSingleParticle(ipart));
  PHMuoTrackv15 *source      = static_cast<PHMuoTrackv15*>(o);
  *destination = *source;
}

//____________________________________________________________________________________
PHMuoTracksv15* PHMuoTracksv15::clone() const
{
  PHMuoTracksv15 *MyClone = new PHMuoTracksv15;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
