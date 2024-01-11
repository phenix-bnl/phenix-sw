#include "MWG.h"
#include "PHMuoTracksv16.h"

ClassImp(PHMuoTracksv16)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv16::PHMuoTracksv16()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv16", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv16::PHMuoTracksv16(TClonesArray *MuT)
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv16::~PHMuoTracksv16(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv16::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv16::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv16::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv16::get_ndf(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_chisquare(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv16::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv16::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_muid_hit_x(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_x(gap) : -8888);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_muid_hit_y(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_y(gap) : -8888);
}

//_______________________________________________________________________
int PHMuoTracksv16::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//_______________________________________________________________________
unsigned short PHMuoTracksv16::get_clusters_size1(short itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_clusters_size1() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtxmutr_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtxmutr_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtxmutr_chi2(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_chi2() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv16::get_fvtx_cluster_size_word(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_size_word() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv16::get_fvtx_cluster_size(const unsigned int itrk, const size_t plane) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_size(plane) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_dphi(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dphi() : 0);
}


//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_dtheta(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dtheta() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_dr(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_dr() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_chi2(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_chi2() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_w(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_w(station) : 0);
}

//_______________________________________________________________________
unsigned long PHMuoTracksv16::get_fvtx_global_strip(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_global_strip(station) : 0);
}

//_______________________________________________________________________
short unsigned int PHMuoTracksv16::get_fvtx_strip(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_strip(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv16::get_fvtx_column(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_column(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv16::get_fvtx_sector(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_sector(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv16::get_fvtx_cage(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cage(station) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv16::get_fvtx_arm(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_arm() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtxmutr_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_fvtx_cluster_charge(const unsigned int itrk, const size_t station) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_cluster_charge(station) : 0);
}

//_______________________________________________________________________
unsigned long  PHMuoTracksv16::get_fvtx_tracklets_cone(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_tracklets_cone() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv16::get_nfvtx_tracklets_conerange(const unsigned int itrk, size_t dtheta_bin) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nfvtx_tracklets_conerange(dtheta_bin) : 0);
}

//_______________________________________________________________________
unsigned long  PHMuoTracksv16::get_fvtx_clusters_cone(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_clusters_cone() : 0);
}

//_______________________________________________________________________
size_t PHMuoTracksv16::get_nfvtx_clusters_conerange(const unsigned int itrk, size_t dtheta_bin) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nfvtx_clusters_conerange(dtheta_bin) : 0);
}

//_______________________________________________________________________
unsigned int PHMuoTracksv16::get_nfvtx_tracklets(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return  (unsigned int)((trk) ? trk->get_nfvtx_tracklets() : 0);
}

//_______________________________________________________________________
unsigned short PHMuoTracksv16::get_fvtx_hits(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_fvtx_hits() : 0);
}

/*
//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtxmutr_vtx(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtxmutr_vtx(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtxmutr_p(const unsigned int itrk, const size_t coord) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtxmutr_p(coord) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtxmutr_chi2(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtxmutr_chi2() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_dphi(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_dphi() : 0);
}


//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_dtheta(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_dtheta() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_dr(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_dr() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_chi2(const unsigned int itrk) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_chi2() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtx_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtx_cov(i,j) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv16::get_sfvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j) const
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_sfvtxmutr_cov(i,j) : 0);
}
*/

//============================================== Particle Mutators
//____________________________________________________________________________________
void PHMuoTracksv16::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_x(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_y(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv16::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHMuoTracksv16::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv16::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv16::set_clusters_size1(const unsigned int itrk, const unsigned short n)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_clusters_size1(n);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtxmutr_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtxmutr_chi2(const unsigned int itrk, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_chi2(x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_cluster_size(const unsigned int itrk, const int word)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_size(word);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_cluster_size(const unsigned int itrk, const size_t plane, const size_t cluster_size)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_size(plane, cluster_size);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_dphi(const unsigned int itrk, const float dphi)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dphi(dphi);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_dtheta(const unsigned int itrk, const float dtheta)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dtheta(dtheta);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_dr(const unsigned int itrk, const float dr)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_dr(dr);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_chi2(const unsigned int itrk, const float chi2)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_chi2(chi2);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_w(const unsigned int itrk, const size_t station, const float proj)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_w(station, proj);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_strip(const unsigned int itrk,
				    const size_t station,
				    const bool arm,
				    const bool cage,
				    const unsigned short sector,
				    const bool column,
				    const unsigned short strip)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_strip(station, arm, cage, sector, column, strip);
}


//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtxmutr_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_cluster_charge(const unsigned int itrk, const size_t station, const float charge)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_cluster_charge(station, charge);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_nfvtx_tracklets_conerange(const unsigned int itrk, const size_t dthetabin, const size_t ntracklets)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_tracklets_conerange(dthetabin, ntracklets);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_nfvtx_clusters_conerange(const unsigned int itrk, const size_t dthetabin, const size_t nclusters)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_clusters_conerange(dthetabin, nclusters);
}

//_______________________________________________________________________
//!overall number of cluster, which point to vertex of this track
void PHMuoTracksv16::set_nfvtx_tracklets(const unsigned int itrk, const unsigned int a)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_nfvtx_tracklets(a);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_fvtx_hits(const unsigned int itrk, unsigned short a)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_fvtx_hits(a);
}

/*
//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtxmutr_vtx(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtxmutr_vtx(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtxmutr_p(const unsigned int itrk, const size_t coord, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtxmutr_p(coord, x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtxmutr_chi2(const unsigned int itrk, const float x)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtxmutr_chi2(x);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_dphi(const unsigned int itrk, const float dphi)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_dphi(dphi);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_dtheta(const unsigned int itrk, const float dtheta)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_dtheta(dtheta);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_dr(const unsigned int itrk, const float dr)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_dr(dr);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_chi2(const unsigned int itrk, const float chi2)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_chi2(chi2);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtx_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtx_cov(i, j, cov);
}

//_______________________________________________________________________
void PHMuoTracksv16::set_sfvtxmutr_cov(const unsigned int itrk, const size_t i, const size_t j, const float cov)
{
  PHMuoTrackv16* trk = (PHMuoTrackv16*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_sfvtxmutr_cov(i, j, cov);
}

*/

//================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv16::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

//____________________________________________________________________________________
void PHMuoTracksv16::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv16 *destination = static_cast<PHMuoTrackv16*>(GetSingleParticle(ipart));
  PHMuoTrackv16 *source      = static_cast<PHMuoTrackv16*>(o);
  *destination = *source;
}

//____________________________________________________________________________________
PHMuoTracksv16* PHMuoTracksv16::clone() const
{
  PHMuoTracksv16 *MyClone = new PHMuoTracksv16;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
