// $Id: PHdiMuoTracksv7.C,v 1.1 2009/07/04 18:32:23 hpereira Exp $
#include "MWG.h"
#include "PHdiMuoTracksv7.h"

ClassImp(PHdiMuoTracksv7)

//_________________________________________________________
//===== constructor/destructor
PHdiMuoTracksv7::PHdiMuoTracksv7()
  :nMuoTracks(0),ndiMuoTracks(0)
{
  
  MuoTracks = new TClonesArray("PHMuoTrackv7",MWG::MU_ARRAY_SIZE);
  diMuoTracks = new TClonesArray("PHdiMuoTrackv4",MWG::DIMU_ARRAY_SIZE);
}

//_________________________________________________________
PHdiMuoTracksv7::~PHdiMuoTracksv7()
{
  delete MuoTracks; 
  delete diMuoTracks;
}


//_________________________________________________________
//=============================================== Dimuon Accessors
int PHdiMuoTracksv7::get_ditrkIndex(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_trkIndex(arrayid) : -1); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_dimass(const unsigned int idimu) const 
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_mass() : 0); 
}

//________________________________________________________________
int PHdiMuoTracksv7::get_dicharge(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_charge() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_dipx(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_px() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_dipy(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_py() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_dipz(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_pz() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_bp_xpos(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_xpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_bp_ypos(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_ypos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_bp_zpos(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_zpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_bp_dca(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_dca() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_xpos(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_xpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_ypos(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_ypos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_zpos(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_zpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_chrg_1(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chrg_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_px_1(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_px_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_py_1(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_py_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_pz_1(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_pz_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_chrg_2(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chrg_2() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_px_2(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_px_2() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_py_2(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_py_2() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_pz_2(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_pz_2() : 0); 
}

//________________________________________________________________
int PHdiMuoTracksv7::get_vtx_ndf(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_ndf() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_chisquare(const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chisquare() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv7::get_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu) const
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_cov(arrayid1,arrayid2) : 0); 
}

//================================================ Dimuon Mutators

//________________________________________________________________
void PHdiMuoTracksv7::set_dimass(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_mass(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_dicharge(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_charge(newVal); 
}


//________________________________________________________________
void PHdiMuoTracksv7::set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_trkIndex(arrayid,newVal);
}

//________________________________________________________________
void PHdiMuoTracksv7::set_dipx(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_px(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_dipy(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_py(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_dipz(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_pz(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_bp_xpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_xpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_bp_ypos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_ypos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_bp_zpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_zpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_bp_dca(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_dca(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_xpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_xpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_ypos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_ypos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_zpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_zpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_chrg_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chrg_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_px_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_px_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_py_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_py_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_pz_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_pz_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_chrg_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chrg_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_px_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_px_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_py_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_py_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_pz_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_pz_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_ndf(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_ndf(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_chisquare(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chisquare(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv7::set_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv4* dimu = (PHdiMuoTrackv4*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_cov(arrayid1,arrayid2,newVal); 
}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHdiMuoTracksv7::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv7::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv7::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv7::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv7::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv7::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv7::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv7::get_muIDhits(const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDhits() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_muID_proj_hit_dist(short gap, short orient, short hit, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_proj_hit_dist(gap, orient, hit) : -9999);
}

//_______________________________________________________________________
short PHdiMuoTracksv7::get_muID_proj_hit_size(short gap, short orient, short hit, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_proj_hit_size(gap, orient, hit) : -1);
}

//_______________________________________________________________________
float PHdiMuoTracksv7::get_muID_gap0(const short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_gap0(arrayid) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv7::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________

//============================================== Particle Mutators
void PHdiMuoTracksv7::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}
//______________________________________________________________
void PHdiMuoTracksv7::set_muID_proj_hit_dist(short gap, short orient, short hit, 
					   const unsigned int itrk, float newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_proj_hit_dist(gap, orient, hit, newVal); 
}
//______________________________________________________________
void PHdiMuoTracksv7::set_muID_proj_hit_size(short gap, short orient, short hit, 
					   const unsigned int itrk, short newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_proj_hit_size(gap, orient, hit, newVal); 
}
//____________________________________________________________________________________
void PHdiMuoTracksv7::set_muIDhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDhits(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_muID_gap0(const short arrayid, const unsigned int itrk, float val)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_gap0(arrayid, val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv7::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv7* trk = (PHMuoTrackv7*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}


  //================================= PHParticle cloning/copying etc...
TObject* PHdiMuoTracksv7::GetSingleParticle(unsigned int ipart)
{
  // Single Muons only...
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv7::AddPHParticle(unsigned int ipart, TObject *o)
{
  // Single Muons only...
  AddPHParticle(ipart);
  PHMuoTrackv7 *destination = static_cast<PHMuoTrackv7*>(GetSingleParticle(ipart));
  PHMuoTrackv7 *source      = static_cast<PHMuoTrackv7*>(o);
  *destination = *source;
}

TObject* PHdiMuoTracksv7::GetSingleDimuon(unsigned int ipart)
{
  // di Muons only...
  return (TObject*) diMuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv7::AddPHDimuon(unsigned int ipart, TObject *o)
{
  // di Muons only...
  AddPHDimuon(ipart);
  PHdiMuoTrackv4 *destination = static_cast<PHdiMuoTrackv4*>(GetSingleDimuon(ipart));
  PHdiMuoTrackv4 *source      = static_cast<PHdiMuoTrackv4*>(o);
  *destination = *source;
}

PHdiMuoTracksv7* PHdiMuoTracksv7::clone() const
{
  //  Cloning is a complete copy...
  PHdiMuoTracksv7 *MyClone = new PHdiMuoTracksv7;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  for (unsigned int i=0; i<(unsigned int)ndiMuoTracks; i++)
    {
      MyClone->AddPHDimuon(i,diMuoTracks->UncheckedAt(i));
    }
  MyClone->set_ndimu(ndiMuoTracks);

  return MyClone;
}
