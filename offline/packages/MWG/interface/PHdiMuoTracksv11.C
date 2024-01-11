// $Id: PHdiMuoTracksv11.C,v 1.1 2009/07/04 18:32:22 hpereira Exp $

#include "MWG.h"
#include "PHdiMuoTracksv11.h"

ClassImp(PHdiMuoTracksv11)

//_________________________________________________________
//===== constructor/destructor
PHdiMuoTracksv11::PHdiMuoTracksv11()
  :nMuoTracks(0),ndiMuoTracks(0)
{
  
  MuoTracks = new TClonesArray("PHMuoTrackv11", MWG::MU_ARRAY_SIZE );
  diMuoTracks = new TClonesArray("PHdiMuoTrackv5", MWG::DIMU_ARRAY_SIZE );
  
}

//_________________________________________________________
PHdiMuoTracksv11::~PHdiMuoTracksv11()
{
  delete MuoTracks; 
  delete diMuoTracks;
}


//_________________________________________________________
//=============================================== Dimuon Accessors
int PHdiMuoTracksv11::get_ditrkIndex(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_trkIndex(arrayid) : -1); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_dimass(const unsigned int idimu) const 
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_mass() : 0); 
}

//________________________________________________________________
int PHdiMuoTracksv11::get_dicharge(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_charge() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_dipx(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_px() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_dipy(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_py() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_dipz(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_pz() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_bp_xpos(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_xpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_bp_ypos(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_ypos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_bp_zpos(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_zpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_bp_dca(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_bp_dca() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_xpos(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_xpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_ypos(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_ypos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_zpos(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_zpos() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_chrg_1(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chrg_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_px_1(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_px_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_py_1(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_py_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_pz_1(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_pz_1() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_chrg_2(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chrg_2() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_px_2(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_px_2() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_py_2(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_py_2() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_pz_2(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_pz_2() : 0); 
}

//________________________________________________________________
int PHdiMuoTracksv11::get_vtx_ndf(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_ndf() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_chisquare(const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chisquare() : 0); 
}

//________________________________________________________________
float PHdiMuoTracksv11::get_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu) const
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_cov(arrayid1,arrayid2) : 0); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_dimass(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_mass(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_dicharge(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_charge(newVal); 
}


//________________________________________________________________
void PHdiMuoTracksv11::set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_trkIndex(arrayid,newVal);
}

//________________________________________________________________
void PHdiMuoTracksv11::set_dipx(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_px(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_dipy(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_py(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_dipz(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_pz(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_bp_xpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_xpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_bp_ypos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_ypos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_bp_zpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_zpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_bp_dca(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_bp_dca(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_xpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_xpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_ypos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_ypos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_zpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_zpos(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_chrg_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chrg_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_px_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_px_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_py_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_py_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_pz_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_pz_1(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_chrg_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chrg_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_px_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_px_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_py_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_py_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_pz_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_pz_2(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_ndf(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_ndf(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_chisquare(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chisquare(newVal); 
}

//________________________________________________________________
void PHdiMuoTracksv11::set_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv5* dimu = (PHdiMuoTrackv5*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_cov(arrayid1,arrayid2,newVal); 
}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHdiMuoTracksv11::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv11::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv11::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv11::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv11::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv11::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_muid_hit_x(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_x(gap) : -8888);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_muid_hit_y(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_y(gap) : -8888);
}

//_______________________________________________________________________
int PHdiMuoTracksv11::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv11::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//_______________________________________________________________________

//============================================== Particle Mutators
void PHdiMuoTracksv11::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_muid_hit_x(const short gap, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_x(gap, val); 
}
//____________________________________________________________________________________
void PHdiMuoTracksv11::set_muid_hit_y(const short gap, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_y(gap, val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv11::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHdiMuoTracksv11::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHdiMuoTracksv11::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}


  //================================= PHParticle cloning/copying etc...
TObject* PHdiMuoTracksv11::GetSingleParticle(unsigned int ipart)
{
  // Single Muons only...
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv11::AddPHParticle(unsigned int ipart, TObject *o)
{
  // Single Muons only...
  AddPHParticle(ipart);
  PHMuoTrackv11 *destination = static_cast<PHMuoTrackv11*>(GetSingleParticle(ipart));
  PHMuoTrackv11 *source      = static_cast<PHMuoTrackv11*>(o);
  *destination = *source;
}

TObject* PHdiMuoTracksv11::GetSingleDimuon(unsigned int ipart)
{
  // di Muons only...
  return (TObject*) diMuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv11::AddPHDimuon(unsigned int ipart, TObject *o)
{
  // di Muons only...
  AddPHDimuon(ipart);
  PHdiMuoTrackv5 *destination = static_cast<PHdiMuoTrackv5*>(GetSingleDimuon(ipart));
  PHdiMuoTrackv5 *source      = static_cast<PHdiMuoTrackv5*>(o);
  *destination = *source;
}

PHdiMuoTracksv11* PHdiMuoTracksv11::clone() const
{
  //  Cloning is a complete copy...
  PHdiMuoTracksv11 *MyClone = new PHdiMuoTracksv11;

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
