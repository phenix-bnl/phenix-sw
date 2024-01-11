// $Id: PHdiMuoTracksv5.C,v 1.1 2009/07/04 18:32:23 hpereira Exp $
#include "MWG.h"
#include "PHdiMuoTracksv5.h"


#include <iostream>
using namespace std;

ClassImp(PHdiMuoTracksv5)

//===== constructor/destructor
//_______________________________________________
PHdiMuoTracksv5::PHdiMuoTracksv5()
  :nMuoTracks(0),ndiMuoTracks(0)
{
  //cerr << "PHdiMuoTracksv5::PHdiMuoTracksv5.\n" << endl;
  MuoTracks = new TClonesArray("PHMuoTrackv5",MWG::MU_ARRAY_SIZE);
  diMuoTracks = new TClonesArray("PHdiMuoTrackv3",MWG::DIMU_ARRAY_SIZE);
}

//_______________________________________________
PHdiMuoTracksv5::~PHdiMuoTracksv5(){delete MuoTracks; delete diMuoTracks;}


//=============================================== Dimuon Accessors

//_______________________________________________
int PHdiMuoTracksv5::get_ditrkIndex(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_trkIndex(arrayid) : -1); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_dimass(const unsigned int idimu) const 
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_mass() : 0); 
}

//_______________________________________________
int PHdiMuoTracksv5::get_dicharge(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_charge() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_dipx(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_px() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_dipy(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_py() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_dipz(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_pz() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_xpos(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_xpos() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_ypos(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_ypos() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_zpos(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_zpos() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_chrg_1(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chrg_1() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_px_1(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_px_1() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_py_1(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_py_1() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_pz_1(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_pz_1() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_chrg_2(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chrg_2() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_px_2(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_px_2() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_py_2(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_py_2() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_pz_2(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_pz_2() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_chisquare(const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_chisquare() : 0); 
}

//_______________________________________________
float PHdiMuoTracksv5::get_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu) const
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_vtx_cov(arrayid1,arrayid2) : 0); 
}

//================================================ Dimuon Mutators

//_______________________________________________
void PHdiMuoTracksv5::set_dimass(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_mass(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_dicharge(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_charge(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_trkIndex(arrayid,newVal);
}

//_______________________________________________
void PHdiMuoTracksv5::set_dipx(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_px(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_dipy(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_py(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_dipz(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_pz(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_xpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_xpos(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_ypos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_ypos(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_zpos(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_zpos(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_chrg_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chrg_1(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_px_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_px_1(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_py_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_py_1(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_pz_1(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_pz_1(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_chrg_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chrg_2(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_px_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_px_2(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_py_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_py_2(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_pz_2(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_pz_2(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_chisquare(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_chisquare(newVal); 
}

//_______________________________________________
void PHdiMuoTracksv5::set_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv3* dimu = (PHdiMuoTrackv3*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_vtx_cov(arrayid1,arrayid2,newVal); 
}
//===================================== Particle Accessors

//_______________________________________________________________________
float PHdiMuoTracksv5::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv5::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv5::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv5::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv5::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv5::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv5::get_muIDhits(const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDhits() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv5::get_muID_gap0(const short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_gap0(arrayid) : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv5::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________

//============================================== Particle Mutators
void PHdiMuoTracksv5::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_muIDhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDhits(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_muID_gap0(const short arrayid, const unsigned int itrk, float val)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_gap0(arrayid, val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv5::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv5* trk = (PHMuoTrackv5*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}






  //================================= PHParticle cloning/copying etc...
TObject* PHdiMuoTracksv5::GetSingleParticle(unsigned int ipart)
{
  // Single Muons only...
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv5::AddPHParticle(unsigned int ipart, TObject *o)
{
  // Single Muons only...
  AddPHParticle(ipart);
  PHMuoTrackv5 *destination = static_cast<PHMuoTrackv5*>(GetSingleParticle(ipart));
  PHMuoTrackv5 *source      = static_cast<PHMuoTrackv5*>(o);
  *destination = *source;
}

TObject* PHdiMuoTracksv5::GetSingleDimuon(unsigned int ipart)
{
  // di Muons only...
  return (TObject*) diMuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv5::AddPHDimuon(unsigned int ipart, TObject *o)
{
  // di Muons only...
  AddPHDimuon(ipart);
  PHdiMuoTrackv3 *destination = static_cast<PHdiMuoTrackv3*>(GetSingleDimuon(ipart));
  PHdiMuoTrackv3 *source      = static_cast<PHdiMuoTrackv3*>(o);
  *destination = *source;
}

PHdiMuoTracksv5* PHdiMuoTracksv5::clone() const
{
  //  Cloning is a complete copy...
  PHdiMuoTracksv5 *MyClone = new PHdiMuoTracksv5;

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
