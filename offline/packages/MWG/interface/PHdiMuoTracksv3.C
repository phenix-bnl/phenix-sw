// $Id: PHdiMuoTracksv3.C,v 1.1 2009/07/04 18:32:22 hpereira Exp $
#include "MWG.h"
#include "PHdiMuoTracksv3.h"

ClassImp(PHdiMuoTracksv3)

//===== constructor/destructor
PHdiMuoTracksv3::PHdiMuoTracksv3()
  :nMuoTracks(0),ndiMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv3",MWG::MU_ARRAY_SIZE);
  diMuoTracks = new TClonesArray("PHdiMuoTrackv2",MWG::DIMU_ARRAY_SIZE);
}

PHdiMuoTracksv3::~PHdiMuoTracksv3(){delete MuoTracks; delete diMuoTracks;}


//=============================================== Dimuon Accessors
int PHdiMuoTracksv3::get_ditrkIndex(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_trkIndex(arrayid) : -1); 
}

float PHdiMuoTracksv3::get_dimass(const unsigned int idimu) const 
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_mass() : 0); 
}
int PHdiMuoTracksv3::get_dicharge(const unsigned int idimu) const
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_charge() : 0); 
}
float PHdiMuoTracksv3::get_dipx(const unsigned int idimu) const
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_px() : 0); 
}
float PHdiMuoTracksv3::get_dipy(const unsigned int idimu) const
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_py() : 0); 
}
float PHdiMuoTracksv3::get_dipz(const unsigned int idimu) const
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_pz() : 0); 
}


//================================================ Dimuon Mutators
void PHdiMuoTracksv3::set_dimass(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_mass(newVal); 
}
void PHdiMuoTracksv3::set_dicharge(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_charge(newVal); 
}

void PHdiMuoTracksv3::set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_trkIndex(arrayid,newVal);
}
void PHdiMuoTracksv3::set_dipx(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_px(newVal); 
}
void PHdiMuoTracksv3::set_dipy(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_py(newVal); 
}
void PHdiMuoTracksv3::set_dipz(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv2* dimu = (PHdiMuoTrackv2*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_pz(newVal); 
}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHdiMuoTracksv3::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv3::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv3::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv3::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv3::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv3::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
int PHdiMuoTracksv3::get_muIDhits(const unsigned int itrk) const
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDhits() : 0);
}

//============================================== Particle Mutators
//_______________________________________________________________________
void PHdiMuoTracksv3::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv3::set_muIDhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv3* trk = (PHMuoTrackv3*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDhits(newVal); 
}

  //================================= PHParticle cloning/copying etc...
TObject* PHdiMuoTracksv3::GetSingleParticle(unsigned int ipart)
{
  // Single Muons only...
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv3::AddPHParticle(unsigned int ipart, TObject *o)
{
  // Single Muons only...
  AddPHParticle(ipart);
  PHMuoTrackv3 *destination = static_cast<PHMuoTrackv3*>(GetSingleParticle(ipart));
  PHMuoTrackv3 *source      = static_cast<PHMuoTrackv3*>(o);
  *destination = *source;
}

TObject* PHdiMuoTracksv3::GetSingleDimuon(unsigned int ipart)
{
  // di Muons only...
  return (TObject*) diMuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv3::AddPHDimuon(unsigned int ipart, TObject *o)
{
  // di Muons only...
  AddPHDimuon(ipart);
  PHdiMuoTrackv2 *destination = static_cast<PHdiMuoTrackv2*>(GetSingleDimuon(ipart));
  PHdiMuoTrackv2 *source      = static_cast<PHdiMuoTrackv2*>(o);
  *destination = *source;
}

PHdiMuoTracksv3* PHdiMuoTracksv3::clone() const
{
  //  Cloning is a complete copy...
  PHdiMuoTracksv3 *MyClone = new PHdiMuoTracksv3;

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


