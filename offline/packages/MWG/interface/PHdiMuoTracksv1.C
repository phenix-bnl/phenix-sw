// $Id: PHdiMuoTracksv1.C,v 1.1 2009/07/04 18:32:22 hpereira Exp $
#include "MWG.h"
#include "PHdiMuoTracksv1.h"

ClassImp(PHdiMuoTracksv1)

//===== constructor/destructor
PHdiMuoTracksv1::PHdiMuoTracksv1()
  :nMuoTracks(0),ndiMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv1",MWG::MU_ARRAY_SIZE);
  diMuoTracks = new TClonesArray("PHdiMuoTrackv1",MWG::DIMU_ARRAY_SIZE);
}

PHdiMuoTracksv1::~PHdiMuoTracksv1(){delete MuoTracks; delete diMuoTracks;}


//=============================================== Dimuon Accessors
int PHdiMuoTracksv1::get_ditrkIndex(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_trkIndex(arrayid) : -1); 
}

float PHdiMuoTracksv1::get_dimass(const unsigned int idimu) const 
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_mass() : 0); 
}
int PHdiMuoTracksv1::get_dicharge(const unsigned int idimu) const
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_charge() : 0); 
}
float PHdiMuoTracksv1::get_dipx(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_px(arrayid) : 0); 
}
float PHdiMuoTracksv1::get_dipy(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_py(arrayid) : 0); 
}
float PHdiMuoTracksv1::get_dipz(short arrayid, const unsigned int idimu) const
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  return ((dimu) ? dimu->get_pz(arrayid) : 0); 
}


//================================================ Dimuon Mutators
void PHdiMuoTracksv1::set_dimass(const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_mass(newVal); 
}
void PHdiMuoTracksv1::set_dicharge(const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_charge(newVal); 
}

void PHdiMuoTracksv1::set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal)
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_trkIndex(arrayid,newVal);
}
void PHdiMuoTracksv1::set_dipx(short arrayid, const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_px(arrayid,newVal); 
}
void PHdiMuoTracksv1::set_dipy(short arrayid, const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_py(arrayid,newVal); 
}
void PHdiMuoTracksv1::set_dipz(short arrayid, const unsigned int idimu, float newVal)
{
  PHdiMuoTrackv1* dimu = (PHdiMuoTrackv1*) diMuoTracks->UncheckedAt(idimu);
  if(dimu) dimu->set_pz(arrayid,newVal); 
}
//===================================== Particle Accessors

//_______________________________________________________________________
float PHdiMuoTracksv1::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv1::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv1::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
short PHdiMuoTracksv1::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHdiMuoTracksv1::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}


//============================================== Particle Mutators
//_______________________________________________________________________
void PHdiMuoTracksv1::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHdiMuoTracksv1::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}


  //================================= PHParticle cloning/copying etc...
TObject* PHdiMuoTracksv1::GetSingleParticle(unsigned int ipart)
{
  // Single Muons only...
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv1::AddPHParticle(unsigned int ipart, TObject *o)
{
  // Single Muons only...
  AddPHParticle(ipart);
  PHMuoTrackv1 *destination = static_cast<PHMuoTrackv1*>(GetSingleParticle(ipart));
  PHMuoTrackv1 *source      = static_cast<PHMuoTrackv1*>(o);
  *destination = *source;
}

TObject* PHdiMuoTracksv1::GetSingleDimuon(unsigned int ipart)
{
  // di Muons only...
  return (TObject*) diMuoTracks->UncheckedAt(ipart);
}

void PHdiMuoTracksv1::AddPHDimuon(unsigned int ipart, TObject *o)
{
  // di Muons only...
  AddPHDimuon(ipart);
  PHdiMuoTrackv1 *destination = static_cast<PHdiMuoTrackv1*>(GetSingleDimuon(ipart));
  PHdiMuoTrackv1 *source      = static_cast<PHdiMuoTrackv1*>(o);
  *destination = *source;
}

PHdiMuoTracksv1* PHdiMuoTracksv1::clone() const
{
  //  Cloning is a complete copy...
  PHdiMuoTracksv1 *MyClone = new PHdiMuoTracksv1;

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


