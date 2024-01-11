// $Id: PHMuoTracksv1.C,v 1.1 2009/07/04 18:32:21 hpereira Exp $

#include "MWG.h"
#include "PHMuoTracksv1.h"


ClassImp(PHMuoTracksv1)

//===== constructor/destructor
PHMuoTracksv1::PHMuoTracksv1()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv1", MWG::MU_ARRAY_SIZE);
}
PHMuoTracksv1::PHMuoTracksv1(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv1::~PHMuoTracksv1(){delete MuoTracks;}

//===================================== Particle Accessors
float PHMuoTracksv1::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}
float PHMuoTracksv1::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}
float PHMuoTracksv1::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}
float PHMuoTracksv1::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}
float PHMuoTracksv1::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}
float PHMuoTracksv1::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}
float PHMuoTracksv1::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}
short PHMuoTracksv1::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}
short PHMuoTracksv1::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}
short PHMuoTracksv1::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}
float PHMuoTracksv1::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}
float PHMuoTracksv1::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}
float PHMuoTracksv1::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}
float PHMuoTracksv1::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}
float PHMuoTracksv1::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//============================================== Particle Mutators
void PHMuoTracksv1::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}
void PHMuoTracksv1::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}
void PHMuoTracksv1::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}
void PHMuoTracksv1::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}
void PHMuoTracksv1::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}
void PHMuoTracksv1::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}
void PHMuoTracksv1::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}
void PHMuoTracksv1::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}
void PHMuoTracksv1::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}
void PHMuoTracksv1::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}
void PHMuoTracksv1::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}
void PHMuoTracksv1::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}
void PHMuoTracksv1::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}
void PHMuoTracksv1::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}
void PHMuoTracksv1::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv1* trk = (PHMuoTrackv1*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}



  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv1::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHMuoTracksv1::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv1 *destination = static_cast<PHMuoTrackv1*>(GetSingleParticle(ipart));
  PHMuoTrackv1 *source      = static_cast<PHMuoTrackv1*>(o);
  *destination = *source;
}

PHMuoTracksv1* PHMuoTracksv1::clone() const
{
  PHMuoTracksv1 *MyClone = new PHMuoTracksv1;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
