// $Id: PHMuoTracksv2.C,v 1.1 2009/07/04 18:32:21 hpereira Exp $

#include "MWG.h"
#include "PHMuoTracksv2.h"


ClassImp(PHMuoTracksv2)

//===== constructor/destructor
PHMuoTracksv2::PHMuoTracksv2()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv2", MWG::MU_ARRAY_SIZE);
}
PHMuoTracksv2::PHMuoTracksv2(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv2::~PHMuoTracksv2(){delete MuoTracks;}

//===================================== Particle Accessors
float PHMuoTracksv2::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}
float PHMuoTracksv2::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}
float PHMuoTracksv2::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}
float PHMuoTracksv2::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}
float PHMuoTracksv2::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}
float PHMuoTracksv2::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}
float PHMuoTracksv2::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}
short PHMuoTracksv2::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}
short PHMuoTracksv2::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}
short PHMuoTracksv2::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}
float PHMuoTracksv2::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}
float PHMuoTracksv2::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}
float PHMuoTracksv2::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}
float PHMuoTracksv2::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}
float PHMuoTracksv2::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}
int PHMuoTracksv2::get_hitplans(const unsigned int itrk) const
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_hitplans() : 0);
}

//============================================== Particle Mutators
void PHMuoTracksv2::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}
void PHMuoTracksv2::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}
void PHMuoTracksv2::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}
void PHMuoTracksv2::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}
void PHMuoTracksv2::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}
void PHMuoTracksv2::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}
void PHMuoTracksv2::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}
void PHMuoTracksv2::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}
void PHMuoTracksv2::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}
void PHMuoTracksv2::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}
void PHMuoTracksv2::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}
void PHMuoTracksv2::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}
void PHMuoTracksv2::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}
void PHMuoTracksv2::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}
void PHMuoTracksv2::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}
void PHMuoTracksv2::set_hitplans(const unsigned int itrk, int newVal)
{
  PHMuoTrackv2* trk = (PHMuoTrackv2*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_hitplans(newVal); 
}



  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv2::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHMuoTracksv2::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv2 *destination = static_cast<PHMuoTrackv2*>(GetSingleParticle(ipart));
  PHMuoTrackv2 *source      = static_cast<PHMuoTrackv2*>(o);
  *destination = *source;
}

PHMuoTracksv2* PHMuoTracksv2::clone() const
{
  PHMuoTracksv2 *MyClone = new PHMuoTracksv2;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
