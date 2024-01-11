// $Id: PHMuoTracksv4.C,v 1.1 2009/07/04 18:32:21 hpereira Exp $

#include "MWG.h"
#include "PHMuoTracksv4.h"


ClassImp(PHMuoTracksv4)

//===== constructor/destructor
PHMuoTracksv4::PHMuoTracksv4()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv4", MWG::MU_ARRAY_SIZE);
}
PHMuoTracksv4::PHMuoTracksv4(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv4::~PHMuoTracksv4(){delete MuoTracks;}

//===================================== Particle Accessors
float PHMuoTracksv4::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}
float PHMuoTracksv4::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}
float PHMuoTracksv4::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}
float PHMuoTracksv4::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}
float PHMuoTracksv4::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}
float PHMuoTracksv4::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}
float PHMuoTracksv4::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}
float PHMuoTracksv4::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}
short PHMuoTracksv4::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}
short PHMuoTracksv4::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}
short PHMuoTracksv4::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}
float PHMuoTracksv4::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}
float PHMuoTracksv4::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}
float PHMuoTracksv4::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}
float PHMuoTracksv4::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}
float PHMuoTracksv4::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}
int PHMuoTracksv4::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}
int PHMuoTracksv4::get_muIDhits(const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDhits() : 0);
}
float PHMuoTracksv4::get_muID_gap0(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_gap0(arrayid) : 0);
}
int PHMuoTracksv4::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//============================================== Particle Mutators
void PHMuoTracksv4::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}
void PHMuoTracksv4::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}
void PHMuoTracksv4::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}
void PHMuoTracksv4::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}
void PHMuoTracksv4::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}
void PHMuoTracksv4::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}
void PHMuoTracksv4::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}
void PHMuoTracksv4::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}
void PHMuoTracksv4::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}
void PHMuoTracksv4::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}
void PHMuoTracksv4::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}
void PHMuoTracksv4::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}
void PHMuoTracksv4::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}
void PHMuoTracksv4::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}
void PHMuoTracksv4::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}
void PHMuoTracksv4::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}
void PHMuoTracksv4::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}
void PHMuoTracksv4::set_muIDhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDhits(newVal); 
}
void PHMuoTracksv4::set_muID_gap0(short arrayid, const unsigned int itrk, float val)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_gap0(arrayid,val); 
}
void PHMuoTracksv4::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv4* trk = (PHMuoTrackv4*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}



  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv4::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHMuoTracksv4::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv4 *destination = static_cast<PHMuoTrackv4*>(GetSingleParticle(ipart));
  PHMuoTrackv4 *source      = static_cast<PHMuoTrackv4*>(o);
  *destination = *source;
}

PHMuoTracksv4* PHMuoTracksv4::clone() const
{
  PHMuoTracksv4 *MyClone = new PHMuoTracksv4;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
