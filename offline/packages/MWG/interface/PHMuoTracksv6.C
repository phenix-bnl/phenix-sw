// $Id: PHMuoTracksv6.C,v 1.1 2009/07/04 18:32:21 hpereira Exp $

#include "MWG.h"
#include "PHMuoTracksv6.h"


ClassImp(PHMuoTracksv6)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv6::PHMuoTracksv6()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv6", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv6::PHMuoTracksv6(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv6::~PHMuoTracksv6(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv6::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv6::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv6::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv6::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv6::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv6::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv6::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}
//_______________________________________________________________________
float PHMuoTracksv6::get_muID_proj_hit_dist(short gap, short orient, short hit, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_proj_hit_dist(gap, orient, hit) : -9999);
}
//_______________________________________________________________________
short PHMuoTracksv6::get_muID_proj_hit_size(short gap, short orient, short hit, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_proj_hit_size(gap, orient, hit) : -1);
}
//_______________________________________________________________________
int PHMuoTracksv6::get_muIDhits(const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv6::get_muID_gap0(const short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_gap0(arrayid) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv6::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________

//============================================== Particle Mutators
void PHMuoTracksv6::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}
void PHMuoTracksv6::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}
void PHMuoTracksv6::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}
void PHMuoTracksv6::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}
void PHMuoTracksv6::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}
void PHMuoTracksv6::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}
void PHMuoTracksv6::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}
void PHMuoTracksv6::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}
void PHMuoTracksv6::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}
void PHMuoTracksv6::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}
void PHMuoTracksv6::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}
void PHMuoTracksv6::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}
void PHMuoTracksv6::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}
void PHMuoTracksv6::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv6::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv6::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

void PHMuoTracksv6::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}
void PHMuoTracksv6::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}
void PHMuoTracksv6::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}
void PHMuoTracksv6::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}
void PHMuoTracksv6::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}
void PHMuoTracksv6::set_muIDhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDhits(newVal); 
}
void PHMuoTracksv6::set_muID_gap0(const short arrayid, const unsigned int itrk, float val)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_gap0(arrayid, val); 
}
void PHMuoTracksv6::set_muID_proj_hit_dist(short gap, short orient, short hit, const unsigned int itrk, float newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_proj_hit_dist(gap, orient, hit, newVal); 
}
void PHMuoTracksv6::set_muID_proj_hit_size(short gap, short orient, short hit, const unsigned int itrk, short newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_proj_hit_size(gap, orient, hit, newVal); 
}
void PHMuoTracksv6::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv6* trk = (PHMuoTrackv6*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}



  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv6::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHMuoTracksv6::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv6 *destination = static_cast<PHMuoTrackv6*>(GetSingleParticle(ipart));
  PHMuoTrackv6 *source      = static_cast<PHMuoTrackv6*>(o);
  *destination = *source;
}

PHMuoTracksv6* PHMuoTracksv6::clone() const
{
  PHMuoTracksv6 *MyClone = new PHMuoTracksv6;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
