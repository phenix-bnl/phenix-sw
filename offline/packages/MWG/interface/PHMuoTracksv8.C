// $Id: PHMuoTracksv8.C,v 1.1 2009/07/04 18:32:21 hpereira Exp $

#include "MWG.h"
#include "PHMuoTracksv8.h"


ClassImp(PHMuoTracksv8)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv8::PHMuoTracksv8()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv8", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv8::PHMuoTracksv8(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv8::~PHMuoTracksv8(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv8::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv8::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv8::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv8::get_PID(const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PID() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_MuonConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_MuonConfidence() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_PionConfidence(const unsigned int itrk) const 
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_PionConfidence() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv8::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv8::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv8::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}
//_______________________________________________________________________
float PHMuoTracksv8::get_muID_proj_hit_dist(short gap, short orient, short hit, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_proj_hit_dist(gap, orient, hit) : -9999);
}
//_______________________________________________________________________
short PHMuoTracksv8::get_muID_proj_hit_size(short gap, short orient, short hit, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_proj_hit_size(gap, orient, hit) : -1);
}
//_______________________________________________________________________
int PHMuoTracksv8::get_muIDhits(const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_muID_gap0(const short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muID_gap0(arrayid) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv8::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv8::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//_______________________________________________________________________

//============================================== Particle Mutators
void PHMuoTracksv8::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}
void PHMuoTracksv8::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}
void PHMuoTracksv8::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}
void PHMuoTracksv8::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}
void PHMuoTracksv8::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}
void PHMuoTracksv8::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}
void PHMuoTracksv8::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}
void PHMuoTracksv8::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}
void PHMuoTracksv8::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}
void PHMuoTracksv8::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}
void PHMuoTracksv8::set_PID(const unsigned int itrk, const short val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PID(val); 
}
void PHMuoTracksv8::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}
void PHMuoTracksv8::set_MuonConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_MuonConfidence(newVal); 
}
void PHMuoTracksv8::set_PionConfidence(const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_PionConfidence(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv8::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv8::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

void PHMuoTracksv8::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}
void PHMuoTracksv8::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}
void PHMuoTracksv8::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}
void PHMuoTracksv8::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}
void PHMuoTracksv8::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}
void PHMuoTracksv8::set_muIDhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDhits(newVal); 
}
void PHMuoTracksv8::set_muID_gap0(const short arrayid, const unsigned int itrk, float val)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_gap0(arrayid, val); 
}
void PHMuoTracksv8::set_muID_proj_hit_dist(short gap, short orient, short hit, const unsigned int itrk, float newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_proj_hit_dist(gap, orient, hit, newVal); 
}
void PHMuoTracksv8::set_muID_proj_hit_size(short gap, short orient, short hit, const unsigned int itrk, short newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muID_proj_hit_size(gap, orient, hit, newVal); 
}
void PHMuoTracksv8::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHMuoTracksv8::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv8::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv8* trk = (PHMuoTrackv8*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}


  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv8::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

void PHMuoTracksv8::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv8 *destination = static_cast<PHMuoTrackv8*>(GetSingleParticle(ipart));
  PHMuoTrackv8 *source      = static_cast<PHMuoTrackv8*>(o);
  *destination = *source;
}

PHMuoTracksv8* PHMuoTracksv8::clone() const
{
  PHMuoTracksv8 *MyClone = new PHMuoTracksv8;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
