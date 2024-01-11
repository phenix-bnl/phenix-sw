
#include "MWG.h"
#include "PHMuoTracksv10.h"

ClassImp(PHMuoTracksv10)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv10::PHMuoTracksv10()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv10", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv10::PHMuoTracksv10(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv10::~PHMuoTracksv10(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv10::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv10::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv10::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv10::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv10::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv10::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_muid_hit_x(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_x(gap) : -8888);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_muid_hit_y(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_y(gap) : -8888);
}

//_______________________________________________________________________
int PHMuoTracksv10::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv10::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//============================================== Particle Mutators
//____________________________________________________________________________________
void PHMuoTracksv10::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_x(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_y(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv10::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHMuoTracksv10::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv10::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}


  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv10::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

//____________________________________________________________________________________
void PHMuoTracksv10::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv10 *destination = static_cast<PHMuoTrackv10*>(GetSingleParticle(ipart));
  PHMuoTrackv10 *source      = static_cast<PHMuoTrackv10*>(o);
  *destination = *source;
}

//____________________________________________________________________________________
PHMuoTracksv10* PHMuoTracksv10::clone() const
{
  PHMuoTracksv10 *MyClone = new PHMuoTracksv10;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
