
#include "MWG.h"
#include "PHMuoTracksv11.h"

ClassImp(PHMuoTracksv11)

//===== constructor/destructor

//_______________________________________________________________________
PHMuoTracksv11::PHMuoTracksv11()
  :nMuoTracks(0)
{
  MuoTracks = new TClonesArray("PHMuoTrackv11", MWG::MU_ARRAY_SIZE);
}

//_______________________________________________________________________
PHMuoTracksv11::PHMuoTracksv11(TClonesArray *MuT) 
  :nMuoTracks(0)
{
  MuoTracks = MuT;
}

PHMuoTracksv11::~PHMuoTracksv11(){delete MuoTracks;}

//===================================== Particle Accessors

//_______________________________________________________________________
float PHMuoTracksv11::get_px(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_px(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_py(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_py(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_pz(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_pz(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_st1_bp_P(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_P(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_st1_bp_pos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_st1_bp_pos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_xpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_xpos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_ypos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ypos(arrayid) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_zpos(short arrayid, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_zpos(arrayid) : 0);
}

//_______________________________________________________________________
short PHMuoTracksv11::get_nhits(const unsigned int itrk) const  {
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_nhits() : 0);
}

//_______________________________________________________________________
short PHMuoTracksv11::get_charge(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_charge() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_cov(short arrayid1,
			      short arrayid2,
			      const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_cov(arrayid1,arrayid2) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv11::get_ndf(const unsigned int itrk) const 
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ndf() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_chisquare(const unsigned int itrk) const 
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_chisquare() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_ghostflag(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_ghostflag() : 0);
}

//_______________________________________________________________________
int PHMuoTracksv11::get_muTRhits(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muTRhits() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_muIDOOchi(const short iroad, const unsigned int itrk ) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOchi(iroad) : 0);
}

//_______________________________________________________________________
int PHMuoTracksv11::get_muIDOOhits(const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOOhits(iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_muIDOO_gap0(arrayid, iroad) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_muid_hit_x(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_x(gap) : -8888);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_muid_hit_y(const short gap, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return ((trk) ? trk->get_muid_hit_y(gap) : -8888);
}

//_______________________________________________________________________
int PHMuoTracksv11::get_TMutTrk_status(const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_TMutTrk_status() : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_delta_q( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q( coord_id ) : 0);
}

//_______________________________________________________________________
float PHMuoTracksv11::get_delta_q_error( const unsigned int coord_id, const unsigned int itrk) const
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  return((trk) ? trk->get_delta_q_error( coord_id ) : 0);
}

//============================================== Particle Mutators
//____________________________________________________________________________________
void PHMuoTracksv11::set_px(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_px(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_py(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_py(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_pz(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_pz(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_st1_bp_P(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_P(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_st1_bp_pos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_st1_bp_pos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_xpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_xpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_ypos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ypos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_zpos(short arrayid, const unsigned int itrk, const float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_zpos(arrayid,val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_nhits(const unsigned int itrk, const short val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_nhits(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_charge(const unsigned int itrk, const short val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_charge(val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_cov(short arrayid1,short arrayid2,
			     const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_cov(arrayid1, arrayid2, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_ndf(const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ndf(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_chisquare(const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_chisquare(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_ghostflag(const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_ghostflag(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_muTRhits(const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muTRhits(newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOchi(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOOhits(iroad, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float val)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muIDOO_gap0(arrayid, iroad, val); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_x(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_muid_hit_y(gap, newVal); 
}

//____________________________________________________________________________________
void PHMuoTracksv11::set_TMutTrk_status(const unsigned int itrk, int newVal)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if(trk) trk->set_TMutTrk_status(newVal); 
}

//_______________________________________________________________________
void PHMuoTracksv11::set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value)
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q( coord_id, value );
}

//_______________________________________________________________________
void PHMuoTracksv11::set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value )
{
  PHMuoTrackv11* trk = (PHMuoTrackv11*) MuoTracks->UncheckedAt(itrk);
  if( trk ) trk->set_delta_q_error( coord_id, value );
}


  //================================= PHParticle cloning/copying etc...
TObject* PHMuoTracksv11::GetSingleParticle(unsigned int ipart)
{
  return (TObject*) MuoTracks->UncheckedAt(ipart);
}

//____________________________________________________________________________________
void PHMuoTracksv11::AddPHParticle(unsigned int ipart, TObject *o)
{
  AddPHParticle(ipart);
  PHMuoTrackv11 *destination = static_cast<PHMuoTrackv11*>(GetSingleParticle(ipart));
  PHMuoTrackv11 *source      = static_cast<PHMuoTrackv11*>(o);
  *destination = *source;
}

//____________________________________________________________________________________
PHMuoTracksv11* PHMuoTracksv11::clone() const
{
  PHMuoTracksv11 *MyClone = new PHMuoTracksv11;

  for (unsigned int i=0; i<(unsigned int)nMuoTracks; i++)
    {
      MyClone->AddPHParticle(i,MuoTracks->UncheckedAt(i));
    }
  MyClone->set_npart(nMuoTracks);

  return MyClone;
}
