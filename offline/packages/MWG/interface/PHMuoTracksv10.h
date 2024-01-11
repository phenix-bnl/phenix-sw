#ifndef __PHMuoTracksv10_H
#define __PHMuoTracksv10_H
#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHMuoTracksOut.h"
#include "PHMuoTrackv10.h"

/** Event muon tracks container

This class contains all the muon tracks stored for each event. Single track variable
descriptions are given in section 4.2.
*/
class PHMuoTracksv10 : public PHMuoTracksOut
{

  public:
  
  //! constructor
  PHMuoTracksv10();
  
  //! constructor
  PHMuoTracksv10(TClonesArray* MuT);
  
  //! destructor
  virtual ~PHMuoTracksv10();
  
  //! reset container
  void Reset()
  {
    nMuoTracks = 0; 
    Clear();
  }
  
  //! identify object
  void identify(std::ostream &os=std::cout) const
  {
      os << "identify yourself:  PHMuoTracksv10 object" << std::endl;
      return;
  }
  
  //! clear container
  void Clear(Option_t *option = "")
  {
    if( MuoTracks ) MuoTracks->Delete();
  }
  
  //! object validity
  int IsValid() const {return 1;}

  //====================================== Particle Fillers
  int set_TClonesArraySize(const unsigned int npart){
      MuoTracks->Expand(npart);
      return npart;
  }
  
  void AddPHParticle(const unsigned int itrk)
  {
      new ((*MuoTracks)[itrk]) PHMuoTrackv10();
  }
  
  void RemovePHParticle(const unsigned int itrk)
  {
      MuoTracks->RemoveAt(itrk);
      return;
  }

  //================================= PHParticle cloning/copying etc...
  TObject* GetSingleParticle(unsigned int ipart);
  void AddPHParticle(unsigned int ipart, TObject *);
  PHMuoTracksv10* clone() const;

  //=================================== Particle Accessors
  /**@name Access methods

  */
  //@{
  unsigned int get_npart() const {return nMuoTracks;}
  // These are the gets appropriate for the PHParticle base class.
  // the PHParticle always refers to momentum at the vertex and
  // so the get_px() routines always reference array index 0.
  //                     TKH 3-13-2002
  float get_px(const unsigned int itrk) const
  {return get_px(0,(const unsigned int)itrk);}

  float get_py(const unsigned int itrk) const 
  {return get_py(0,(const unsigned int)itrk);} 

  float get_pz(const unsigned int itrk) const 
  {return get_pz(0,(const unsigned int)itrk);}

  /// Return {\bf itrk}$^{th}$ track {\bf charge}.
  short get_charge(const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf px[arrayid]}.
  float get_px(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf py[arrayid]}.
  float get_py(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf pz[arrayid]}.
  float get_pz(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf st1\_bp\_P[arrayid]}.
  float get_st1_bp_P(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf st1\_bp\_pos[arrayid]}.
  float get_st1_bp_pos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf xpos[arrayid]}.
  float get_xpos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf ypos[arrayid]}.
  float get_ypos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf zpos[arrayid]}.
  float get_zpos(short arrayid, const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf nhits}.
  short get_nhits(const unsigned int itrk) const;  

  /// Return {\bf itrk}$^{th}$ track {\bf cov[arrayid1][arrayid2]}.
  float get_cov(short arrayid1, short arrayid2, const unsigned int itrk) const;
  
  /// Return {\bf itrk}$^{th}$ track {\bf number of degrees of freedom}.
  int get_ndf( const unsigned int itrk ) const ;
  
  /// Return {\bf itrk}$^{th}$ track {\bf reduced chisquare}.
  float get_chisquare(const unsigned int itrk) const ;
  
  /// Return {\bf itrk}$^{th}$ track {\bf ghostflag}.
  float get_ghostflag(const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ track {\bf hits}.
  int get_muTRhits(const unsigned int itrk) const;

  /// Return {\bf itrk}$^{th}$ road {\bf chi_square}.
  float get_muIDOOchi(const short iroad, const unsigned int itrk ) const;

  /// Return {\bf itrk}$^{th}$ road {\bf hits}.
  int get_muIDOOhits(const short iroad, const unsigned int itrk ) const;

  /// Return road position and direction at gap0 (x,y,z,dxdz,dydz)
  float get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const;

  /// Return the (x,y) of the hits associated with the deepest road
  float get_muid_hit_x(const short gap, const unsigned int itrk) const;
  
  float get_muid_hit_y(const short gap, const unsigned int itrk) const;

  /// Return MutOO track status
  int get_TMutTrk_status(const unsigned int itrk) const;
  
  /// return gap_coordinate charge difference
  float get_delta_q( const unsigned int coord_id, const unsigned int itrk ) const;
  
  /// return error on gap_coordinate charge difference
  float get_delta_q_error( const unsigned int coord_id, const unsigned int itrk ) const;

  //@}

  //=================================== Particle Mutators
  void set_npart(const unsigned int npart) 
  {nMuoTracks=npart;}
  
  // These are the sets appropriate for the PHParticle base class.
  void set_px(const unsigned int itrk, const float newVal)
  {set_px(0,(const unsigned short)itrk,newVal);}

  void set_py(const unsigned int itrk, const float newVal)
  {set_py(0,(const unsigned short)itrk,newVal);}

  void set_pz(const unsigned int itrk, const float newVal)
  {set_pz(0,(const unsigned short)itrk,newVal);}

  void set_nhits(const unsigned int itrk, const short newVal);
  
  void set_charge(const unsigned int itrk, const short newVal);

  void set_px(short arrayid, const unsigned int itrk, float newVal);
  
  void set_py(short arrayid, const unsigned int itrk, float newVal);
  
  void set_pz(short arrayid, const unsigned int itrk, float newVal);
  
  void set_st1_bp_P(short arrayid, const unsigned int itrk, float newVal);
  
  void set_st1_bp_pos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_xpos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_ypos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_zpos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_cov(short arrayid1, short arrayid2, const unsigned int itrk, float newVal);
  
  //! changes track number of degrees of freedom
  void set_ndf(const unsigned int itrk, int newVal) ;
  
  //! changes track reduced chisquare
  void set_chisquare(const unsigned int itrk, float newVal) ;
  
  void set_ghostflag(const unsigned int itrk, float newVal);
  
  void set_muTRhits(const unsigned int itrk, int newVal);
  
  void set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal);

  void set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal);

  void set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float newVal);

  
  void set_muid_hit_x(const short gap, const unsigned int itrk, const float newVal);

  void set_muid_hit_y(const short gap, const unsigned int itrk, const float newVal);

  void set_TMutTrk_status(const unsigned int itrk, int newVal);
  
  /// set gap_coordinate charge difference
  void set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value );
  
  /// set error on gap_coordinate charge difference
  void set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value );

    /*
  Following accessors and modifiers are for hidden (that is: non-persistent)
  varibles. Variables are set, and used, at run-time. They are not stored into
  the output MWG root file
  */
  
  //!@name association to level2 muid primitives
  //@{
 
  //! number of associated primitives
  virtual unsigned int get_n_primitives( const unsigned int itrk ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_n_primitives():0; 
  }
  
  //! set number of primitives
  virtual void set_n_primitives( const unsigned int itrk, const unsigned int& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_n_primitives( value );
  }
  
  //! associated primitive level2 angle
  virtual double get_level2_phi( const unsigned int itrk, const unsigned int& i ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_level2_phi(i):0; 
  }

  //! associated primitive level2 angle
  virtual void set_level2_phi( const unsigned int itrk, const unsigned int&i, const double& value )
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_phi( i, value );
  }  
  
  //! associated primitive level2 angle
  virtual double get_level2_theta( const unsigned int itrk, const unsigned int& i ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_level2_theta(i):0; 
  }

  //! associated primitive level2 angle
  virtual void set_level2_theta( const unsigned int itrk, const unsigned int& i, const double& value )
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_theta( i, value );
  }
  
  //@}
  
  //!@name association to level2 mutr primitives
  //@{
  
  //! number of associated primitives
  virtual unsigned int get_n_mutr_primitives( const unsigned int itrk ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_n_mutr_primitives():0; 
  }
  
  //! set number of primitives
  virtual void set_n_mutr_primitives( const unsigned int itrk, const unsigned int& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_n_mutr_primitives( value );
  }

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_x( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmin_x( i, value );
  }  

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_y( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmin_y( i, value );
  }  

  //! associated level2 primitive min momentum
  virtual void set_level2_pmin_z( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmin_z( i, value );
  }  

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_x( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmin_x( i ):0;
  }  

  //! associated level2 primitive min momentum
  virtual double get_level2_pmin_y( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmin_y( i ):0;
  }  

    //! associated level2 primitive min momentum
  virtual double get_level2_pmin_z( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmin_z( i ):0;
  }  
  
  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_x( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmax_x( i, value );
  }  

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_y( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmax_y( i, value );
  }  

  //! associated level2 primitive max momentum
  virtual void set_level2_pmax_z( const unsigned int itrk, const unsigned int& i, const double& value )
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_level2_pmax_z( i, value );
  }  

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_x( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmax_x( i ):0;
  }  

  //! associated level2 primitive max momentum
  virtual double get_level2_pmax_y( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmax_z( i ):0;
  }  

    //! associated level2 primitive max momentum
  virtual double get_level2_pmax_z( const unsigned int itrk, const unsigned int& i ) const
  {
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_level2_pmax_z( i ):0;
  }  
  
  //@}
  
  //!@name event vertex information
  //@{
 
  //! event vertex z
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z( const unsigned int itrk ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_event_vertex_z():0; 
  }
  
  //! event vertex z error
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to fit together tracks that do not belong to the same event, when 
  performing event mixing
  */
  virtual double get_event_vertex_z_error( const unsigned int itrk ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return (trk) ? trk->get_event_vertex_z_error():0; 
  }
  
  //! event vertex z
  virtual void set_event_vertex_z( const unsigned int itrk, const double& value )
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_event_vertex_z( value );
  }

  //! event vertex z error
  virtual void set_event_vertex_z_error( const unsigned int itrk, const double& value )
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_event_vertex_z_error( value );
  }
  
  //@}  

  //!@name event reaction plane information
  //@{
  
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual double get_event_rp_angle( const unsigned int itrk, const unsigned int& arm ) const
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    return trk ? trk->get_event_rp_angle( arm ):0;
  }
 
  //! event reaction plane
  /*! 
  it needs to be stored on a track by track basis in order to be able
  to calculate the "average reaction plane angle for muons that do not belong to the same event
  */
  virtual void set_event_rp_angle( const unsigned int itrk, const unsigned int& arm, const double& value )
  { 
    PHMuoTrackv10* trk = (PHMuoTrackv10*) MuoTracks->UncheckedAt(itrk);
    if( trk ) trk->set_event_rp_angle( arm, value );
  }

  //@}
private:
  /**@name Variables

  */
  //@{
  /// Number of Muon Tracks in the event.
  int nMuoTracks;
  /// Muon Tracks Container.
  TClonesArray* MuoTracks;
  //@}

ClassDef(PHMuoTracksv10,1)
};
#endif
