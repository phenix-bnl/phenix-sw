#ifndef __PHMUOTRACKSv8_H
#define __PHMUOTRACKSv8_H
#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHMuoTracksOut.h"
#include "PHMuoTrackv8.h"

/** Event muon tracks container

This class contains all the muon tracks stored for each event. Single track variable
descriptions are given in section 4.2.
*/
class PHMuoTracksv8 : public PHMuoTracksOut
{

  public:
  
  //! constructor
  PHMuoTracksv8();
  
  //! constructor
  PHMuoTracksv8(TClonesArray* MuT);
  
  //! destructor
  virtual ~PHMuoTracksv8();
  
  //! reset container
  void Reset()
  {
    nMuoTracks = 0; 
    Clear();
  }
  
  //! identify object
  void identify(std::ostream &os=std::cout) const
  {
      os << "identify yourself:  PHMuoTracksv8 object" << std::endl;
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
      new ((*MuoTracks)[itrk]) PHMuoTrackv8();
  }
  
  void RemovePHParticle(const unsigned int itrk)
  {
      MuoTracks->RemoveAt(itrk);
      return;
  }

  //================================= PHParticle cloning/copying etc...
  TObject* GetSingleParticle(unsigned int ipart);
  void AddPHParticle(unsigned int ipart, TObject *);
  PHMuoTracksv8* clone() const;

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
  /// Return {\bf itrk}$^{th}$ track {\bf PID}.
  short get_PID(const unsigned int itrk) const;
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
  /// Return {\bf itrk}$^{th}$ track {\bf MuonConfidence}.
  float get_MuonConfidence(const unsigned int itrk) const ;
  /// Return {\bf itrk}$^{th}$ track {\bf PionConfidence}.
  float get_PionConfidence(const unsigned int itrk) const ;
  
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
  /// Return {\bf itrk}$^{th}$ road {\bf hits}.
  int get_muIDhits(const unsigned int itrk ) const;
  /// Return road position and direction at gap0 (x,y,z,dxdz,dydz)
  float get_muID_gap0(const short arrayid, const unsigned int itrk) const;
  /// return distance of the muID hit from the projected track/muID point
  float get_muID_proj_hit_dist(short gap, short orient, short hit, 
			       const unsigned int itrk) const;
  short get_muID_proj_hit_size(short gap, short orient, short hit, 
			       const unsigned int itrk) const;
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
  void set_PID(const unsigned int itrk, const short newVal);
  void set_charge(const unsigned int itrk, const short newVal);
  //
  void set_px(short arrayid, const unsigned int itrk, float newVal);
  void set_py(short arrayid, const unsigned int itrk, float newVal);
  void set_pz(short arrayid, const unsigned int itrk, float newVal);
  void set_st1_bp_P(short arrayid, const unsigned int itrk, float newVal);
  void set_st1_bp_pos(short arrayid, const unsigned int itrk, float newVal);
  void set_xpos(short arrayid, const unsigned int itrk, float newVal);
  void set_ypos(short arrayid, const unsigned int itrk, float newVal);
  void set_zpos(short arrayid, const unsigned int itrk, float newVal);
  void set_cov(short arrayid1, short arrayid2, const unsigned int itrk, float newVal);
  void set_MuonConfidence(const unsigned int itrk, float newVal) ;
  void set_PionConfidence(const unsigned int itrk, float newVal) ;
  
  //! changes track number of degrees of freedom
  void set_ndf(const unsigned int itrk, int newVal) ;
  
  //! changes track reduced chisquare
  void set_chisquare(const unsigned int itrk, float newVal) ;
  
  void set_ghostflag(const unsigned int itrk, float newVal);
  void set_muTRhits(const unsigned int itrk, int newVal);
  void set_muIDOOchi(const short iroad, const unsigned int itrk, float newVal);
  void set_muIDOOhits(const short iroad, const unsigned int itrk, int newVal);
  void set_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk, float newVal);
  void set_muIDhits(const unsigned int itrk, int newVal);
  void set_muID_gap0(const short arrayid, const unsigned int itrk, float newVal);
  void set_muID_proj_hit_dist(short gap, short orient, short hit, 
			      const unsigned int itrk, float newVal);
  void set_muID_proj_hit_size(short gap, short orient, short hit, 
			      const unsigned int itrk, short newVal);
  void set_TMutTrk_status(const unsigned int itrk, int newVal);
  
  /// set gap_coordinate charge difference
  void set_delta_q( const unsigned int coord_id, const unsigned int itrk, float value );
  
  /// set error on gap_coordinate charge difference
  void set_delta_q_error( const unsigned int coord_id, const unsigned int itrk, float value );

private:
  /**@name Variables

  */
  //@{
  /// Number of Muon Tracks in the event.
  int nMuoTracks;
  /// Muon Tracks Container.
  TClonesArray* MuoTracks;
  //@}

ClassDef(PHMuoTracksv8,1)
};
#endif
