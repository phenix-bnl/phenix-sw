#ifndef __PHDIMUOTRACKSv8_H
#define __PHDIMUOTRACKSv8_H
#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHMuoTracksOut.h"
#include "PHdiMuoTrackv4.h"
#include "PHMuoTracksv8.h"

/** Event Dimuon Candidates Container

This class contains all the dimuon candidates stored for each event. 
Dimuon candidates are obtained by doing a combinatorial pairing of Muon Tracks
stored in $MuoTracks$ branch. Dimuon candidates are then either opposite
sign or like sign dimuons.\\ 
Single dimuon candidate variable descriptions are given in section 4.2.
*/
class PHdiMuoTracksv8 : public PHMuoTracksOut
{

  public:
  
  //! destructor
  PHdiMuoTracksv8();
  
  //! constructor
  virtual ~PHdiMuoTracksv8();

  //! reset containers
  void Reset(){nMuoTracks = 0; ndiMuoTracks = 0; Clear();}
  
  //! identify object
  void identify(std::ostream &os=std::cout) const
  {
      os << "identify yourself:  PHdiMuoTracksv8 object" << std::endl;
      return;
  }
  
  //! clear containers
  void Clear(Option_t *option = "")
  {
      if (MuoTracks > 0) MuoTracks->Delete();
      if (diMuoTracks > 0) diMuoTracks->Delete();
  }
  
  //! object validity
  int IsValid() const {return 1;}

  //================================= PHParticle cloning/copying etc...
  TObject* GetSingleParticle(unsigned int ipart);
  void AddPHParticle(unsigned int ipart, TObject *);
  TObject* GetSingleDimuon(unsigned int ipart);
  void AddPHDimuon(unsigned int ipart, TObject *);
  PHdiMuoTracksv8* clone() const;

  //======================================= Dimuon fillers
  int Set_DimuArraySize(const unsigned int ndimu){
      diMuoTracks->Expand(ndimu);
      return ndimu;
  }
  void AddPHDimuon(const unsigned int idimu){
      new ((*diMuoTracks)[idimu]) PHdiMuoTrackv4();
  }
  void RemovePHDimuon(const unsigned int idimu){
      diMuoTracks->RemoveAt(idimu);
      return;
  }

  //==================================== Dimuon Accessors
  /**@name Access methods

  */
  //@{
  /// Return {\bf ndiMuoTracks}. 
  int get_ndimu() const 
  {return ndiMuoTracks;}  
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf ditrkIndex[arrayid]}.
  int get_ditrkIndex(short arrayid, const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ {\bf dimass}.
  float get_dimass(const unsigned int idimu) const; 
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dicharge}.
  int get_dicharge(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dipx}.
  float get_dipx(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dipy}.
  float get_dipy(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dipz}.
  float get_dipz(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_xpos}.
  float get_vtx_bp_xpos(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_ypos}.
  float get_vtx_bp_ypos(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_zpos}.
  float get_vtx_bp_zpos(const unsigned int idimu) const;
   
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_bp_dca}.
  float get_vtx_bp_dca(const unsigned int idimu) const;

  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_xpos}.
  float get_vtx_xpos(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_ypos}.
  float get_vtx_ypos(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_zpos}.
  float get_vtx_zpos(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_chrg_1}.
  float get_vtx_chrg_1(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_px_1}.
  float get_vtx_px_1(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_py_1}.
  float get_vtx_py_1(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_pz_1}.
  float get_vtx_pz_1(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_chrg_2}.
  float get_vtx_chrg_2(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_px_2}.
  float get_vtx_px_2(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_py_2}.
  float get_vtx_py_2(const unsigned int idimu) const;
  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf vtx_pz_2}.
  float get_vtx_pz_2(const unsigned int idimu) const;
  
  
  /// Return {\bf idimu}$^{th}$ reduced number of degrees of freedom
  int get_vtx_ndf(const unsigned int idimu) const;
   
  
  /// Return {\bf idimu}$^{th}$ reduced chisquare from vertex fit.
  float get_vtx_chisquare(const unsigned int idimu) const;
 
  /// Return {\bf idimu}$^{th}$ covariance matrix from vertex fit
  float get_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu) const;
  //@}

  //===================================== Dimuon Mutators
  void set_ndimu(const unsigned int ndimu)
  {ndiMuoTracks=ndimu;}
  
  void set_dimass(const unsigned int idimu, float newVal);
  
  void set_dicharge(const unsigned int idimu, int newVal);
  
  void set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal);
  
  void set_dipx(const unsigned int idimu, float newVal);
  
  void set_dipy(const unsigned int idimu, float newVal);
  
  void set_dipz(const unsigned int idimu, float newVal);
  
  void set_vtx_bp_xpos(const unsigned int idimu, float newVal); 
  
  void set_vtx_bp_ypos(const unsigned int idimu, float newVal); 
  
  void set_vtx_bp_zpos(const unsigned int idimu, float newVal); 

  void set_vtx_bp_dca(const unsigned int idimu, float newVal); 
  
  void set_vtx_xpos(const unsigned int idimu, float newVal); 
  
  void set_vtx_ypos(const unsigned int idimu, float newVal); 
  
  void set_vtx_zpos(const unsigned int idimu, float newVal); 
  
  void set_vtx_chrg_1(const unsigned int idimu, float newVal); 
  
  void set_vtx_px_1(const unsigned int idimu, float newVal); 
  
  void set_vtx_py_1(const unsigned int idimu, float newVal);
  
  void set_vtx_pz_1(const unsigned int idimu, float newVal);
  
  void set_vtx_chrg_2(const unsigned int idimu, float newVal); 
  
  void set_vtx_px_2(const unsigned int idimu, float newVal); 
  
  void set_vtx_py_2(const unsigned int idimu, float newVal);
  
  void set_vtx_pz_2(const unsigned int idimu, float newVal);
  
  //! sets vertex number of degrees of freedom
  void set_vtx_ndf(const unsigned int idimu, int newVal);

  //! sets vertex reduced chisquare
  void set_vtx_chisquare(const unsigned int idimu, float newVal);
  
  void set_vtx_cov(short arrayid1, short arrayid2, const unsigned int idimu, float newVal);

  //====================================== Particle Fillers
  int set_TClonesArraySize(const unsigned int npart)
  {
      MuoTracks->Expand(npart);
      return npart;
  }
  
  void AddPHParticle(const unsigned int itrk)
  { new ((*MuoTracks)[itrk]) PHMuoTrackv8(); }
  
  void RemovePHParticle(const unsigned int itrk)
  {
      MuoTracks->RemoveAt(itrk);
      return;
  }

  //=================================== Particle Accessors
  unsigned int get_npart() const 
      {return nMuoTracks;}
  float get_px(const unsigned int itrk) const
      {return get_px(0,(const unsigned int)itrk);}
  float get_py(const unsigned int itrk) const 
      {return get_py(0,(const unsigned int)itrk);} 
  float get_pz(const unsigned int itrk) const 
      {return get_pz(0,(const unsigned int)itrk);}
  short get_PID(const unsigned int itrk) const;
  short get_charge(const unsigned int itrk) const;
  float get_px(short arrayid, const unsigned int itrk) const;
  float get_py(short arrayid, const unsigned int itrk) const;
  float get_pz(short arrayid, const unsigned int itrk) const;

  float get_st1_bp_P(short arrayid, const unsigned int itrk) const;
  float get_st1_bp_pos(short arrayid, const unsigned int itrk) const;
  float get_xpos(short arrayid, const unsigned int itrk) const;
  float get_ypos(short arrayid, const unsigned int itrk) const;
  float get_zpos(short arrayid, const unsigned int itrk) const;
  short get_nhits(const unsigned int itrk) const;
  float get_cov(short arrayid1, short arrayid2, const unsigned int itrk) const;
  float get_MuonConfidence(const unsigned int itrk) const; 
  float get_PionConfidence(const unsigned int itrk) const; 
  
  //! returns track number of degrees of freedom
  int get_ndf(const unsigned int itrk) const; 
  
  //! returns track reduced chisquare
  float get_chisquare(const unsigned int itrk) const; 
  
  float get_ghostflag(const unsigned int itrk) const;

  int get_muTRhits(const unsigned int itrk) const;

  float get_muIDOOchi(const short iroad, const unsigned int itrk) const;

  int get_muIDOOhits(const short iroad, const unsigned int itrk) const;

  float get_muIDOO_gap0(const short arrayid, const short iroad, const unsigned int itrk) const;

  int get_muIDhits(const unsigned int itrk) const;

  float get_muID_gap0(const short arrayid, const unsigned int itrk) const;

  float get_muID_proj_hit_dist(short gap, short orient, short hit, 
			       const unsigned int itrk) const;

  short get_muID_proj_hit_size(short gap, short orient, short hit, 
			       const unsigned int itrk) const;

  int get_TMutTrk_status(const unsigned int itrk) const;
 
  /// return gap_coordinate charge difference
  float get_delta_q( const unsigned int coord_id, const unsigned int itrk ) const;
  
  /// return error on gap_coordinate charge difference
  float get_delta_q_error( const unsigned int coord_id, const unsigned int itrk ) const;


  //=================================== Particle Mutators
  void set_npart(const unsigned int npart) 
      {nMuoTracks=npart;}

  void set_px(const unsigned int itrk, const float newVal)
      {set_px(0,(const unsigned short)itrk,newVal);}

  void set_py(const unsigned int itrk, const float newVal)
      {set_py(0,(const unsigned short)itrk,newVal);}

  void set_pz(const unsigned int itrk, const float newVal)
      {set_pz(0,(const unsigned short)itrk,newVal);}

  void set_nhits(const unsigned int itrk, const short newVal);

  void set_PID(const unsigned int itrk, const short newVal);

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

  void set_MuonConfidence(const unsigned int itrk, float newVal);
  
  void set_PionConfidence(const unsigned int itrk, float newVal);

  //! sets track number of degrees of freedom
  void set_ndf(const unsigned int itrk, int newVal); 
  
  //! sets track reduced chisquare
  void set_chisquare(const unsigned int itrk, float newVal); 

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
  /// Muon Tracks Branch.
  TClonesArray* MuoTracks;
  /// Number of Dimuon Candidates in the event.
  int ndiMuoTracks;
  /// Dimuon Candidates Branch.
  TClonesArray* diMuoTracks;
  //@}

ClassDef(PHdiMuoTracksv8,1)
};
#endif
