#ifndef __PHMUOTRACKSV1_H
#define __PHMUOTRACKSV1_H
#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHMuoTracksOut.h"
#include "PHMuoTrackv1.h"

/** Event muon tracks container

This class contains all the muon tracks stored for each event. Single track variable
descriptions are given in section 4.2.
*/
class PHMuoTracksv1 : public PHMuoTracksOut
{

public:
  PHMuoTracksv1();
  PHMuoTracksv1(TClonesArray* MuT);
  virtual ~PHMuoTracksv1();
  
  //========== Standard functions of all virtual classes...
  void Reset(){nMuoTracks = 0; Clear();}
  void identify(std::ostream &os=std::cout) const {
      os << "identify yourself:  PHMuoTracksv1 object" << std::endl;
      return;
  }
  void Clear(Option_t *option = ""){
      if (MuoTracks > 0) MuoTracks->Clear();
  }
  int IsValid() const {return 1;}

  //================================= PHParticle cloning/copying etc...
  TObject* GetSingleParticle(unsigned int ipart);
  void AddPHParticle(unsigned int ipart, TObject *);
  PHMuoTracksv1* clone() const;


  //====================================== Particle Fillers
  int set_TClonesArraySize(const unsigned int npart){
      MuoTracks->Expand(npart);
      return npart;
  }
  void AddPHParticle(const unsigned int itrk){
      new ((*MuoTracks)[itrk]) PHMuoTrackv1();
  }
  void RemovePHParticle(const unsigned int itrk){
      MuoTracks->RemoveAt(itrk);
      return;
  }

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
  /// Return {\bf itrk}$^{th}$ track {\bf chisquare}.
  float get_chisquare(const unsigned int itrk) const ;
  /// Return {\bf itrk}$^{th}$ track {\bf ghostflag}.
  float get_ghostflag(const unsigned int itrk) const;
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
  void set_xpos(short arrayid, const unsigned int itrk, float newVal);
  void set_ypos(short arrayid, const unsigned int itrk, float newVal);
  void set_zpos(short arrayid, const unsigned int itrk, float newVal);
  void set_cov(short arrayid1, short arrayid2, const unsigned int itrk, float newVal);
  void set_MuonConfidence(const unsigned int itrk, float newVal) ;
  void set_PionConfidence(const unsigned int itrk, float newVal) ;
  void set_chisquare(const unsigned int itrk, float newVal) ;
  void set_ghostflag(const unsigned int itrk, float newVal);

private:
  /**@name Variables

  */
  //@{
  /// Number of Muon Tracks in the event.
  int nMuoTracks;
  /// Muon Tracks Container.
  TClonesArray* MuoTracks;
  //@}

ClassDef(PHMuoTracksv1,1)
};
#endif
