#ifndef __PHDIMUOTRACKSV1_H
#define __PHDIMUOTRACKSV1_H
#include <iostream>
#include "PHObject.h"
#include "TClonesArray.h"
#include "PHMuoTracksOut.h"
#include "PHdiMuoTrackv1.h"
#include "PHMuoTracksv1.h"

/** Event Dimuon Candidates Container

This class contains all the dimuon candidates stored for each event. 
Dimuon candidates are obtained by doing a combinatorial pairing of Muon Tracks
stored in $MuoTracks$ branch. Dimuon candidates are then either opposite
sign or like sign dimuons.\\ 
Single dimuon candidate variable descriptions are given in section 4.2.
*/
class PHdiMuoTracksv1 : public PHMuoTracksOut
{
typedef PHMuoTracksv1 MuTr;

public:
  PHdiMuoTracksv1();
  virtual ~PHdiMuoTracksv1();

  //========== Standard functions of all virtual classes...
  void Reset(){nMuoTracks = 0; ndiMuoTracks = 0; Clear();}
  void identify(std::ostream &os=std::cout) const {
      os << "identify yourself:  PHdiMuoTracksv1 object" << std::endl;
      return;
  }
  void Clear(Option_t *option = ""){
      if (MuoTracks > 0) MuoTracks->Clear();
      if (diMuoTracks > 0) diMuoTracks->Clear();
  }
  int IsValid() const {return 1;}

  //================================= PHParticle cloning/copying etc...
  TObject* GetSingleParticle(unsigned int ipart);
  void AddPHParticle(unsigned int ipart, TObject *);
  TObject* GetSingleDimuon(unsigned int ipart);
  void AddPHDimuon(unsigned int ipart, TObject *);
  PHdiMuoTracksv1* clone() const;


  //======================================= Dimuon fillers
  int Set_DimuArraySize(const unsigned int ndimu){
      diMuoTracks->Expand(ndimu);
      return ndimu;
  }
  void AddPHDimuon(const unsigned int idimu){
      new ((*diMuoTracks)[idimu]) PHdiMuoTrackv1();
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
  int get_ndimu() const {return ndiMuoTracks;}  
  /// Return {\bf idimu}$^{th}$ ditrack {\bf ditrkIndex[arrayid]}.
  int get_ditrkIndex(short arrayid, const unsigned int idimu) const;
  /// Return {\bf idimu}$^{th}$ {\bf dimass}.
  float get_dimass(const unsigned int idimu) const; 
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dicharge}.
  int get_dicharge(const unsigned int idimu) const;
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dipx[arrayid]}.
  float get_dipx(short arrayid, const unsigned int idimu) const;
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dipy[arrayid]}.
  float get_dipy(short arrayid, const unsigned int idimu) const;
  /// Return {\bf idimu}$^{th}$ ditrack {\bf dipz[arrayid]}.
  float get_dipz(short arrayid, const unsigned int idimu) const;
  //@}

  // unhide functions from PHMuoTracksOut
  float get_dipx(unsigned) const { PHOOL_VIRTUAL_WARNING; return -9999; }
  float get_dipy(unsigned) const { PHOOL_VIRTUAL_WARNING; return -9999; }
  float get_dipz(unsigned) const { PHOOL_VIRTUAL_WARNING; return -9999; }
  void set_dipx(unsigned, float) { PHOOL_VIRTUAL_WARNING; }
  void set_dipy(unsigned, float) { PHOOL_VIRTUAL_WARNING; }
  void set_dipz(unsigned, float) { PHOOL_VIRTUAL_WARNING; }

  //===================================== Dimuon Mutators
  void set_ndimu(const unsigned int ndimu){ndiMuoTracks=ndimu;}
  void set_dimass(const unsigned int idimu, float newVal);
  void set_dicharge(const unsigned int idimu, int newVal);
  void set_ditrkIndex(short arrayid, const unsigned int idimu, int newVal);
  void set_dipx(short arrayid, const unsigned int idimu, float newVal);
  void set_dipy(short arrayid, const unsigned int idimu, float newVal);
  void set_dipz(short arrayid, const unsigned int idimu, float newVal);

  //====================================== Particle Fillers
  int set_TClonesArraySize(const unsigned int npart)
  {
      MuoTracks->Expand(npart);
      return npart;
  }

  void AddPHParticle(const unsigned int itrk)
  { new ((*MuoTracks)[itrk]) PHMuoTrackv1(); }

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
  
  float get_xpos(short arrayid, const unsigned int itrk) const;
  
  float get_ypos(short arrayid, const unsigned int itrk) const;
  
  float get_zpos(short arrayid, const unsigned int itrk) const;
  
  short get_nhits(const unsigned int itrk) const;
  
  float get_cov(short arrayid1, short arrayid2, const unsigned int itrk) const;
  
  float get_MuonConfidence(const unsigned int itrk) const;
  
  float get_PionConfidence(const unsigned int itrk) const;
  
  float get_chisquare(const unsigned int itrk) const;
  
  float get_ghostflag(const unsigned int itrk) const;

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
  
  void set_xpos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_ypos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_zpos(short arrayid, const unsigned int itrk, float newVal);
  
  void set_cov(short arrayid1, short arrayid2, const unsigned int itrk, float newVal);
  
  void set_MuonConfidence(const unsigned int itrk, float newVal);
  
  void set_PionConfidence(const unsigned int itrk, float newVal);
  
  void set_chisquare(const unsigned int itrk, float newVal);
  
  void set_ghostflag(const unsigned int itrk, float newVal);

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

ClassDef(PHdiMuoTracksv1,1)
};
#endif
