#ifndef __PHCENTRALTRACKV4_H
#define __PHCENTRALTRACKV4_H

#include <iostream>
#include "PHCentralTrack.h"

class TClonesArray;

//
//  This class is a so-called "versioned" object.  We are
//  inheriting from the PHCentralTrack.  That virtual base 
//  class has fields which are specific to central arm tracks.
//  However, since the PHCentralTrack inherits from PHParticle,
//  we get our 4-vector from there.
//
//  Following the PHENIX Schema Evolution system we choose to
//  make meaningful implementations of all those fields which we
//  want to keep in this particular version of a PHCentralTrack.
//
//  We also chose to *not* override functions that we don't want.
//  As examples of these, I conclude that since we don't make final
//  decisions on the PID in this version of track, we should not 
//  implement either the PID function (int ID number of particle),
//  nor should we implement the Emass (Energy of the particle based
//  upon the known mass).  For that reason, I have commented out the 
//  lines which would have implemented these methods.
//
//  The name of this file is not really chosen by a particle type.
//  The name refers to the fact that the CentralTrack working group
//  will be making nDSTs containing PHCentralTracks.  
//                                 TKH 3-7-2002
//
//  This is the "v4" version which is the candidate for CNT tracks
//  that we will keep in the QM2002 analysis.
//                                 TKH 4-17-2002
//
class PHCentralTrackv4 : public PHCentralTrack
{
 public:
  PHCentralTrackv4(int fetch=0);
  virtual ~PHCentralTrackv4();

  // The "standard response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Actual implementations of the "set" methods...
  // First the methods from PHParticle:
  void set_npart      (const unsigned int NTRACK) {nCentral = NTRACK; return;}
  void set_px         (const unsigned int itrk, const float val);
  void set_py         (const unsigned int itrk, const float val);
  void set_pz         (const unsigned int itrk, const float val);
  //void set_E          (const unsigned int itrk, const float val);
  void set_charge     (const unsigned int itrk, const short val);
  //void set_PID        (const unsigned int itrk, const short val);

  // Then implement the methods specific to CentralTracks:
  void set_quality    (const unsigned int itrk, const short val);
  void set_zed        (const unsigned int itrk, const float val);
  void set_phi        (const unsigned int itrk, const float val);
  void set_alpha      (const unsigned int itrk, const float val);
  void set_beta	      (const unsigned int itrk, const float val);
  void set_mom	      (const unsigned int itrk, const float val);
  void set_the0	      (const unsigned int itrk, const float val);
  void set_phi0	      (const unsigned int itrk, const float val);
  void set_status     (const unsigned int itrk, const short val);
  void set_alpha1     (const unsigned int itrk, const float val);
  void set_alpha2     (const unsigned int itrk, const float val);
  void set_nx1hits    (const unsigned int itrk, const short val);
  void set_nx2hits    (const unsigned int itrk, const short val);
  void set_nx1x2fit   (const unsigned int itrk, const short val);
  void set_alphaf     (const unsigned int itrk, const float val);
  void set_ppc1x      (const unsigned int itrk, const float val);
  void set_ppc1y      (const unsigned int itrk, const float val);
  void set_ppc1z      (const unsigned int itrk, const float val);
  void set_ppc2x      (const unsigned int itrk, const float val);
  void set_ppc2y      (const unsigned int itrk, const float val);
  void set_ppc2z      (const unsigned int itrk, const float val);
  void set_ptecx      (const unsigned int itrk, const float val);
  void set_ptecy      (const unsigned int itrk, const float val);
  void set_ptecz      (const unsigned int itrk, const float val);
  void set_ppc3x      (const unsigned int itrk, const float val);
  void set_ppc3y      (const unsigned int itrk, const float val);
  void set_ppc3z      (const unsigned int itrk, const float val);
  void set_pemcx      (const unsigned int itrk, const float val);
  void set_pemcy      (const unsigned int itrk, const float val);
  void set_pemcz      (const unsigned int itrk, const float val);
  void set_ptofx      (const unsigned int itrk, const float val);
  void set_ptofy      (const unsigned int itrk, const float val);
  void set_ptofz      (const unsigned int itrk, const float val);
  void set_pltof      (const unsigned int itrk, const float val);
  void set_plemc      (const unsigned int itrk, const float val);
  void set_sect       (const unsigned int itrk, const short val);
  void set_ysect      (const unsigned int itrk, const short val);
  void set_zsect      (const unsigned int itrk, const short val);
  void set_ecorr      (const unsigned int itrk, const float val);
  void set_ecore      (const unsigned int itrk, const float val);
  void set_temc       (const unsigned int itrk, const float val);
  void set_prob       (const unsigned int itrk, const float val);
  void set_ecent      (const unsigned int itrk, const float val);
  void set_twrhit     (const unsigned int itrk, const short val);
  void set_e9         (const unsigned int itrk, const float val);
  void set_re9        (const unsigned int itrk, const float val);
  void set_emcchi2    (const unsigned int itrk, const float val);
  void set_secorr     (const unsigned int itrk, const float val);
  void set_secore     (const unsigned int itrk, const float val);
  void set_stemc      (const unsigned int itrk, const float val);
  void set_sprob      (const unsigned int itrk, const float val);
  void set_stwrhit    (const unsigned int itrk, const short val);
  void set_semcchi2   (const unsigned int itrk, const float val);
  void set_slat       (const unsigned int itrk, const int   val);
  void set_ttof       (const unsigned int itrk, const float val);
  void set_etof       (const unsigned int itrk, const float val);
  void set_sttof      (const unsigned int itrk, const float val);
  void set_setof      (const unsigned int itrk, const float val);
  void set_n0         (const unsigned int itrk, const short val);
  void set_npe0       (const unsigned int itrk, const float val);
  void set_n1         (const unsigned int itrk, const short val);
  void set_npe1       (const unsigned int itrk, const float val);
  void set_chi2       (const unsigned int itrk, const float val);
  void set_disp       (const unsigned int itrk, const float val);
  void set_tcrk       (const unsigned int itrk, const float val);
  void set_cross_phi  (const unsigned int itrk, const float val);
  void set_cross_z    (const unsigned int itrk, const float val);
  void set_center_phi (const unsigned int itrk, const float val);
  void set_center_z   (const unsigned int itrk, const float val);
  void set_sn0        (const unsigned int itrk, const short val);
  void set_snpe0      (const unsigned int itrk, const float val);
  void set_sn1        (const unsigned int itrk, const short val);
  void set_snpe1      (const unsigned int itrk, const float val);
  void set_schi2      (const unsigned int itrk, const float val);
  void set_sdisp      (const unsigned int itrk, const float val);
  void set_stcrk      (const unsigned int itrk, const float val);
  void set_tecdedx1   (const unsigned int itrk, const float val);
  void set_tecdedx2   (const unsigned int itrk, const float val);
  void set_pc2sdphi   (const unsigned int itrk, const float val);
  void set_pc2sdz     (const unsigned int itrk, const float val);
  void set_pc3sdphi   (const unsigned int itrk, const float val);
  void set_pc3sdz     (const unsigned int itrk, const float val);
  void set_emcsdphi   (const unsigned int itrk, const float val);
  void set_emcsdz     (const unsigned int itrk, const float val);
  void set_tofsdphi   (const unsigned int itrk, const float val);
  void set_tofsdz     (const unsigned int itrk, const float val);
  void set_tecsdphi   (const unsigned int itrk, const float val);
  void set_tecsdalpha (const unsigned int itrk, const float val);
  void set_spc2sdphi  (const unsigned int itrk, const float val);
  void set_spc2sdz    (const unsigned int itrk, const float val);
  void set_spc3sdphi  (const unsigned int itrk, const float val);
  void set_spc3sdz    (const unsigned int itrk, const float val);
  void set_semcsdphi  (const unsigned int itrk, const float val);
  void set_semcsdz    (const unsigned int itrk, const float val);
  void set_stofsdphi  (const unsigned int itrk, const float val);
  void set_stofsdz    (const unsigned int itrk, const float val);
  void set_stecsdphi  (const unsigned int itrk, const float val);
  void set_stecsdalpha(const unsigned int itrk, const float val);
  void set_m2tof      (const unsigned int itrk, const float val);
  void set_m2emc      (const unsigned int itrk, const float val);
  void set_isPi       (const unsigned int itrk, const float val);
  void set_isK        (const unsigned int itrk, const float val);
  void set_isP        (const unsigned int itrk, const float val);
  void set_categoryl2eLowPt(const unsigned int itrk, const long input); 
  void set_categoryl2eHighPt(const unsigned int itrk, const long input); 
  void set_candIDl2e(const unsigned int itrk, const short input);  
  void set_dcarm      (const unsigned int itrk, const short val);
  void set_dcside     (const unsigned int itrk, const short val);
  void set_pc1sect    (const unsigned int itrk, const short val);
  void set_pc2sect    (const unsigned int itrk, const short val);
  void set_pc3sect    (const unsigned int itrk, const short val);
  void set_emcsdphi_e (const unsigned int itrk, const float val);
  void set_emcsdz_e   (const unsigned int itrk, const float val);
  void set_semcsdphi_e(const unsigned int itrk, const float val);
  void set_semcsdz_e  (const unsigned int itrk, const float val);
  void set_tecnhit    (const unsigned int itrk, const short val);
  void set_mx1dist    (const unsigned int itrk, const float val);
  void set_mx2dist    (const unsigned int itrk, const float val);
  void set_mchi2      (const unsigned int itrk, const float val);
  void set_n2	      (const unsigned int itrk, const short val);
  void set_npe2	      (const unsigned int itrk, const float val);
  void set_n3	      (const unsigned int itrk, const short val);
  void set_npe3	      (const unsigned int itrk, const float val);
  void set_sn2	      (const unsigned int itrk, const short val);
  void set_snpe2      (const unsigned int itrk, const float val);
  void set_sn3	      (const unsigned int itrk, const short val);
  void set_snpe3      (const unsigned int itrk, const float val);
  void set_deadmap    (const unsigned int itrk, const int   val);
  void set_warnmap    (const unsigned int itrk, const int   val);
  void set_sdeadmap   (const unsigned int itrk, const int   val);
  void set_swarnmap   (const unsigned int itrk, const int   val);


  // Actual implementations of the "get" methods...
  unsigned int   get_npart() const {return nCentral;}
  float get_px         (const unsigned int itrk) const;
  float get_py         (const unsigned int itrk) const;
  float get_pz         (const unsigned int itrk) const;
  //float get_E          (const unsigned int itrk) const;
  short get_charge     (const unsigned int itrk) const;
  //short get_PID        (const unsigned int itrk) const;

  short get_quality    (const unsigned int itrk) const;
  float get_zed        (const unsigned int itrk) const;
  float get_phi        (const unsigned int itrk) const;
  float get_alpha      (const unsigned int itrk) const;
  float get_beta       (const unsigned int itrk) const;
  float get_mom        (const unsigned int itrk) const;
  float get_the0       (const unsigned int itrk) const;
  float get_phi0       (const unsigned int itrk) const;
  short get_status     (const unsigned int itrk) const;
  float get_alpha1     (const unsigned int itrk) const;
  float get_alpha2     (const unsigned int itrk) const;
  short get_nx1hits    (const unsigned int itrk) const;
  short get_nx2hits    (const unsigned int itrk) const;
  short get_nx1x2fit   (const unsigned int itrk) const;
  float get_alphaf     (const unsigned int itrk) const;
  float get_ppc1x      (const unsigned int itrk) const;
  float get_ppc1y      (const unsigned int itrk) const;
  float get_ppc1z      (const unsigned int itrk) const;
  float get_ppc2x      (const unsigned int itrk) const;
  float get_ppc2y      (const unsigned int itrk) const;
  float get_ppc2z      (const unsigned int itrk) const;
  float get_ptecx      (const unsigned int itrk) const;
  float get_ptecy      (const unsigned int itrk) const;
  float get_ptecz      (const unsigned int itrk) const;
  float get_ppc3x      (const unsigned int itrk) const;
  float get_ppc3y      (const unsigned int itrk) const;
  float get_ppc3z      (const unsigned int itrk) const;
  float get_pemcx      (const unsigned int itrk) const;
  float get_pemcy      (const unsigned int itrk) const;
  float get_pemcz      (const unsigned int itrk) const;
  float get_ptofx      (const unsigned int itrk) const;
  float get_ptofy      (const unsigned int itrk) const;
  float get_ptofz      (const unsigned int itrk) const;
  float get_pltof      (const unsigned int itrk) const;
  float get_plemc      (const unsigned int itrk) const;
  short get_sect       (const unsigned int itrk) const;
  short get_ysect      (const unsigned int itrk) const;
  short get_zsect      (const unsigned int itrk) const;
  float get_ecorr      (const unsigned int itrk) const;
  float get_ecore      (const unsigned int itrk) const;
  float get_temc       (const unsigned int itrk) const;
  float get_prob       (const unsigned int itrk) const;
  float get_ecent      (const unsigned int itrk) const;
  short get_twrhit     (const unsigned int itrk) const;
  float get_e9         (const unsigned int itrk) const;
  float get_re9        (const unsigned int itrk) const;
  float get_emcchi2    (const unsigned int itrk) const;
  float get_secorr     (const unsigned int itrk) const;
  float get_secore     (const unsigned int itrk) const;
  float get_stemc      (const unsigned int itrk) const;
  float get_sprob      (const unsigned int itrk) const;
  short get_stwrhit    (const unsigned int itrk) const;
  float get_semcchi2   (const unsigned int itrk) const;
  int   get_slat       (const unsigned int itrk) const;
  float get_ttof       (const unsigned int itrk) const;
  float get_etof       (const unsigned int itrk) const;
  float get_sttof      (const unsigned int itrk) const;
  float get_setof      (const unsigned int itrk) const;
  short get_n0         (const unsigned int itrk) const;
  float get_npe0       (const unsigned int itrk) const;
  short get_n1         (const unsigned int itrk) const;
  float get_npe1       (const unsigned int itrk) const;
  float get_chi2       (const unsigned int itrk) const;
  float get_disp       (const unsigned int itrk) const;
  float get_tcrk       (const unsigned int itrk) const;
  float get_cross_phi  (const unsigned int itrk) const;
  float get_cross_z    (const unsigned int itrk) const;
  float get_center_phi (const unsigned int itrk) const;
  float get_center_z   (const unsigned int itrk) const;
  short get_sn0        (const unsigned int itrk) const;
  float get_snpe0      (const unsigned int itrk) const;
  short get_sn1        (const unsigned int itrk) const;
  float get_snpe1      (const unsigned int itrk) const;
  float get_schi2      (const unsigned int itrk) const;
  float get_sdisp      (const unsigned int itrk) const;
  float get_stcrk      (const unsigned int itrk) const;
  float get_tecdedx1   (const unsigned int itrk) const;
  float get_tecdedx2   (const unsigned int itrk) const;
  float get_pc2sdphi   (const unsigned int itrk) const;
  float get_pc2sdz     (const unsigned int itrk) const;
  float get_pc3sdphi   (const unsigned int itrk) const;
  float get_pc3sdz     (const unsigned int itrk) const;
  float get_emcsdphi   (const unsigned int itrk) const;
  float get_emcsdz     (const unsigned int itrk) const;
  float get_tofsdphi   (const unsigned int itrk) const;
  float get_tofsdz     (const unsigned int itrk) const;
  float get_tecsdphi   (const unsigned int itrk) const;
  float get_tecsdalpha (const unsigned int itrk) const;
  float get_spc2sdphi  (const unsigned int itrk) const;
  float get_spc2sdz    (const unsigned int itrk) const;
  float get_spc3sdphi  (const unsigned int itrk) const;
  float get_spc3sdz    (const unsigned int itrk) const;
  float get_semcsdphi  (const unsigned int itrk) const;
  float get_semcsdz    (const unsigned int itrk) const;
  float get_stofsdphi  (const unsigned int itrk) const;
  float get_stofsdz    (const unsigned int itrk) const;
  float get_stecsdphi  (const unsigned int itrk) const;
  float get_stecsdalpha(const unsigned int itrk) const;
  float get_m2tof      (const unsigned int itrk) const;
  float get_m2emc      (const unsigned int itrk) const;
  float get_isPi       (const unsigned int itrk) const;
  float get_isK        (const unsigned int itrk) const;
  float get_isP        (const unsigned int itrk) const;
  long get_categoryl2eLowPt(const unsigned int itrk) const;
  long get_categoryl2eHighPt(const unsigned int itrk) const;
  short get_candIDl2e(const unsigned int itrk) const;
  short get_dcarm      (const unsigned int itrk) const;
  short get_dcside     (const unsigned int itrk) const;
  short get_pc1sect    (const unsigned int itrk) const;
  short get_pc2sect    (const unsigned int itrk) const;
  short get_pc3sect    (const unsigned int itrk) const;
  float get_emcsdphi_e (const unsigned int itrk) const;
  float get_emcsdz_e   (const unsigned int itrk) const;
  float get_semcsdphi_e(const unsigned int itrk) const;
  float get_semcsdz_e  (const unsigned int itrk) const;
  short get_tecnhit    (const unsigned int itrk) const;
  float get_mx1dist    (const unsigned int itrk) const;
  float get_mx2dist    (const unsigned int itrk) const;
  float get_mchi2      (const unsigned int itrk) const;
  short get_n2	       (const unsigned int itrk) const;
  float get_npe2       (const unsigned int itrk) const;
  short get_n3	       (const unsigned int itrk) const;
  float get_npe3       (const unsigned int itrk) const;
  short get_sn2	       (const unsigned int itrk) const;
  float get_snpe2      (const unsigned int itrk) const;
  short get_sn3	       (const unsigned int itrk) const;
  float get_snpe3      (const unsigned int itrk) const;
  int   get_deadmap    (const unsigned int itrk) const;
  int   get_warnmap    (const unsigned int itrk) const;
  int   get_sdeadmap   (const unsigned int itrk) const;
  int   get_swarnmap   (const unsigned int itrk) const;

  // Routines to manipulate the particle array
  int set_TClonesArraySize(const unsigned int ntrk);
  void AddPHParticle      (const unsigned int itrk);
  void RemovePHParticle   (const unsigned int itrk);

 protected:
  TClonesArray *GetCentral() const {return Central;}
  unsigned int nCentral;
  TClonesArray *Central;

  float sigmaAlpha;
  float sigmaMultScatt;
  float sigmaTOF;
  float sigmaEMC;
  float k1;

  ClassDef(PHCentralTrackv4,1)
};

#endif /* __PHCENTRALTRACKV4_H */






