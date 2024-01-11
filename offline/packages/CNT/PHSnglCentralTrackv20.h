#ifndef __PHSNGLCENTRALTRACKV20_H_
#define __PHSNGLCENTRALTRACKV20_H_

#include "PHObject.h"
#include "PHSnglCentralTrack.h"

class PHSnglCentralTrackv20 : public PHSnglCentralTrack
{
 public:
  PHSnglCentralTrackv20();
  PHSnglCentralTrackv20(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv20() {}

  void identify(std::ostream &os=std::cout) const;

  // Here are the very explicit set routines...
  void set_charge     (const short val) {charge     = val; return;}
  void set_quality    (const short val) {quality    = val; return;}
  void set_zed        (const float val) {zed        = val; return;}
  void set_phi        (const float val) {phi        = val; return;}
  void set_alpha      (const float val) {alpha      = val; return;}
  void set_beta       (const float val) {beta       = val; return;}
  void set_phi0       (const float val) {phi0       = val; return;}
  void set_the0       (const float val) {the0       = val; return;}
  void set_mom        (const float val) {mom        = val; return;}
  void set_status     (const short val) {status     = val; return;}
  void set_alpha1     (const float val) {alpha1     = val; return;}
  void set_alpha2     (const float val) {alpha2     = val; return;}
  void set_nx1hits    (const short val) {nx1hits    = val; return;}
  void set_nx2hits    (const short val) {nx2hits    = val; return;}
  void set_ppc1x      (const float val) {ppc1x      = val; return;}
  void set_ppc1y      (const float val) {ppc1y      = val; return;}
  void set_ppc1z      (const float val) {ppc1z      = val; return;}
  void set_ppc2x      (const float val) {ppc2x      = val; return;}
  void set_ppc2y      (const float val) {ppc2y      = val; return;}
  void set_ppc2z      (const float val) {ppc2z      = val; return;}
  void set_ptecx      (const float val) {ptecx      = val; return;}
  void set_ptecy      (const float val) {ptecy      = val; return;}
  void set_ptecz      (const float val) {ptecz      = val; return;}
  void set_ppc3x      (const float val) {ppc3x      = val; return;}
  void set_ppc3y      (const float val) {ppc3y      = val; return;}
  void set_ppc3z      (const float val) {ppc3z      = val; return;}
  void set_pemcx      (const float val) {pemcx      = val; return;}
  void set_pemcy      (const float val) {pemcy      = val; return;}
  void set_pemcz      (const float val) {pemcz      = val; return;}
  void set_ptofx      (const float val) {ptofx      = val; return;}
  void set_ptofy      (const float val) {ptofy      = val; return;}
  void set_ptofz      (const float val) {ptofz      = val; return;}
  void set_phbdx      (const float val) {phbdx      = val; return;}
  void set_phbdy      (const float val) {phbdy      = val; return;}
  void set_phbdz      (const float val) {phbdz      = val; return;}
  void set_ptofwx     (const float val) {ptofwx     = val; return;}
  void set_ptofwy     (const float val) {ptofwy     = val; return;}
  void set_ptofwz     (const float val) {ptofwz     = val; return;}
  void set_pltof      (const float val) {pltof      = val; return;}
  void set_plemc      (const float val) {plemc      = val; return;}
  void set_pltofw     (const float val) {pltofw     = val; return;}

  // emc
  void set_sect       (const short val) {sect       = val; return;}
  void set_ysect      (const short val) {ysect      = val; return;}
  void set_zsect      (const short val) {zsect      = val; return;}
  void set_ecore      (const float val) {ecore      = val; return;}
  void set_emce       (const float val) {emce       = val; return;}
  void set_emcdispy   (const float val) {emcdispy   = val; return;}
  void set_emcdispz   (const float val) {emcdispz   = val; return;}
  void set_emcrawtdc  (const int val)   {emcrawtdc  = val; return;}
  void set_emcrawadc  (const int val)   {emcrawadc  = val; return;}
  void set_emcrawadclg(const int val)   {emcrawadclg= val; return;}
  void set_temc       (const float val) {temc       = val; return;}
  void set_prob       (const float val) {prob       = val; return;}
  void set_m2emc      (const float val) {m2emc      = val; return;}
  void set_ecent      (const float val) {ecent      = val; return;}
  void set_twrhit     (const short val) {twrhit     = val; return;}
  void set_e9         (const float val) {e9         = val; return;}
  void set_re9        (const float val) {re9        = val; return;}
  void set_emcchi2    (const float val) {emcchi2    = val; return;}
  void set_deadmap    (const int val)   {deadmap    = val; return;}
  void set_warnmap    (const int val)   {warnmap    = val; return;}

  // swapped emc
  void set_secore     (const float val) {secore     = val; return;}
  void set_semce      (const float val) {semce      = val; return;}
  void set_semcdispy  (const float val) {semcdispy  = val; return;}
  void set_semcdispz  (const float val) {semcdispz  = val; return;}
  void set_stemc      (const float val) {stemc      = val; return;}
  void set_sprob      (const float val) {sprob      = val; return;}
  void set_secent     (const float val) {secent     = val; return;}
  void set_stwrhit    (const short val) {stwrhit    = val; return;}
  void set_se9        (const float val) {se9        = val; return;}
  void set_semcchi2   (const float val) {semcchi2   = val; return;}
  void set_sdeadmap   (const int val)   {sdeadmap   = val; return;}
  void set_swarnmap   (const int val)   {swarnmap   = val; return;}

  // tof
  void set_slat       (const int   val) {slat       = val; return;}
  void set_ttof       (const float val) {ttof       = val; return;}
  void set_etof       (const float val) {etof       = val; return;}
  void set_tofph1     (const float val) {tofph1     = val; return;}
  void set_tofph2     (const float val) {tofph2     = val; return;}
  void set_toftdc1    (const float val) {toftdc1    = val; return;}
  void set_toftdc2    (const float val) {toftdc2    = val; return;}

  // swapped tof
  void set_sttof      (const float val) {sttof      = val; return;}
  void set_setof      (const float val) {setof      = val; return;}

  // tec
  void set_tecid      (const short val) {tecid      = val; return;}

  // swapped tec
  void set_stecid     (const short val) {stecid     = val; return;}

  // crk
  void set_n0         (const short val) {n0         = val; return;}
  void set_npe0       (const float val) {npe0       = val; return;}
  void set_n1         (const short val) {n1         = val; return;}
  void set_npe1       (const float val) {npe1       = val; return;}
  void set_n2	      (const short val) {n2	    = val; return;}
  void set_npe2	      (const float val) {npe2	    = val; return;}
  void set_n3	      (const short val) {n3	    = val; return;}
  void set_npe3	      (const float val) {npe3	    = val; return;}
  void set_chi2       (const float val) {chi2       = val; return;}
  void set_disp       (const float val) {disp       = val; return;}
  void set_tcrk       (const float val) {tcrk       = val; return;}
  void set_cross_phi  (const float val) {cross_phi  = val; return;}
  void set_cross_z    (const float val) {cross_z    = val; return;}
  void set_center_phi (const float val) {center_phi = val; return;}
  void set_center_z   (const float val) {center_z   = val; return;}

  // swapped crk
  void set_sn0        (const short val) {sn0        = val; return;}
  void set_snpe0      (const float val) {snpe0      = val; return;}
  void set_sn1        (const short val) {sn1        = val; return;}
  void set_snpe1      (const float val) {snpe1      = val; return;}
  void set_sn2	      (const short val) {sn2	    = val; return;}
  void set_snpe2      (const float val) {snpe2	    = val; return;}
  void set_sn3	      (const short val) {sn3	    = val; return;}
  void set_snpe3      (const float val) {snpe3	    = val; return;}
  void set_schi2      (const float val) {schi2      = val; return;}
  void set_sdisp      (const float val) {sdisp      = val; return;}
  void set_stcrk      (const float val) {stcrk      = val; return;}
  void set_scross_phi (const float val) {scross_phi = val; return;}
  void set_scross_z   (const float val) {scross_z   = val; return;}

  // tofw
  void set_striptofw  (const int   val) {striptofw  = val; return;}
  void set_tofwx      (const float val) {tofwx      = val; return;}
  void set_tofwy      (const float val) {tofwy      = val; return;}
  void set_tofwz      (const float val) {tofwz      = val; return;}
  void set_ttofw      (const float val) {ttofw      = val; return;}  
  void set_qtofw      (const float val) {qtofw      = val; return;}
  void set_tofwtdcup  (const float val) {tofwtdcup  = val; return;}
  void set_tofwtdcdw  (const float val) {tofwtdcdw  = val; return;}
  void set_tofwadcup  (const float val) {tofwadcup  = val; return;}
  void set_tofwadcdw  (const float val) {tofwadcdw  = val; return;}
 
  // hbd
  void set_hbdsector  (const short val) {hbdsector  = val; return;}
  void set_hbdsize    (const short val) {hbdsize    = val; return;}
  void set_hbdcharge  (const float val) {hbdcharge  = val; return;}
  void set_hbdx       (const float val) {hbdx       = val; return;}
  void set_hbdy       (const float val) {hbdy       = val; return;}
  void set_hbdz       (const float val) {hbdz       = val; return;}
  
  // matching
  void set_tofdphi    (const float val) {tofdphi    = val; return;}
  void set_tofdz      (const float val) {tofdz      = val; return;}
  void set_tecdphi    (const float val) {tecdphi    = val; return;}
  void set_tecdalpha  (const float val) {tecdalpha  = val; return;}
  void set_pc2dphi    (const float val) {pc2dphi    = val; return;}
  void set_pc2dz      (const float val) {pc2dz      = val; return;}
  void set_pc3dphi    (const float val) {pc3dphi    = val; return;}
  void set_pc3dz      (const float val) {pc3dz      = val; return;}
  void set_emcdphi    (const float val) {emcdphi    = val; return;}
  void set_emcdz      (const float val) {emcdz      = val; return;}
  void set_hbddphi    (const float val) {hbddphi    = val; return;}
  void set_hbddz      (const float val) {hbddz      = val; return;}
  void set_tofwdphi   (const float val) {tofwdphi   = val; return;}
  void set_tofwdz     (const float val) {tofwdz     = val; return;}

  // sigmalized matching
  void set_tofsdphi   (const float val) {tofsdphi   = val; return;}
  void set_tofsdz     (const float val) {tofsdz     = val; return;}
  void set_tecsdphi   (const float val) {tecsdphi   = val; return;}
  void set_tecsdalpha (const float val) {tecsdalpha = val; return;}
  void set_pc2sdphi   (const float val) {pc2sdphi   = val; return;}
  void set_pc2sdz     (const float val) {pc2sdz     = val; return;}
  void set_pc3sdphi   (const float val) {pc3sdphi   = val; return;}
  void set_pc3sdz     (const float val) {pc3sdz     = val; return;}
  void set_emcsdphi   (const float val) {emcsdphi   = val; return;}
  void set_emcsdz     (const float val) {emcsdz     = val; return;}
  void set_emcsdphi_e (const float val) {emcsdphi_e = val; return;}
  void set_emcsdz_e   (const float val) {emcsdz_e   = val; return;}
  void set_hbdsdphi   (const float val) {hbdsdphi   = val; return;}
  void set_hbdsdz     (const float val) {hbdsdz     = val; return;}
  void set_tofwsdphi  (const float val) {tofwsdphi  = val; return;}
  void set_tofwsdz    (const float val) {tofwsdz    = val; return;}

  // swapped matching
  void set_stofdphi   (const float val) {stofdphi   = val; return;}
  void set_stofdz     (const float val) {stofdz     = val; return;}
  void set_stecdphi   (const float val) {stecdphi   = val; return;}
  void set_stecdalpha (const float val) {stecdalpha = val; return;}
  void set_spc2dphi   (const float val) {spc2dphi   = val; return;}
  void set_spc2dz     (const float val) {spc2dz     = val; return;}
  void set_spc3dphi   (const float val) {spc3dphi   = val; return;}
  void set_spc3dz     (const float val) {spc3dz     = val; return;}
  void set_semcdphi   (const float val) {semcdphi   = val; return;}
  void set_semcdz     (const float val) {semcdz     = val; return;}
  void set_semcdphi_e (const float val) {semcdphi_e = val; return;}
  void set_semcdz_e   (const float val) {semcdz_e   = val; return;}

  // swapped sigmalized matching
  void set_stofsdphi  (const float val) {stofsdphi  = val; return;}
  void set_stofsdz    (const float val) {stofsdz    = val; return;}
  void set_stecsdphi  (const float val) {stecsdphi  = val; return;}
  void set_stecsdalpha(const float val) {stecsdalpha= val; return;}
  void set_spc2sdphi  (const float val) {spc2sdphi  = val; return;}
  void set_spc2sdz    (const float val) {spc2sdz    = val; return;}
  void set_spc3sdphi  (const float val) {spc3sdphi  = val; return;}
  void set_spc3sdz    (const float val) {spc3sdz    = val; return;}
  void set_semcsdphi  (const float val) {semcsdphi  = val; return;}
  void set_semcsdz    (const float val) {semcsdz    = val; return;}
  void set_semcsdphi_e(const float val) {semcsdphi_e= val; return;}
  void set_semcsdz_e  (const float val) {semcsdz_e  = val; return;}

  // etc.
  void set_m2tof      (const float val) {m2tof      = val; return;}
  void set_m2tofw     (const float val) {m2tofw     = val; return;}
  void set_isPi       (const float val) {isPi       = val; return;}
  void set_isK        (const float val) {isK        = val; return;}
  void set_isP        (const float val) {isP        = val; return;}
  void set_dcarm      (const short val) {dcarm	    = val; return;}
  void set_dcside     (const short val) {dcside	    = val; return;}
  void set_pc1sect    (const short val) {pc1sect    = val; return;}
  void set_pc2sect    (const short val) {pc2sect    = val; return;}
  void set_pc3sect    (const short val) {pc3sect    = val; return;}
  void set_mx1dist    (const float val) {mx1dist    = val; return;}
  void set_mx2dist    (const float val) {mx2dist    = val; return;}
  void set_mchi2      (const float val) {mchi2	    = val; return;}
  void set_pc1id      (const short val) {pc1id      = val; return;}
  void set_pc2id      (const short val) {pc2id      = val; return;}
  void set_pc3id      (const short val) {pc3id      = val; return;}
  void set_emcid      (const short val) {emcid      = val; return;}
  void set_tofid      (const short val) {tofid      = val; return;}
  void set_tofwid     (const short val) {tofwid     = val; return;}
  void set_hbdid      (const short val) {hbdid      = val; return;}
  void set_ring       (const int val)   {ring       = val; return;}
  void set_mcid       (const int val)   {mcid       = val; return;}
  void set_dchid      (const int val)   {dchid      = val; return;}
  void set_aerindex   (const int val)   {aerindex   = val; return;}
  void set_aersindex  (const int val)   {aersindex  = val; return;}
  void set_candIDl2e  (const short input) {candIDl2e = input; return;}
  void set_categoryl2eLowPt(const long input) {categoryl2eLowPt = input; return;}
  void set_categoryl2eHighPt(const long input){categoryl2eHighPt = input; return;}

  // IDs to subsystem nodes
  void set_idtrk_tof  (const int val) {idtrk_tof    = val; return;}
  void set_idtrk_tec  (const int val) {idtrk_tec    = val; return;}
  void set_idtrk_crk  (const int val) {idtrk_crk    = val; return;}
  void set_idtrk_pc2  (const int val) {idtrk_pc2    = val; return;}
  void set_idtrk_pc3  (const int val) {idtrk_pc3    = val; return;}
  void set_idtrk_hbd  (const int val) {idtrk_hbd    = val; return;}
  void set_idtrk_tofw (const int val) {idtrk_tofw   = val; return;}

  // IDs to swapped subsystem nodes
  void set_idtrk_stof (const int val) {idtrk_stof   = val; return;}
  void set_idtrk_stec (const int val) {idtrk_stec   = val; return;}
  void set_idtrk_scrk (const int val) {idtrk_scrk   = val; return;}
  void set_idtrk_spc2 (const int val) {idtrk_spc2   = val; return;}
  void set_idtrk_spc3 (const int val) {idtrk_spc3   = val; return;}

  // Here are the very explicit "get" routines...
  short get_charge     () const { return charge       ;}
  short get_quality    () const { return quality      ;}
  float get_zed        () const { return zed          ;}
  float get_phi        () const { return phi          ;}
  float get_alpha      () const { return alpha        ;}
  float get_beta       () const { return beta         ;}
  float get_phi0       () const { return phi0         ;}
  float get_the0       () const { return the0         ;}
  float get_mom        () const { return mom          ;}
  short get_status     () const { return status       ;}
  float get_alpha1     () const { return alpha1       ;}
  float get_alpha2     () const { return alpha2       ;}
  short get_nx1hits    () const { return nx1hits      ;}
  short get_nx2hits    () const { return nx2hits      ;}
  float get_ppc1x      () const { return ppc1x        ;}
  float get_ppc1y      () const { return ppc1y        ;}
  float get_ppc1z      () const { return ppc1z        ;}
  float get_ppc2x      () const { return ppc2x        ;}
  float get_ppc2y      () const { return ppc2y        ;}
  float get_ppc2z      () const { return ppc2z        ;}
  float get_ptecx      () const { return ptecx        ;}
  float get_ptecy      () const { return ptecy        ;}
  float get_ptecz      () const { return ptecz        ;}
  float get_ppc3x      () const { return ppc3x        ;}
  float get_ppc3y      () const { return ppc3y        ;}
  float get_ppc3z      () const { return ppc3z        ;}
  float get_pemcx      () const { return pemcx        ;}
  float get_pemcy      () const { return pemcy        ;}
  float get_pemcz      () const { return pemcz        ;}
  float get_ptofx      () const { return ptofx        ;}
  float get_ptofy      () const { return ptofy        ;}
  float get_ptofz      () const { return ptofz        ;}
  float get_phbdx      () const { return phbdx        ;}
  float get_phbdy      () const { return phbdy        ;}
  float get_phbdz      () const { return phbdz        ;}
  float get_ptofwx     () const { return ptofwx       ;}
  float get_ptofwy     () const { return ptofwy       ;}
  float get_ptofwz     () const { return ptofwz       ;}
  float get_pltof      () const { return pltof        ;}
  float get_plemc      () const { return plemc        ;}
  float get_pltofw     () const { return pltofw       ;}

  // emc
  short get_sect       () const { return sect         ;}
  short get_ysect      () const { return ysect        ;}
  short get_zsect      () const { return zsect        ;}
  int   get_emcrawtdc  () const { return emcrawtdc    ;}
  int   get_emcrawadc  () const { return emcrawadc    ;}
  int   get_emcrawadclg() const { return emcrawadclg  ;}
  float get_ecore      () const { return ecore        ;}
  float get_emce       () const { return emce         ;}
  float get_emcdispy   () const { return emcdispy     ;}
  float get_emcdispz   () const { return emcdispz     ;}
  float get_temc       () const { return temc         ;}
  float get_prob       () const { return prob         ;}
  float get_ecent      () const { return ecent        ;}
  short get_twrhit     () const { return twrhit       ;}
  float get_e9         () const { return e9           ;}
  float get_re9        () const { return re9          ;}
  float get_emcchi2    () const { return emcchi2      ;}
  int   get_deadmap    () const { return deadmap      ;}
  int   get_warnmap    () const { return warnmap      ;}

  // swapped emc
  float get_secore     () const { return secore       ;}
  float get_semce      () const { return semce        ;}
  float get_semcdispy  () const { return semcdispy    ;}
  float get_semcdispz  () const { return semcdispz    ;}
  float get_stemc      () const { return stemc        ;}
  float get_sprob      () const { return sprob        ;}
  float get_secent     () const { return secent       ;}
  short get_stwrhit    () const { return stwrhit      ;}
  float get_se9        () const { return se9          ;}
  float get_semcchi2   () const { return semcchi2     ;}
  int   get_sdeadmap   () const { return sdeadmap     ;}
  int   get_swarnmap   () const { return swarnmap     ;}

  // tof
  int   get_slat       () const { return slat         ;}
  float get_ttof       () const { return ttof         ;}
  float get_etof       () const { return etof         ;}
  float get_tofph1     () const { return tofph1       ;}
  float get_tofph2     () const { return tofph2       ;}
  float get_toftdc1    () const { return toftdc1      ;}
  float get_toftdc2    () const { return toftdc2      ;}

  // swapped tof
  float get_sttof      () const { return sttof        ;}
  float get_setof      () const { return setof        ;}

  // tec
  short get_tecid      () const { return tecid        ;}

  // swapped tec
  short get_stecid     () const { return  stecid      ;}

  // crk
  short get_n0         () const { return n0           ;}
  float get_npe0       () const { return npe0         ;}
  short get_n1         () const { return n1           ;}
  float get_npe1       () const { return npe1         ;}
  short get_n2	       () const { return n2	      ;}
  float get_npe2       () const { return npe2	      ;}
  short get_n3	       () const { return n3	      ;}
  float get_npe3       () const { return npe3	      ;}
  float get_chi2       () const { return chi2         ;}
  float get_disp       () const { return disp         ;}
  float get_tcrk       () const { return tcrk         ;}
  float get_cross_phi  () const { return cross_phi    ;}
  float get_cross_z    () const { return cross_z      ;}
  float get_center_phi () const { return center_phi   ;}
  float get_center_z   () const { return center_z     ;}

  // swapped crk
  short get_sn0        () const { return sn0          ;}
  float get_snpe0      () const { return snpe0        ;}
  short get_sn1        () const { return sn1          ;}
  float get_snpe1      () const { return snpe1        ;}
  short get_sn2	       () const { return sn2	      ;}
  float get_snpe2      () const { return snpe2	      ;}
  short get_sn3        () const { return sn3	      ;}
  float get_snpe3      () const { return snpe3	      ;}
  float get_schi2      () const { return schi2        ;}
  float get_sdisp      () const { return sdisp        ;}
  float get_stcrk      () const { return stcrk        ;}
  float get_scross_phi () const { return scross_phi   ;}
  float get_scross_z   () const { return scross_z     ;}

  // tofw
  int   get_striptofw  () const { return  striptofw   ;}
  float get_tofwx      () const { return  tofwx       ;}
  float get_tofwy      () const { return  tofwy       ;}
  float get_tofwz      () const { return  tofwz       ;}
  float get_ttofw      () const { return  ttofw       ;}
  float get_qtofw      () const { return  qtofw       ;}
  float get_tofwadcup  () const { return  tofwadcup   ;}
  float get_tofwadcdw  () const { return  tofwadcdw   ;}
  float get_tofwtdcup  () const { return  tofwtdcup   ;}
  float get_tofwtdcdw  () const { return  tofwtdcdw   ;}

  // hbd
  short get_hbdsector  () const { return hbdsector    ;}
  short get_hbdsize    () const { return hbdsize      ;}
  float get_hbdcharge  () const { return hbdcharge    ;}
  float get_hbdx       () const { return hbdx         ;}
  float get_hbdy       () const { return hbdy         ;}
  float get_hbdz       () const { return hbdz         ;}

  // matching
  float get_tofdphi    () const { return tofdphi      ;}
  float get_tofdz      () const { return tofdz        ;}
  float get_tecdphi    () const { return tecdphi      ;}
  float get_tecdalpha  () const { return tecdalpha    ;}
  float get_pc2dphi    () const { return pc2dphi      ;}
  float get_pc2dz      () const { return pc2dz        ;}
  float get_pc3dphi    () const { return pc3dphi      ;}
  float get_pc3dz      () const { return pc3dz        ;}
  float get_emcdphi    () const { return emcdphi      ;}
  float get_emcdz      () const { return emcdz        ;}
  float get_hbddphi    () const { return hbddphi      ;}
  float get_hbddz      () const { return hbddz        ;}
  float get_tofwdphi   () const { return tofwdphi     ;}
  float get_tofwdz     () const { return tofwdz       ;}

  // sigmalized matching
  float get_tofsdphi   () const { return tofsdphi     ;}
  float get_tofsdz     () const { return tofsdz       ;}
  float get_tecsdphi   () const { return tecsdphi     ;}
  float get_tecsdalpha () const { return tecsdalpha   ;}
  float get_pc2sdphi   () const { return pc2sdphi     ;}
  float get_pc2sdz     () const { return pc2sdz       ;}
  float get_pc3sdphi   () const { return pc3sdphi     ;}
  float get_pc3sdz     () const { return pc3sdz       ;}
  float get_emcsdphi   () const { return emcsdphi     ;}
  float get_emcsdz     () const { return emcsdz       ;}
  float get_emcsdphi_e () const { return emcsdphi_e   ;}
  float get_emcsdz_e   () const { return emcsdz_e     ;}
  float get_hbdsdphi   () const { return hbdsdphi     ;}
  float get_hbdsdz     () const { return hbdsdz       ;}
  float get_tofwsdphi  () const { return tofwsdphi    ;}
  float get_tofwsdz    () const { return tofwsdz      ;}

  // swapped matching
  float get_stofdphi   () const { return stofdphi     ;}
  float get_stofdz     () const { return stofdz       ;}
  float get_stecdphi   () const { return stecdphi     ;}
  float get_stecdalpha () const { return stecdalpha   ;}
  float get_spc2dphi   () const { return spc2dphi     ;}
  float get_spc2dz     () const { return spc2dz       ;}
  float get_spc3dphi   () const { return spc3dphi     ;}
  float get_spc3dz     () const { return spc3dz       ;}
  float get_semcdphi   () const { return semcdphi     ;}
  float get_semcdz     () const { return semcdz       ;}
  float get_semcdphi_e () const { return semcdphi_e   ;}
  float get_semcdz_e   () const { return semcdz_e     ;}

  // swapped sigmalized matching
  float get_stofsdphi  () const { return stofsdphi    ;}
  float get_stofsdz    () const { return stofsdz      ;}
  float get_stecsdphi  () const { return stecsdphi    ;}
  float get_stecsdalpha() const { return stecsdalpha  ;}
  float get_spc2sdphi  () const { return spc2sdphi    ;}
  float get_spc2sdz    () const { return spc2sdz      ;}
  float get_spc3sdphi  () const { return spc3sdphi    ;}
  float get_spc3sdz    () const { return spc3sdz      ;}
  float get_semcsdphi  () const { return semcsdphi    ;}
  float get_semcsdz    () const { return semcsdz      ;}
  float get_semcsdphi_e() const { return semcsdphi_e  ;}
  float get_semcsdz_e  () const { return semcsdz_e    ;}

  // etc.
  float get_m2tof      () const { return m2tof        ;}
  float get_m2tofw     () const { return m2tofw       ;}
  float get_m2emc      () const { return m2emc        ;}
  float get_isPi       () const { return isPi         ;}
  float get_isK        () const { return isK          ;}
  float get_isP        () const { return isP          ;}
  short get_dcarm      () const { return dcarm	      ;}
  short get_dcside     () const { return dcside       ;}
  short get_pc1sect    () const { return pc1sect      ;}
  short get_pc2sect    () const { return pc2sect      ;}
  short get_pc3sect    () const { return pc3sect      ;}
  float get_mx1dist    () const { return mx1dist      ;}
  float get_mx2dist    () const { return mx2dist      ;}
  float get_mchi2      () const { return mchi2	      ;}
  short get_pc1id      () const { return pc1id        ;}
  short get_pc2id      () const { return pc2id        ;}
  short get_pc3id      () const { return pc3id        ;}
  short get_emcid      () const { return emcid        ;}
  short get_tofid      () const { return tofid        ;}
  short get_tofwid     () const { return tofwid       ;}
  int   get_ring       () const { return ring         ;}
  int   get_mcid       () const { return mcid         ;}
  int   get_dchid      () const { return dchid        ;}
  int   get_aerindex   () const { return aerindex     ;}
  int   get_aersindex  () const { return aersindex    ;}
  short get_candIDl2e  () const { return candIDl2e    ;}
  long  get_categoryl2eLowPt() const { return categoryl2eLowPt ;}
  long  get_categoryl2eHighPt() const{ return categoryl2eHighPt;}

  // IDs to subsystem nodes
  int   get_idtrk_tof  () const { return idtrk_tof   ;}
  int   get_idtrk_tec  () const { return idtrk_tec   ;}
  int   get_idtrk_crk  () const { return idtrk_crk   ;}
  int   get_idtrk_pc2  () const { return idtrk_pc2   ;}
  int   get_idtrk_pc3  () const { return idtrk_pc3   ;}
  int   get_idtrk_hbd  () const { return idtrk_hbd   ;}
  int   get_idtrk_tofw () const { return idtrk_tofw  ;}

  // IDs to swapped subsystem nodes
  int   get_idtrk_stof () const { return idtrk_stof  ;}
  int   get_idtrk_stec () const { return idtrk_stec  ;}
  int   get_idtrk_scrk () const { return idtrk_scrk  ;}
  int   get_idtrk_spc2 () const { return idtrk_spc2  ;}
  int   get_idtrk_spc3 () const { return idtrk_spc3  ;}

 protected:
  short charge;
  short quality;
  float zed;
  float phi;
  float alpha;
  float beta;
  float phi0;
  float the0;
  float mom;
  short status;
  float alpha1;
  float alpha2;
  short nx1hits;
  short nx2hits;
  float ppc1x;
  float ppc1y;
  float ppc1z;
  float ppc2x;
  float ppc2y;
  float ppc2z;
  float ptecx;
  float ptecy;
  float ptecz;
  float ppc3x;
  float ppc3y;
  float ppc3z;
  float pemcx;
  float pemcy;
  float pemcz;
  float ptofx;
  float ptofy;
  float ptofz;
  float phbdx;
  float phbdy;
  float phbdz;
  float ptofwx;
  float ptofwy;
  float ptofwz;
  float pltof;
  float plemc;
  float pltofw;

  // emc
  short sect;
  short ysect;
  short zsect;
  float ecore;
  float emce;
  float emcdispy;
  float emcdispz;
  int emcrawtdc;
  int emcrawadc;
  int emcrawadclg;
  float temc;
  float prob;
  float ecent;
  short twrhit;
  float e9;
  float re9;
  float emcchi2;
  int deadmap;
  int warnmap;

  // swapped emc
  float secore;
  float semce;
  float semcdispy;
  float semcdispz;
  float stemc;
  float sprob;
  float secent;
  short stwrhit;
  float se9;
  float semcchi2;
  int sdeadmap;
  int swarnmap;

  // tof
  int   slat;
  float ttof;
  float etof;
  float tofph1;
  float tofph2;
  float toftdc1;
  float toftdc2;

  // swapped tof
  float sttof;
  float setof;

  // tec
  short tecid;

  // swapped tec
  short stecid;

  // crk
  short n0;
  float npe0;
  short n1;
  float npe1;
  short n2;
  float npe2;
  short n3;
  float npe3;
  float chi2;
  float disp;
  float tcrk;
  float cross_phi;
  float cross_z;
  float center_phi;
  float center_z;

  // swapped crk
  short sn0;
  float snpe0;
  short sn1;
  float snpe1;
  short sn2;
  float snpe2;
  short sn3;
  float snpe3;
  float schi2;
  float sdisp;
  float stcrk;
  float scross_phi;
  float scross_z;

  // tofw
  int   striptofw;
  float tofwx;
  float tofwy;
  float tofwz;
  float ttofw;  
  float qtofw;
  float tofwtdcup;
  float tofwtdcdw;
  float tofwadcup;
  float tofwadcdw;

  // hbd
  short hbdsector;
  short hbdsize;
  float hbdcharge;
  float hbdx;
  float hbdy;
  float hbdz;
  
  // matching
  float tofdphi;
  float tofdz;
  float tecdphi;
  float tecdalpha;
  float pc2dphi;
  float pc2dz;
  float pc3dphi;
  float pc3dz;
  float emcdphi;
  float emcdz;
  float hbddphi;
  float hbddz;
  float tofwdphi;
  float tofwdz;

  // sigmalized matching
  float tofsdphi;
  float tofsdz;
  float tecsdphi;
  float tecsdalpha;
  float pc2sdphi;
  float pc2sdz;
  float pc3sdphi;
  float pc3sdz;
  float emcsdphi;
  float emcsdz;
  float emcsdphi_e;
  float emcsdz_e;
  float hbdsdphi;
  float hbdsdz;
  float tofwsdphi;
  float tofwsdz;

  // swapped matching
  float stofdphi;
  float stofdz;
  float stecdphi;
  float stecdalpha;
  float spc2dphi;
  float spc2dz;
  float spc3dphi;
  float spc3dz;
  float semcdphi;
  float semcdz;
  float semcdphi_e;
  float semcdz_e;
  float shbddphi;
  float shbddz;

  // swapped sigmalized matching
  float stofsdphi;
  float stofsdz;
  float stecsdphi;
  float stecsdalpha;
  float spc2sdphi;
  float spc2sdz;
  float spc3sdphi;
  float spc3sdz;
  float semcsdphi;
  float semcsdz;
  float semcsdphi_e;
  float semcsdz_e;
  float stofwsdphi;
  float stofwsdz;

  // etc.
  float m2tof;
  float m2tofw;
  float m2emc;
  float isPi;
  float isK;
  float isP;
  short dcarm;
  short dcside;
  short pc1sect;
  short pc2sect;
  short pc3sect;
  float mx1dist;
  float mx2dist;
  float mchi2;
  short pc1id;
  short pc2id;
  short pc3id;
  short emcid;
  short tofid;
  short tofwid;
  short hbdid;
  int ring;
  int mcid;
  int dchid;
  int aerindex;
  int aersindex;
  short candIDl2e;
  long categoryl2eLowPt;
  long categoryl2eHighPt;

  // IDs to subsystem nodes
  int idtrk_tof;
  int idtrk_tofw;
  int idtrk_tec;
  int idtrk_crk;
  int idtrk_pc2;
  int idtrk_pc3;
  int idtrk_hbd;

  // IDs to swapped subsystem nodes
  int idtrk_stof;
  int idtrk_stec;
  int idtrk_scrk;
  int idtrk_spc2;
  int idtrk_spc3;

  ClassDef(PHSnglCentralTrackv20,1)
};

#endif /* PHSNGLCENTRALTRACKV20 */

