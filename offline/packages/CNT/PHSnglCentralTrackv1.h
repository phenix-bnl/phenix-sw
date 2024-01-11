#ifndef __PHSNGLCENTRALTRACKV1_H_
#define __PHSNGLCENTRALTRACKV1_H_

#include "PHObject.h"

class PHSnglCentralTrackv1 : public TObject
{
 public:
  PHSnglCentralTrackv1();
  PHSnglCentralTrackv1(const PHSnglCentralTrackv1 &track);  
  virtual ~PHSnglCentralTrackv1() {}

  // Here are the very explicit set routines...
  void set_charge     (const short val) {charge     =val; return;}
  void set_quality    (const short val) {quality    =val; return;}
  void set_zed        (const float val) {zed        =val; return;}
  void set_phi        (const float val) {phi        =val; return;}
  void set_alpha      (const float val) {alpha      =val; return;}
  void set_beta       (const float val) {beta       =val; return;}
  void set_phi0       (const float val) {phi0       =val; return;}
  void set_the0       (const float val) {the0       =val; return;}
  void set_mom        (const float val) {mom        =val; return;}
  void set_mompx      (const float val) {mompx      =val; return;}
  void set_mompy      (const float val) {mompy      =val; return;}
  void set_mompz      (const float val) {mompz      =val; return;}
  void set_status     (const short val) {status     =val; return;}
  void set_alpha1     (const float val) {alpha1     =val; return;}
  void set_alpha2     (const float val) {alpha2     =val; return;}
  void set_nx1hits    (const short val) {nx1hits    =val; return;}
  void set_nx2hits    (const short val) {nx2hits    =val; return;}
  void set_mx1dist    (const float val) {mx1dist    =val; return;}
  void set_mx2dist    (const float val) {mx2dist    =val; return;}
  void set_chi2x1     (const float val) {chi2x1     =val; return;}
  void set_chi2x2     (const float val) {chi2x2     =val; return;}
  void set_nx1x2fit   (const short val) {nx1x2fit   =val; return;}
  void set_mchi2      (const float val) {mchi2      =val; return;}
  void set_error      (const float val) {error      =val; return;}
  void set_alphaf     (const float val) {alphaf     =val; return;}
  void set_pc1id      (const short val) {pc1id      =val; return;}
  void set_pc2id      (const short val) {pc2id      =val; return;}
  void set_pc3id      (const short val) {pc3id      =val; return;}
  void set_emcid      (const short val) {emcid      =val; return;}
  void set_tofid      (const short val) {tofid      =val; return;}
  void set_tecid      (const short val) {tecid      =val; return;}
  void set_spc2id     (const short val) {spc2id     =val; return;}
  void set_spc3id     (const short val) {spc3id     =val; return;}
  void set_semcid     (const short val) {semcid     =val; return;}
  void set_stofid     (const short val) {stofid     =val; return;}
  void set_stecid     (const short val) {stecid     =val; return;}
  void set_ppc1x      (const float val) {ppc1x      =val; return;}
  void set_ppc1y      (const float val) {ppc1y      =val; return;}
  void set_ppc1z      (const float val) {ppc1z      =val; return;}
  void set_ppc2x      (const float val) {ppc2x      =val; return;}
  void set_ppc2y      (const float val) {ppc2y      =val; return;}
  void set_ppc2z      (const float val) {ppc2z      =val; return;}
  void set_ptecx      (const float val) {ptecx      =val; return;}
  void set_ptecy      (const float val) {ptecy      =val; return;}
  void set_ptecz      (const float val) {ptecz      =val; return;}
  void set_ppc3x      (const float val) {ppc3x      =val; return;}
  void set_ppc3y      (const float val) {ppc3y      =val; return;}
  void set_ppc3z      (const float val) {ppc3z      =val; return;}
  void set_pemcx      (const float val) {pemcx      =val; return;}
  void set_pemcy      (const float val) {pemcy      =val; return;}
  void set_pemcz      (const float val) {pemcz      =val; return;}
  void set_ptofx      (const float val) {ptofx      =val; return;}
  void set_ptofy      (const float val) {ptofy      =val; return;}
  void set_ptofz      (const float val) {ptofz      =val; return;}
  void set_pltof      (const float val) {pltof      =val; return;}
  void set_plemc      (const float val) {plemc      =val; return;}
  void set_pc2dphi    (const float val) {pc2dphi    =val; return;}
  void set_pc2dz      (const float val) {pc2dz      =val; return;}
  void set_pc3dphi    (const float val) {pc3dphi    =val; return;}
  void set_pc3dz      (const float val) {pc3dz      =val; return;}
  void set_emcdphi    (const float val) {emcdphi    =val; return;}
  void set_emcdz      (const float val) {emcdz      =val; return;}
  void set_tofdphi    (const float val) {tofdphi    =val; return;}
  void set_tofdz      (const float val) {tofdz      =val; return;}
  void set_tecdphi    (const float val) {tecdphi    =val; return;}
  void set_tecdalpha  (const float val) {tecdalpha  =val; return;}
  void set_spc2dphi   (const float val) {spc2dphi   =val; return;}
  void set_spc2dz     (const float val) {spc2dz     =val; return;}
  void set_spc3dphi   (const float val) {spc3dphi   =val; return;}
  void set_spc3dz     (const float val) {spc3dz     =val; return;}
  void set_semcdphi   (const float val) {semcdphi   =val; return;}
  void set_semcdz     (const float val) {semcdz     =val; return;}
  void set_stofdphi   (const float val) {stofdphi   =val; return;}
  void set_stofdz     (const float val) {stofdz     =val; return;}
  void set_stecdphi   (const float val) {stecdphi   =val; return;}
  void set_stecdalpha (const float val) {stecdalpha =val; return;}
  void set_arm        (const short val) {arm        =val; return;}
  void set_sect       (const short val) {sect       =val; return;}
  void set_ysect      (const short val) {ysect      =val; return;}
  void set_zsect      (const short val) {zsect      =val; return;}
  void set_ecorr      (const float val) {ecorr      =val; return;}
  void set_ecore      (const float val) {ecore      =val; return;}
  void set_temc       (const float val) {temc       =val; return;}
  void set_prob       (const float val) {prob       =val; return;}
  void set_secorr     (const float val) {secorr     =val; return;}
  void set_secore     (const float val) {secore     =val; return;}
  void set_stemc      (const float val) {stemc      =val; return;}
  void set_sprob      (const float val) {sprob      =val; return;}
  void set_slat       (const int   val) {slat       =val; return;}
  void set_ttof       (const float val) {ttof       =val; return;}
  void set_etof       (const float val) {etof       =val; return;}
  void set_sttof      (const float val) {sttof      =val; return;}
  void set_setof      (const float val) {setof      =val; return;}
  void set_acc        (const short val) {acc        =val; return;}
  void set_ring       (const int   val) {ring       =val; return;}
  void set_n0         (const short val) {n0         =val; return;}
  void set_npe0       (const float val) {npe0       =val; return;}
  void set_chi2       (const float val) {chi2       =val; return;}
  void set_disp       (const float val) {disp       =val; return;}
  void set_tcrk       (const float val) {tcrk       =val; return;}
  void set_sacc       (const short val) {sacc       =val; return;}
  void set_sring      (const int   val) {sring      =val; return;}
  void set_sn0        (const short val) {sn0        =val; return;}
  void set_snpe0      (const float val) {snpe0      =val; return;}
  void set_schi2      (const float val) {schi2      =val; return;}
  void set_sdisp      (const float val) {sdisp      =val; return;}
  void set_stcrk      (const float val) {stcrk      =val; return;}
  void set_tecdedx1   (const float val) {tecdedx1   =val; return;}
  void set_tecdedx2   (const float val) {tecdedx2   =val; return;}
  void set_pc2sdphi   (const float val) {pc2sdphi   =val; return;}
  void set_pc2sdz     (const float val) {pc2sdz     =val; return;}
  void set_pc3sdphi   (const float val) {pc3sdphi   =val; return;}
  void set_pc3sdz     (const float val) {pc3sdz     =val; return;}
  void set_emcsdphi   (const float val) {emcsdphi   =val; return;}
  void set_emcsdz     (const float val) {emcsdz     =val; return;}
  void set_tofsdphi   (const float val) {tofsdphi   =val; return;}
  void set_tofsdz     (const float val) {tofsdz     =val; return;}
  void set_spc2sdphi  (const float val) {spc2sdphi  =val; return;}
  void set_spc2sdz    (const float val) {spc2sdz    =val; return;}
  void set_spc3sdphi  (const float val) {spc3sdphi  =val; return;}
  void set_spc3sdz    (const float val) {spc3sdz    =val; return;}
  void set_semcsdphi  (const float val) {semcsdphi  =val; return;}
  void set_semcsdz    (const float val) {semcsdz    =val; return;}
  void set_stofsdphi  (const float val) {stofsdphi  =val; return;}
  void set_stofsdz    (const float val) {stofsdz    =val; return;}
  // added jv
  void set_m2tof      (const float val) {m2tof      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}


  // Here are the very explicit "get" routines...
  short get_charge     () const  { return  charge      ;}
  short get_quality    () const  { return  quality     ;}
  float get_zed        () const  { return  zed         ;}
  float get_phi        () const  { return  phi         ;}
  float get_alpha      () const  { return  alpha       ;}
  float get_beta       () const  { return  beta        ;}
  float get_phi0       () const  { return  phi0        ;}
  float get_the0       () const  { return  the0        ;}
  float get_mom        () const  { return  mom         ;}
  float get_mompx      () const  { return  mompx       ;}
  float get_mompy      () const  { return  mompy       ;}
  float get_mompz      () const  { return  mompz       ;}
  short get_status     () const  { return  status      ;}
  float get_alpha1     () const  { return  alpha1      ;}
  float get_alpha2     () const  { return  alpha2      ;}
  short get_nx1hits    () const  { return  nx1hits     ;}
  short get_nx2hits    () const  { return  nx2hits     ;}
  float get_mx1dist    () const  { return  mx1dist     ;}
  float get_mx2dist    () const  { return  mx2dist     ;}
  float get_chi2x1     () const  { return  chi2x1      ;}
  float get_chi2x2     () const  { return  chi2x2      ;}
  short get_nx1x2fit   () const  { return  nx1x2fit    ;}
  float get_mchi2      () const  { return  mchi2       ;}
  float get_error      () const  { return  error       ;}
  float get_alphaf     () const  { return  alphaf      ;}
  short get_pc1id      () const  { return  pc1id       ;}
  short get_pc2id      () const  { return  pc2id       ;}
  short get_pc3id      () const  { return  pc3id       ;}
  short get_emcid      () const  { return  emcid       ;}
  short get_tofid      () const  { return  tofid       ;}
  short get_tecid      () const  { return  tecid       ;}
  short get_spc2id     () const  { return  spc2id      ;}
  short get_spc3id     () const  { return  spc3id      ;}
  short get_semcid     () const  { return  semcid      ;}
  short get_stofid     () const  { return  stofid      ;}
  short get_stecid     () const  { return  stecid      ;}
  float get_ppc1x      () const  { return  ppc1x       ;}
  float get_ppc1y      () const  { return  ppc1y       ;}
  float get_ppc1z      () const  { return  ppc1z       ;}
  float get_ppc2x      () const  { return  ppc2x       ;}
  float get_ppc2y      () const  { return  ppc2y       ;}
  float get_ppc2z      () const  { return  ppc2z       ;}
  float get_ptecx      () const  { return  ptecx       ;}
  float get_ptecy      () const  { return  ptecy       ;}
  float get_ptecz      () const  { return  ptecz       ;}
  float get_ppc3x      () const  { return  ppc3x       ;}
  float get_ppc3y      () const  { return  ppc3y       ;}
  float get_ppc3z      () const  { return  ppc3z       ;}
  float get_pemcx      () const  { return  pemcx       ;}
  float get_pemcy      () const  { return  pemcy       ;}
  float get_pemcz      () const  { return  pemcz       ;}
  float get_ptofx      () const  { return  ptofx       ;}
  float get_ptofy      () const  { return  ptofy       ;}
  float get_ptofz      () const  { return  ptofz       ;}
  float get_pltof      () const  { return  pltof       ;}
  float get_plemc      () const  { return  plemc       ;}
  float get_pc2dphi    () const  { return  pc2dphi     ;}
  float get_pc2dz      () const  { return  pc2dz       ;}
  float get_pc3dphi    () const  { return  pc3dphi     ;}
  float get_pc3dz      () const  { return  pc3dz       ;}
  float get_emcdphi    () const  { return  emcdphi     ;}
  float get_emcdz      () const  { return  emcdz       ;}
  float get_tofdphi    () const  { return  tofdphi     ;}
  float get_tofdz      () const  { return  tofdz       ;}
  float get_tecdphi    () const  { return  tecdphi     ;}
  float get_tecdalpha  () const  { return  tecdalpha   ;}
  float get_spc2dphi   () const  { return  spc2dphi    ;}
  float get_spc2dz     () const  { return  spc2dz      ;}
  float get_spc3dphi   () const  { return  spc3dphi    ;}
  float get_spc3dz     () const  { return  spc3dz      ;}
  float get_semcdphi   () const  { return  semcdphi    ;}
  float get_semcdz     () const  { return  semcdz      ;}
  float get_stofdphi   () const  { return  stofdphi    ;}
  float get_stofdz     () const  { return  stofdz      ;}
  float get_stecdphi   () const  { return  stecdphi    ;}
  float get_stecdalpha () const  { return  stecdalpha  ;}
  short get_arm        () const  { return  arm         ;}
  short get_sect       () const  { return  sect        ;}
  short get_ysect      () const  { return  ysect       ;}
  short get_zsect      () const  { return  zsect       ;}
  float get_ecorr      () const  { return  ecorr       ;}
  float get_ecore      () const  { return  ecore       ;}
  float get_temc       () const  { return  temc        ;}
  float get_prob       () const  { return  prob        ;}
  float get_secorr     () const  { return  secorr      ;}
  float get_secore     () const  { return  secore      ;}
  float get_stemc      () const  { return  stemc       ;}
  float get_sprob      () const  { return  sprob       ;}
  int   get_slat       () const  { return  slat        ;}
  float get_ttof       () const  { return  ttof        ;}
  float get_etof       () const  { return  etof        ;}
  float get_sttof      () const  { return  sttof       ;}
  float get_setof      () const  { return  setof       ;}
  short get_acc        () const  { return  acc         ;}
  int   get_ring       () const  { return  ring        ;}
  short get_n0         () const  { return  n0          ;}
  float get_npe0       () const  { return  npe0        ;}
  float get_chi2       () const  { return  chi2        ;}
  float get_disp       () const  { return  disp        ;}
  float get_tcrk       () const  { return  tcrk        ;}
  short get_sacc       () const  { return  sacc        ;}
  int   get_sring      () const  { return  sring       ;}
  short get_sn0        () const  { return  sn0         ;}
  float get_snpe0      () const  { return  snpe0       ;}
  float get_schi2      () const  { return  schi2       ;}
  float get_sdisp      () const  { return  sdisp       ;}
  float get_stcrk      () const  { return  stcrk       ;}
  float get_tecdedx1   () const  { return  tecdedx1    ;}
  float get_tecdedx2   () const  { return  tecdedx2    ;}
  float get_pc2sdphi   () const  { return  pc2sdphi    ;}
  float get_pc2sdz     () const  { return  pc2sdz      ;}
  float get_pc3sdphi   () const  { return  pc3sdphi    ;}
  float get_pc3sdz     () const  { return  pc3sdz      ;}
  float get_emcsdphi   () const  { return  emcsdphi    ;}
  float get_emcsdz     () const  { return  emcsdz      ;}
  float get_tofsdphi   () const  { return  tofsdphi    ;}
  float get_tofsdz     () const  { return  tofsdz      ;}
  float get_spc2sdphi  () const  { return  spc2sdphi   ;}
  float get_spc2sdz    () const  { return  spc2sdz     ;}
  float get_spc3sdphi  () const  { return  spc3sdphi   ;}
  float get_spc3sdz    () const  { return  spc3sdz     ;}
  float get_semcsdphi  () const  { return  semcsdphi   ;}
  float get_semcsdz    () const  { return  semcsdz     ;}
  float get_stofsdphi  () const  { return  stofsdphi   ;}
  float get_stofsdz    () const  { return  stofsdz     ;}
  // added jv
  float get_m2tof      () const  { return  m2tof       ;}
  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}

  //float determine_PIDtof  (const float m2) ;  //moved to CentralTrack...
  //float determine_PIDemc  (const float m2) ;
  
 protected:
  short charge     ;
  short quality    ;
  float zed        ;
  float phi        ;
  float alpha      ;
  float beta       ;
  float phi0       ;
  float the0       ;
  float mom        ;
  float mompx      ;
  float mompy      ;
  float mompz      ;
  short status     ;
  float alpha1     ;
  float alpha2     ;
  short nx1hits    ;
  short nx2hits    ;
  float mx1dist    ;
  float mx2dist    ;
  float chi2x1     ;
  float chi2x2     ;
  short nx1x2fit   ;
  float mchi2      ;
  float error      ;
  float alphaf     ;
  short pc1id      ;
  short pc2id      ;
  short pc3id      ;
  short emcid      ;
  short tofid      ;
  short tecid      ;
  short spc2id     ;
  short spc3id     ;
  short semcid     ;
  short stofid     ;
  short stecid     ;
  float ppc1x      ;
  float ppc1y      ;
  float ppc1z      ;
  float ppc2x      ;
  float ppc2y      ;
  float ppc2z      ;
  float ptecx      ;
  float ptecy      ;
  float ptecz      ;
  float ppc3x      ;
  float ppc3y      ;
  float ppc3z      ;
  float pemcx      ;
  float pemcy      ;
  float pemcz      ;
  float ptofx      ;
  float ptofy      ;
  float ptofz      ;
  float pltof      ;
  float plemc      ;
  float pc2dphi    ;
  float pc2dz      ;
  float pc3dphi    ;
  float pc3dz      ;
  float emcdphi    ;
  float emcdz      ;
  float tofdphi    ;
  float tofdz      ;
  float tecdphi    ;
  float tecdalpha  ;
  float spc2dphi   ;
  float spc2dz     ;
  float spc3dphi   ;
  float spc3dz     ;
  float semcdphi   ;
  float semcdz     ;
  float stofdphi   ;
  float stofdz     ;
  float stecdphi   ;
  float stecdalpha ;
  short arm        ;
  short sect       ;
  short ysect      ;
  short zsect      ;
  float ecorr      ;
  float ecore      ;
  float temc       ;
  float prob       ;
  float secorr     ;
  float secore     ;
  float stemc      ;
  float sprob      ;
  int   slat       ;
  float ttof       ;
  float etof       ;
  float sttof      ;
  float setof      ;
  short acc        ;
  int   ring       ;
  short n0         ;
  float npe0       ;
  float chi2       ;
  float disp       ;
  float tcrk       ;
  short sacc       ;
  int   sring      ;
  short sn0        ;
  float snpe0      ;
  float schi2      ;
  float sdisp      ;
  float stcrk      ;
  float tecdedx1   ;
  float tecdedx2   ;
  float pc2sdphi   ;
  float pc2sdz     ;
  float pc3sdphi   ;
  float pc3sdz     ;
  float emcsdphi   ;
  float emcsdz     ;
  float tofsdphi   ;
  float tofsdz     ;
  float spc2sdphi  ;
  float spc2sdz    ;
  float spc3sdphi  ;
  float spc3sdz    ;
  float semcsdphi  ;
  float semcsdz    ;
  float stofsdphi  ;
  float stofsdz    ;
  // added jv
  float m2tof      ;
  float m2emc      ;
  float isPi       ;
  float isK        ;
  float isP        ;

  ClassDef(PHSnglCentralTrackv1,1)
};

#endif /* PHHSNGLCENTRALTRACKV1 */

