#ifndef __PHSNGLCENTRALTRACKV9_H_
#define __PHSNGLCENTRALTRACKV9_H_

#include "PHObject.h"
#include "PHSnglCentralTrack.h"

class PHSnglCentralTrackv9 : public PHSnglCentralTrack
{
 public:
  PHSnglCentralTrackv9();
  PHSnglCentralTrackv9(const PHSnglCentralTrackv9 &track);  
  virtual ~PHSnglCentralTrackv9() {}

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
  void set_nx1hits    (const short val) {nx1hits    =val; return;}
  void set_nx2hits    (const short val) {nx2hits    =val; return;}
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
  void set_sect       (const short val) {sect       =val; return;}
  void set_ysect      (const short val) {ysect      =val; return;}
  void set_zsect      (const short val) {zsect      =val; return;}
  void set_ecorr      (const float val) {ecorr      =val; return;}
  void set_ecore      (const float val) {ecore      =val; return;}
  void set_temc       (const float val) {temc       =val; return;}
  void set_prob       (const float val) {prob       =val; return;}
  void set_ecent      (const float val) {ecent      =val; return;}
  void set_twrhit     (const short val) {twrhit     =val; return;}
  void set_e9         (const float val) {e9         =val; return;}
  void set_re9        (const float val) {re9        =val; return;}
  void set_emcchi2    (const float val) {emcchi2    =val; return;}
  void set_slat       (const int   val) {slat       =val; return;}
  void set_ttof       (const float val) {ttof       =val; return;}
  void set_etof       (const float val) {etof       =val; return;}
  void set_n0         (const short val) {n0         =val; return;}
  void set_npe0       (const float val) {npe0       =val; return;}
  void set_n1         (const short val) {n1         =val; return;}
  void set_npe1       (const float val) {npe1       =val; return;}
  void set_chi2       (const float val) {chi2       =val; return;}
  void set_disp       (const float val) {disp       =val; return;}
  void set_tcrk       (const float val) {tcrk       =val; return;}
  void set_cross_phi  (const float val) {cross_phi  =val; return;}
  void set_cross_z    (const float val) {cross_z    =val; return;}
  void set_center_phi (const float val) {center_phi =val; return;}
  void set_center_z   (const float val) {center_z   =val; return;}
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
  void set_tecsdphi   (const float val) {tecsdphi   =val; return;}
  void set_tecsdalpha (const float val) {tecsdalpha =val; return;}
  void set_m2tof      (const float val) {m2tof      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}
  void set_dcarm      (const short val) {dcarm	    = val; return;}
  void set_dcside     (const short val) {dcside	    = val; return;}
  void set_pc1sect    (const short val) {pc1sect    = val; return;}
  void set_pc2sect    (const short val) {pc2sect    = val; return;}
  void set_pc3sect    (const short val) {pc3sect    = val; return;}
  void set_emcsdphi_e (const float val) {emcsdphi_e = val; return;}
  void set_emcsdz_e   (const float val) {emcsdz_e   = val; return;}
  void set_tecnhit    (const short val) {tecnhit    = val; return;}
  void set_n2	      (const short val) {n2	    = val; return;}
  void set_npe2	      (const float val) {npe2	    = val; return;}
  void set_n3	      (const short val) {n3	    = val; return;}
  void set_npe3	      (const float val) {npe3	    = val; return;}
  void set_deadmap    (const int val)   {deadmap    = val; return;}
  void set_warnmap    (const int val)   {warnmap    = val; return;}
  void set_emce	        (const float val) {emce	= val; return;}
  void set_pc1id	(const short val) {pc1id	= val; return;}
  void set_pc2id	(const short val) {pc2id	= val; return;}
  void set_pc3id	(const short val) {pc3id	= val; return;}
  void set_emcid	(const short val) {emcid	= val; return;}
  void set_tofid	(const short val) {tofid	= val; return;}
  void set_tecid	(const short val) {tecid	= val; return;}
  void set_L1Trig	(const int   val) {L1Trig	= val; return;}
  void set_pc2dphi	(const float val) {pc2dphi	= val; return;}
  void set_pc2dz	(const float val) {pc2dz	= val; return;}
  void set_pc3dphi	(const float val) {pc3dphi	= val; return;}
  void set_pc3dz	(const float val) {pc3dz	= val; return;}
  void set_emcdphi	(const float val) {emcdphi	= val; return;}
  void set_emcdz	(const float val) {emcdz	= val; return;}
  void set_tofdphi	(const float val) {tofdphi	= val; return;}
  void set_tofdz	(const float val) {tofdz	= val; return;}
  void set_tecdphi	(const float val) {tecdphi	= val; return;}
  void set_tecdalpha	(const float val) {tecdalpha	= val; return;}
  void set_ring	        (const int   val) {ring	= val; return;}
  void set_pc1phi	(const float val) {pc1phi	= val; return;}
  void set_pc1z	        (const float val) {pc1z	= val; return;}
  void set_pc2phi	(const float val) {pc2phi	= val; return;}
  void set_pc2z	        (const float val) {pc2z	= val; return;}
  void set_pc3phi	(const float val) {pc3phi	= val; return;}
  void set_pc3z	        (const float val) {pc3z	= val; return;}
  void set_emcphi	(const float val) {emcphi	= val; return;}
  void set_emcz	        (const float val) {emcz	= val; return;}
  void set_tofphi	(const float val) {tofphi	= val; return;}
  void set_tofz	        (const float val) {tofz	= val; return;}
  void set_tecphi	(const float val) {tecphi	= val; return;}
  void set_tecalpha	(const float val) {tecalpha	= val; return;}
  void set_RawL1        (const int val) {RawL1  = val; return;}
  void set_LivL1        (const int val) {LivL1  = val; return;}
  void set_SclL1        (const int val) {SclL1  = val; return;}


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
  short get_nx1hits    () const  { return  nx1hits     ;}
  short get_nx2hits    () const  { return  nx2hits     ;}
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
  short get_sect       () const  { return  sect        ;}
  short get_ysect      () const  { return  ysect       ;}
  short get_zsect      () const  { return  zsect       ;}
  float get_ecorr      () const  { return  ecorr       ;}
  float get_ecore      () const  { return  ecore       ;}
  float get_temc       () const  { return  temc        ;}
  float get_prob       () const  { return  prob        ;}
  float get_ecent      () const  { return  ecent       ;}
  short get_twrhit     () const  { return  twrhit      ;}
  float get_e9         () const  { return  e9          ;}
  float get_re9        () const  { return  re9         ;}
  float get_emcchi2    () const  { return  emcchi2     ;}
  int   get_slat       () const  { return  slat        ;}
  float get_ttof       () const  { return  ttof        ;}
  float get_etof       () const  { return  etof        ;}
  short get_n0         () const  { return  n0          ;}
  float get_npe0       () const  { return  npe0        ;}
  short get_n1         () const  { return  n1          ;}
  float get_npe1       () const  { return  npe1        ;}
  float get_chi2       () const  { return  chi2        ;}
  float get_disp       () const  { return  disp        ;}
  float get_tcrk       () const  { return  tcrk        ;}
  float get_cross_phi  () const  { return  cross_phi   ;}
  float get_cross_z    () const  { return  cross_z     ;}
  float get_center_phi () const  { return  center_phi  ;}
  float get_center_z   () const  { return  center_z    ;}
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
  float get_tecsdphi   () const  { return  tecsdphi    ;}
  float get_tecsdalpha () const  { return  tecsdalpha  ;}
  float get_m2tof      () const  { return  m2tof       ;}
  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}
  short get_dcarm	() const { return dcarm	        ;}
  short get_dcside	() const { return dcside        ;}
  short get_pc1sect	() const { return pc1sect	;}
  short get_pc2sect	() const { return pc2sect	;}
  short get_pc3sect	() const { return pc3sect	;}
  float get_emcsdphi_e	() const { return emcsdphi_e	;}
  float get_emcsdz_e	() const { return emcsdz_e	;}
  short get_tecnhit	() const { return tecnhit	;}
  short get_n2	        () const { return n2	        ;}
  float get_npe2	() const { return npe2	        ;}
  short get_n3	        () const { return n3	        ;}
  float get_npe3	() const { return npe3	        ;}
  int   get_deadmap	() const { return deadmap	;}
  int   get_warnmap	() const { return warnmap	;}
  float get_emce	() const { return emce	;}
  short get_pc1id	() const { return pc1id	;}
  short get_pc2id	() const { return pc2id	;}
  short get_pc3id	() const { return pc3id	;}
  short get_emcid	() const { return emcid	;}
  short get_tofid	() const { return tofid	;}
  short get_tecid	() const { return tecid	;}
  int   get_L1Trig	() const { return L1Trig	;}
  float get_pc2dphi	() const { return pc2dphi	;}
  float get_pc2dz	() const { return pc2dz	;}
  float get_pc3dphi	() const { return pc3dphi	;}
  float get_pc3dz	() const { return pc3dz	;}
  float get_emcdphi	() const { return emcdphi	;}
  float get_emcdz	() const { return emcdz	;}
  float get_tofdphi	() const { return tofdphi	;}
  float get_tofdz	() const { return tofdz	;}
  float get_tecdphi	() const { return tecdphi	;}
  float get_tecdalpha	() const { return tecdalpha	;}
  int   get_ring	() const { return ring	;}
  float get_pc1phi	() const { return pc1phi	;}
  float get_pc1z	() const { return pc1z	;}
  float get_pc2phi	() const { return pc2phi	;}
  float get_pc2z	() const { return pc2z	;}
  float get_pc3phi	() const { return pc3phi	;}
  float get_pc3z	() const { return pc3z	;}
  float get_emcphi	() const { return emcphi	;}
  float get_emcz	() const { return emcz	;}
  float get_tofphi	() const { return tofphi	;}
  float get_tofz	() const { return tofz	;}
  float get_tecphi	() const { return tecphi	;}
  float get_tecalpha	() const { return tecalpha	;}
  int get_RawL1 () const { return RawL1 ;}
  int get_LivL1 () const { return LivL1 ;}
  int get_SclL1 () const { return SclL1 ;}




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
  short nx1hits    ;
  short nx2hits    ;
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
  short sect       ;
  short ysect      ;
  short zsect      ;
  float ecorr      ;
  float ecore      ;
  float temc       ;
  float prob       ;
  float ecent      ;
  short twrhit     ;
  float e9         ;
  float re9        ;
  float emcchi2    ;
  int   slat       ;
  float ttof       ;
  float etof       ;
  short n0         ;
  float npe0       ;
  short n1         ;
  float npe1       ;
  float chi2       ;
  float disp       ;
  float tcrk       ;
  float cross_phi  ;
  float cross_z    ;
  float center_phi ;
  float center_z   ;
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
  float tecsdphi   ;
  float tecsdalpha ;
  float m2tof      ;
  float m2emc      ;
  float isPi       ;
  float isK        ;
  float isP        ;
  short dcarm	;
  short dcside	;
  short pc1sect	;
  short pc2sect	;
  short pc3sect	;
  float emcsdphi_e	;
  float emcsdz_e	;
  short tecnhit	;
  short n2	;
  float npe2	;
  short n3	;
  float npe3	;
  int deadmap	;
  int warnmap	;
  float emce	;
  short pc1id	;
  short pc2id	;
  short pc3id	;
  short emcid	;
  short tofid	;
  short tecid	;
  int   L1Trig	;
  float pc2dphi	;
  float pc2dz	;
  float pc3dphi	;
  float pc3dz	;
  float emcdphi	;
  float emcdz	;
  float tofdphi	;
  float tofdz	;
  float tecdphi	;
  float tecdalpha	;
  int ring	;
  float pc1phi	;
  float pc1z	;
  float pc2phi	;
  float pc2z	;
  float pc3phi	;
  float pc3z	;
  float emcphi	;
  float emcz	;
  float tofphi	;
  float tofz	;
  float tecphi	;
  float tecalpha	;
  int RawL1     ;
  int LivL1     ;
  int SclL1     ;



  ClassDef(PHSnglCentralTrackv9,1)
};

#endif /* PHHSNGLCENTRALTRACKV9 */

