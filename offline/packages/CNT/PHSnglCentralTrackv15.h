#ifndef __PHSNGLCENTRALTRACKV15_H_
#define __PHSNGLCENTRALTRACKV15_H_

#include "PHObject.h"
#include "PHSnglCentralTrack.h"

class PHSnglCentralTrackv15 : public PHSnglCentralTrack
{
 public:
  PHSnglCentralTrackv15();
  PHSnglCentralTrackv15(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv15() {}

  void identify(std::ostream &os=std::cout) const;

  // this is nececessary because set_mom, set_phi0, and set_the0 methods
  // use set_Px(get_px) methods internally  
  void Copy(const PHSnglCentralTrack &src);
                                                                                                                             
  // Here are the very explicit set routines...
  void set_charge     (const short val) {charge     =val; return;}
  void set_quality    (const short val) {quality    =val; return;}
  void set_zed        (const float val) {zed        =val; return;}
  void set_phi        (const float val) {phi        =val; return;}
  void set_alpha      (const float val) {alpha      =val; return;}
  void set_beta       (const float val) {beta       =val; return;}
  void set_momphi0the0 (const float valmom, const float valphi0, const float valthe0); // see .C file
  void set_phi0       (const float val); // see .C file
  void set_the0       (const float val); // see .C file
  void set_mom        (const float val); // see .C file
  void set_nx1hits    (const short val) {nx1hits    =val; return;}
  void set_nx2hits    (const short val) {nx2hits    =val; return;}
  void set_ppc1x      (const float val) {ppc1x      =val; return;}
  void set_ppc1y      (const float val) {ppc1y      =val; return;}
  void set_ppc1z      (const float val) {ppc1z      =val; return;}
  void set_ppc2x      (const float val) {ppc2x      =val; return;}
  void set_ppc2y      (const float val) {ppc2y      =val; return;}
  void set_ppc2z      (const float val) {ppc2z      =val; return;}
  void set_ppc3x      (const float val) {ppc3x      =val; return;}
  void set_ppc3y      (const float val) {ppc3y      =val; return;}
  void set_ppc3z      (const float val) {ppc3z      =val; return;}
  void set_pemcx      (const float val) {pemcx      =val; return;}
  void set_pemcy      (const float val) {pemcy      =val; return;}
  void set_pemcz      (const float val) {pemcz      =val; return;}
  void set_plemc      (const float val) {plemc      =val; return;}
  void set_sect       (const short val) {sect       =val; return;}
  void set_ysect      (const short val) {ysect      =val; return;}
  void set_zsect      (const short val) {zsect      =val; return;}
  void set_ecore      (const float val) {ecore      =val; return;}
  void set_emce       (const float val) {emce       =val; return;}
  void set_emcdispy   (const float val) {emcdispy   =val; return;}
  void set_emcdispz   (const float val) {emcdispz   =val; return;}
  void set_temc       (const float val) {temc       =val; return;}
  void set_prob       (const float val) {prob       =val; return;}
  void set_ecent      (const float val) {ecent      =val; return;}
  void set_twrhit     (const short val) {twrhit     =val; return;}
  void set_e9         (const float val) {e9         =val; return;}
  void set_emcchi2    (const float val) {emcchi2    =val; return;}
  void set_secore     (const float val) {secore     =val; return;}
  void set_semce      (const float val) {semce      =val; return;}
  void set_stemc      (const float val) {stemc      =val; return;}
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
  void set_sn0        (const short val) {sn0        =val; return;}
  void set_snpe0      (const float val) {snpe0      =val; return;}
  void set_sn1        (const short val) {sn1        =val; return;}
  void set_snpe1      (const float val) {snpe1      =val; return;}
  void set_schi2      (const float val) {schi2      =val; return;}
  void set_sdisp      (const float val) {sdisp      =val; return;}
  void set_stcrk      (const float val) {stcrk      =val; return;}
  void set_pc3sdphi   (const float val) {pc3sdphi   =val; return;}
  void set_pc3sdz     (const float val) {pc3sdz     =val; return;}
  void set_emcsdphi   (const float val) {emcsdphi   =val; return;}
  void set_emcsdz     (const float val) {emcsdz     =val; return;}
  void set_spc3sdphi  (const float val) {spc3sdphi  =val; return;}
  void set_spc3sdz    (const float val) {spc3sdz    =val; return;}
  void set_semcsdphi  (const float val) {semcsdphi  =val; return;}
  void set_semcsdz    (const float val) {semcsdz    =val; return;}
  void set_m2tof      (const float val) {m2tof      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_dcarm      (const short val) {dcarm	    = val; return;}
  void set_dcside     (const short val) {dcside	    = val; return;}
  void set_emcsdphi_e (const float val) {emcsdphi_e = val; return;}
  void set_emcsdz_e   (const float val) {emcsdz_e   = val; return;}
  void set_semcsdphi_e(const float val) {semcsdphi_e= val; return;}
  void set_semcsdz_e  (const float val) {semcsdz_e  = val; return;}
  void set_deadmap    (const int val)   {deadmap    = val; return;}
  void set_warnmap    (const int val)   {warnmap    = val; return;}
  void set_sdeadmap   (const int val)   {sdeadmap   = val; return;}
  void set_swarnmap   (const int val)   {swarnmap   = val; return;}
  void set_pc3dphi      (const float val) {pc3dphi      = val; return;}
  void set_pc3dz        (const float val) {pc3dz        = val; return;}
  void set_emcdphi      (const float val) {emcdphi      = val; return;}
  void set_emcdz        (const float val) {emcdz        = val; return;}
  void set_scross_phi   (const float val) {scross_phi   = val; return;}
  void set_scross_z     (const float val) {scross_z     = val; return;}
  void set_se9          (const float val) {se9  = val; return;}
  void set_Px          (const float val) {Px  = val; return;}
  void set_Py          (const float val) {Py  = val; return;}
  void set_Pz          (const float val) {Pz  = val; return;}


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
  float get_ppc3x      () const  { return  ppc3x       ;}
  float get_ppc3y      () const  { return  ppc3y       ;}
  float get_ppc3z      () const  { return  ppc3z       ;}
  float get_pemcx      () const  { return  pemcx       ;}
  float get_pemcy      () const  { return  pemcy       ;}
  float get_pemcz      () const  { return  pemcz       ;}
  float get_plemc      () const  { return  plemc       ;}
  short get_sect       () const  { return  sect        ;}
  short get_ysect      () const  { return  ysect       ;}
  short get_zsect      () const  { return  zsect       ;}
  float get_ecore      () const  { return  ecore       ;}
  float get_emce       () const  { return  emce      ;}
  float get_emcdispy   () const  { return  emcdispy  ;}
  float get_emcdispz   () const  { return  emcdispz  ;}
  float get_temc       () const  { return  temc        ;}
  float get_prob       () const  { return  prob        ;}
  float get_ecent      () const  { return  ecent       ;}
  short get_twrhit     () const  { return  twrhit      ;}
  float get_e9         () const  { return  e9          ;}
  float get_emcchi2    () const  { return  emcchi2     ;}
  float get_secore     () const  { return  secore      ;}
  float get_semce      () const  { return  semce       ;}
  float get_stemc      () const  { return  stemc       ;}
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
  short get_sn0        () const  { return  sn0         ;}
  float get_snpe0      () const  { return  snpe0       ;}
  short get_sn1        () const  { return  sn1         ;}
  float get_snpe1      () const  { return  snpe1       ;}
  float get_schi2      () const  { return  schi2       ;}
  float get_sdisp      () const  { return  sdisp       ;}
  float get_stcrk      () const  { return  stcrk       ;}
  float get_pc3sdphi   () const  { return  pc3sdphi    ;}
  float get_pc3sdz     () const  { return  pc3sdz      ;}
  float get_emcsdphi   () const  { return  emcsdphi    ;}
  float get_emcsdz     () const  { return  emcsdz      ;}
  float get_spc3sdphi  () const  { return  spc3sdphi   ;}
  float get_spc3sdz    () const  { return  spc3sdz     ;}
  float get_semcsdphi  () const  { return  semcsdphi   ;}
  float get_semcsdz    () const  { return  semcsdz     ;}
  float get_m2tof      () const  { return  m2tof       ;}
  float get_m2emc      () const  { return  m2emc       ;}
  short get_dcarm	() const { return dcarm	        ;}
  short get_dcside	() const { return dcside        ;}
  float get_emcsdphi_e	() const { return emcsdphi_e	;}
  float get_emcsdz_e	() const { return emcsdz_e	;}
  float get_semcsdphi_e	() const { return semcsdphi_e	;}
  float get_semcsdz_e	() const { return semcsdz_e	;}
  int   get_deadmap	() const { return deadmap	;}
  int   get_warnmap	() const { return warnmap	;}
  int   get_sdeadmap	() const { return sdeadmap	;}
  int   get_swarnmap	() const { return swarnmap	;}
  float get_pc3dphi     () const { return pc3dphi       ;}
  float get_pc3dz       () const { return pc3dz ;}
  float get_emcdphi     () const { return emcdphi       ;}
  float get_emcdz       () const { return emcdz ;}
  float get_scross_phi  () const { return scross_phi    ;}
  float get_scross_z    () const { return scross_z      ;}
  float get_se9         () const { return se9   ;}
  float get_Px          () const { return Px   ;}
  float get_Py          () const { return Py   ;}
  float get_Pz          () const { return Pz   ;}


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
  float ppc3x      ;
  float ppc3y      ;
  float ppc3z      ;
  float pemcx      ;
  float pemcy      ;
  float pemcz      ;
  float plemc      ;
  short sect       ;
  short ysect      ;
  short zsect      ;
  float ecore      ;
  float emce       ;
  float emcdispy   ;
  float emcdispz   ;
  float temc       ;
  float prob       ;
  float ecent      ;
  short twrhit     ;
  float e9         ;
  float emcchi2    ;
  float secore     ;
  float semce      ;
  float stemc      ;
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
  short sn0        ;
  float snpe0      ;
  short sn1        ;
  float snpe1      ;
  float schi2      ;
  float sdisp      ;
  float stcrk      ;
  float pc3sdphi   ;
  float pc3sdz     ;
  float emcsdphi   ;
  float emcsdz     ;
  float spc3sdphi  ;
  float spc3sdz    ;
  float semcsdphi  ;
  float semcsdz    ;
  float m2tof      ;
  float m2emc      ;
  short dcarm	;
  short dcside	;
  float emcsdphi_e	;
  float emcsdz_e	;
  float semcsdphi_e	;
  float semcsdz_e	;
  int deadmap	;
  int warnmap	;
  int sdeadmap	;
  int swarnmap	;
  float pc3dphi ;
  float pc3dz   ;
  float emcdphi ;
  float emcdz   ;
  float scross_phi      ;
  float scross_z        ;
  float se9     ;
  float Px     ;
  float Py     ;
  float Pz     ;


  ClassDef(PHSnglCentralTrackv15,1)
};

#endif 

