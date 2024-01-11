#ifndef __PHSNGLCENTRALTRACKV4_H_
#define __PHSNGLCENTRALTRACKV4_H_

#include "PHObject.h"

class PHSnglCentralTrackv4 : public TObject
{
 public:
  PHSnglCentralTrackv4();
  PHSnglCentralTrackv4(const PHSnglCentralTrackv4 &track);  
  virtual ~PHSnglCentralTrackv4() {}

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
  void set_status     (const short val) {status     =val; return;}
  void set_alpha1     (const float val) {alpha1     =val; return;}
  void set_alpha2     (const float val) {alpha2     =val; return;}
  void set_nx1hits    (const short val) {nx1hits    =val; return;}
  void set_nx2hits    (const short val) {nx2hits    =val; return;}
  void set_nx1x2fit   (const short val) {nx1x2fit   =val; return;}
  void set_alphaf     (const float val) {alphaf     =val; return;}
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
  void set_secorr     (const float val) {secorr     =val; return;}
  void set_secore     (const float val) {secore     =val; return;}
  void set_stemc      (const float val) {stemc      =val; return;}
  void set_sprob      (const float val) {sprob      =val; return;}
  void set_stwrhit    (const short val) {stwrhit    =val; return;}
  void set_semcchi2   (const float val) {semcchi2   =val; return;}
  void set_slat       (const int   val) {slat       =val; return;}
  void set_ttof       (const float val) {ttof       =val; return;}
  void set_etof       (const float val) {etof       =val; return;}
  void set_sttof      (const float val) {sttof      =val; return;}
  void set_setof      (const float val) {setof      =val; return;}
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
  void set_spc2sdphi  (const float val) {spc2sdphi  =val; return;}
  void set_spc2sdz    (const float val) {spc2sdz    =val; return;}
  void set_spc3sdphi  (const float val) {spc3sdphi  =val; return;}
  void set_spc3sdz    (const float val) {spc3sdz    =val; return;}
  void set_semcsdphi  (const float val) {semcsdphi  =val; return;}
  void set_semcsdz    (const float val) {semcsdz    =val; return;}
  void set_stofsdphi  (const float val) {stofsdphi  =val; return;}
  void set_stofsdz    (const float val) {stofsdz    =val; return;}
  void set_stecsdphi  (const float val) {stecsdphi  =val; return;}
  void set_stecsdalpha(const float val) {stecsdalpha=val; return;}
  void set_m2tof      (const float val) {m2tof      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}
  void set_categoryl2eLowPt(const long input) {categoryl2eLowPt = input;}
  void set_categoryl2eHighPt(const long input){categoryl2eHighPt = input;}
  void set_candIDl2e(const short input)       { candIDl2e = input;}
  void set_dcarm      (const short val) {dcarm	    = val; return;}
  void set_dcside     (const short val) {dcside	    = val; return;}
  void set_pc1sect    (const short val) {pc1sect    = val; return;}
  void set_pc2sect    (const short val) {pc2sect    = val; return;}
  void set_pc3sect    (const short val) {pc3sect    = val; return;}
  void set_emcsdphi_e (const float val) {emcsdphi_e = val; return;}
  void set_emcsdz_e   (const float val) {emcsdz_e   = val; return;}
  void set_semcsdphi_e(const float val) {semcsdphi_e= val; return;}
  void set_semcsdz_e  (const float val) {semcsdz_e  = val; return;}
  void set_tecnhit    (const short val) {tecnhit    = val; return;}
  void set_mx1dist    (const float val) {mx1dist    = val; return;}
  void set_mx2dist    (const float val) {mx2dist    = val; return;}
  void set_mchi2      (const float val) {mchi2	    = val; return;}
  void set_n2	      (const short val) {n2	    = val; return;}
  void set_npe2	      (const float val) {npe2	    = val; return;}
  void set_n3	      (const short val) {n3	    = val; return;}
  void set_npe3	      (const float val) {npe3	    = val; return;}
  void set_sn2	      (const short val) {sn2	    = val; return;}
  void set_snpe2      (const float val) {snpe2	    = val; return;}
  void set_sn3	      (const short val) {sn3	    = val; return;}
  void set_snpe3      (const float val) {snpe3	    = val; return;}
  void set_deadmap    (const int val)   {deadmap    = val; return;}
  void set_warnmap    (const int val)   {warnmap    = val; return;}
  void set_sdeadmap   (const int val)   {sdeadmap   = val; return;}
  void set_swarnmap   (const int val)   {swarnmap   = val; return;}

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
  short get_status     () const  { return  status      ;}
  float get_alpha1     () const  { return  alpha1      ;}
  float get_alpha2     () const  { return  alpha2      ;}
  short get_nx1hits    () const  { return  nx1hits     ;}
  short get_nx2hits    () const  { return  nx2hits     ;}
  short get_nx1x2fit   () const  { return  nx1x2fit    ;}
  float get_alphaf     () const  { return  alphaf      ;}
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
  float get_secorr     () const  { return  secorr      ;}
  float get_secore     () const  { return  secore      ;}
  float get_stemc      () const  { return  stemc       ;}
  float get_sprob      () const  { return  sprob       ;}
  short get_stwrhit    () const  { return  stwrhit     ;}
  float get_semcchi2   () const  { return  semcchi2    ;}
  int   get_slat       () const  { return  slat        ;}
  float get_ttof       () const  { return  ttof        ;}
  float get_etof       () const  { return  etof        ;}
  float get_sttof      () const  { return  sttof       ;}
  float get_setof      () const  { return  setof       ;}
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
  float get_spc2sdphi  () const  { return  spc2sdphi   ;}
  float get_spc2sdz    () const  { return  spc2sdz     ;}
  float get_spc3sdphi  () const  { return  spc3sdphi   ;}
  float get_spc3sdz    () const  { return  spc3sdz     ;}
  float get_semcsdphi  () const  { return  semcsdphi   ;}
  float get_semcsdz    () const  { return  semcsdz     ;}
  float get_stofsdphi  () const  { return  stofsdphi   ;}
  float get_stofsdz    () const  { return  stofsdz     ;}
  float get_stecsdphi  () const  { return  stecsdphi   ;}
  float get_stecsdalpha() const  { return  stecsdalpha  ;}
  float get_m2tof      () const  { return  m2tof       ;}
  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}
  long get_categoryl2eLowPt() const { return categoryl2eLowPt;}
  long get_categoryl2eHighPt() const { return categoryl2eHighPt;}
  short get_candIDl2e() const { return candIDl2e;}
  short get_dcarm	() const { return dcarm	        ;}
  short get_dcside	() const { return dcside        ;}
  short get_pc1sect	() const { return pc1sect	;}
  short get_pc2sect	() const { return pc2sect	;}
  short get_pc3sect	() const { return pc3sect	;}
  float get_emcsdphi_e	() const { return emcsdphi_e	;}
  float get_emcsdz_e	() const { return emcsdz_e	;}
  float get_semcsdphi_e	() const { return semcsdphi_e	;}
  float get_semcsdz_e	() const { return semcsdz_e	;}
  short get_tecnhit	() const { return tecnhit	;}
  float get_mx1dist	() const { return mx1dist	;}
  float get_mx2dist	() const { return mx2dist	;}
  float get_mchi2	() const { return mchi2	        ;}
  short get_n2	        () const { return n2	        ;}
  float get_npe2	() const { return npe2	        ;}
  short get_n3	        () const { return n3	        ;}
  float get_npe3	() const { return npe3	        ;}
  short get_sn2	        () const { return sn2	        ;}
  float get_snpe2	() const { return snpe2	        ;}
  short get_sn3         () const { return sn3	        ;}
  float get_snpe3	() const { return snpe3	        ;}
  int   get_deadmap	() const { return deadmap	;}
  int   get_warnmap	() const { return warnmap	;}
  int   get_sdeadmap	() const { return sdeadmap	;}
  int   get_swarnmap	() const { return swarnmap	;}

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
  short status     ;
  float alpha1     ;
  float alpha2     ;
  short nx1hits    ;
  short nx2hits    ;
  short nx1x2fit   ;
  float alphaf     ;
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
  float secorr     ;
  float secore     ;
  float stemc      ;
  float sprob      ;
  short stwrhit    ;
  float semcchi2   ;
  int   slat       ;
  float ttof       ;
  float etof       ;
  float sttof      ;
  float setof      ;
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
  float spc2sdphi  ;
  float spc2sdz    ;
  float spc3sdphi  ;
  float spc3sdz    ;
  float semcsdphi  ;
  float semcsdz    ;
  float stofsdphi  ;
  float stofsdz    ;
  float stecsdphi  ;
  float stecsdalpha;
  float m2tof      ;
  float m2emc      ;
  float isPi       ;
  float isK        ;
  float isP        ;
  long categoryl2eLowPt;
  long categoryl2eHighPt;
  short candIDl2e;
  short dcarm	;
  short dcside	;
  short pc1sect	;
  short pc2sect	;
  short pc3sect	;
  float emcsdphi_e	;
  float emcsdz_e	;
  float semcsdphi_e	;
  float semcsdz_e	;
  short tecnhit	;
  float mx1dist	;
  float mx2dist	;
  float mchi2	;
  short n2	;
  float npe2	;
  short n3	;
  float npe3	;
  short sn2	;
  float snpe2	;
  short sn3	;
  float snpe3	;
  int deadmap	;
  int warnmap	;
  int sdeadmap	;
  int swarnmap	;

  ClassDef(PHSnglCentralTrackv4,1)
};

#endif /* PHHSNGLCENTRALTRACKV4 */

