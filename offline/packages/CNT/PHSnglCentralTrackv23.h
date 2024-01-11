#ifndef __PHSNGLCENTRALTRACKV23_H_
#define __PHSNGLCENTRALTRACKV23_H_

#include "PHObject.h"
#include "PHSnglCentralTrack.h"

class PHSnglCentralTrackv23 : public PHSnglCentralTrack
{
 public:
  PHSnglCentralTrackv23();
  PHSnglCentralTrackv23(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv23() {}

  void identify(std::ostream &os=std::cout) const;


  ///////////////////////////////////////
  // This version 23 is originally inherited from v22, just add isPiTofw, isKTofw, isPTofw


  ////////////////
  // Methods from v17

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
  void set_ptofx      (const float val) {if(get_dcarm()==0) ptofx      =val; return;}
  void set_ptofy      (const float val) {if(get_dcarm()==0) ptofy      =val; return;}
  void set_ptofz      (const float val) {if(get_dcarm()==0) ptofz      =val; return;}
  void set_pltof      (const float val) {if(get_dcarm()==0) pltof      =val; return;}
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
  void set_re9        (const float val) {re9        =val; return;}
  void set_emcchi2    (const float val) {emcchi2    =val; return;}
  void set_secore     (const float val) {secore     =val; return;}
  void set_semce      (const float val) {semce      =val; return;}
  void set_semcdispy  (const float val) {semcdispy  =val; return;}
  void set_semcdispz  (const float val) {semcdispz  =val; return;}
  void set_stemc      (const float val) {stemc      =val; return;}
  void set_sprob      (const float val) {sprob      =val; return;}
  void set_stwrhit    (const short val) {stwrhit    =val; return;}
  void set_semcchi2   (const float val) {semcchi2   =val; return;}
  void set_slat       (const int   val) {if(get_dcarm()==0) slat       =val; return;}
  void set_ttof       (const float val) {if(get_dcarm()==0) ttof       =val; return;}
  void set_etof       (const float val) {if(get_dcarm()==0) etof       =val; return;}
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
  void set_pc2sdphi   (const float val) {pc2sdphi   =val; return;}
  void set_pc2sdz     (const float val) {pc2sdz     =val; return;}
  void set_pc3sdphi   (const float val) {pc3sdphi   =val; return;}
  void set_pc3sdz     (const float val) {pc3sdz     =val; return;}
  void set_emcsdphi   (const float val) {emcsdphi   =val; return;}
  void set_emcsdz     (const float val) {emcsdz     =val; return;}
  void set_tofsdphi   (const float val) {if(get_dcarm()==0) tofsdphi   =val; return;}
  void set_tofsdz     (const float val) {if(get_dcarm()==0) tofsdz     =val; return;}
  void set_tecsdphi   (const float val) {tecsdphi   =val; return;}
  void set_tecsdalpha (const float val) {tecsdalpha =val; return;}
  void set_spc2sdphi  (const float val) {spc2sdphi  =val; return;}
  void set_spc2sdz    (const float val) {spc2sdz    =val; return;}
  void set_spc3sdphi  (const float val) {spc3sdphi  =val; return;}
  void set_spc3sdz    (const float val) {spc3sdz    =val; return;}
  void set_semcsdphi  (const float val) {semcsdphi  =val; return;}
  void set_semcsdz    (const float val) {semcsdz    =val; return;}
  void set_stofsdphi  (const float val) {if(get_dcarm()==0) stofsdphi  =val; return;}
  void set_stofsdz    (const float val) {if(get_dcarm()==0) stofsdz    =val; return;}
  void set_stofdphi   (const float val) {if(get_dcarm()==0) stofdphi   =val; return;}
  void set_stofdz     (const float val) {if(get_dcarm()==0) stofdz     =val; return;}
  void set_stecsdphi  (const float val) {stecsdphi  =val; return;}
  void set_stecsdalpha(const float val) {stecsdalpha=val; return;}
  void set_m2tof      (const float val) {if(get_dcarm()==0) m2tof      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}
  void set_isPiTofw   (const float val) {isPiTofw       =val; return;}
  void set_isKTofw    (const float val) {isKTofw        =val; return;}
  void set_isPTofw    (const float val) {isPTofw        =val; return;}
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
  void set_pc1id        (const short val) {pc1id        = val; return;}
  void set_pc2id        (const short val) {pc2id        = val; return;}
  void set_pc3id        (const short val) {pc3id        = val; return;}
  void set_emcid        (const short val) {emcid        = val; return;}
  void set_tofid        (const short val) {if(get_dcarm()==0) tofid        = val; return;}
  void set_tecid        (const short val) {tecid        = val; return;}
  void set_ring (const int val) {ring   = val; return;}
  void set_pc2dphi      (const float val) {pc2dphi      = val; return;}
  void set_pc2dz        (const float val) {pc2dz        = val; return;}
  void set_pc3dphi      (const float val) {pc3dphi      = val; return;}
  void set_pc3dz        (const float val) {pc3dz        = val; return;}
  void set_emcdphi      (const float val) {emcdphi      = val; return;}
  void set_emcdz        (const float val) {emcdz        = val; return;}
  void set_tofdphi      (const float val) {if(get_dcarm()==0) tofdphi      = val; return;}
  void set_tofdz        (const float val) {if(get_dcarm()==0) tofdz        = val; return;}
  void set_tecdphi      (const float val) {tecdphi      = val; return;}
  void set_tecdalpha    (const float val) {tecdalpha    = val; return;}
  void set_tofph1       (const float val) {if(get_dcarm()==0) tofph1       = val; return;}
  void set_tofph2       (const float val) {if(get_dcarm()==0) tofph2       = val; return;}
  void set_toftdc1      (const float val) {if(get_dcarm()==0) toftdc1      = val; return;}
  void set_toftdc2      (const float val) {if(get_dcarm()==0) toftdc2      = val; return;}
  void set_scross_phi   (const float val) {scross_phi   = val; return;}
  void set_scross_z     (const float val) {scross_z     = val; return;}
  void set_spc3phi      (const float val) {spc3phi      = val; return;}
  void set_spc3z        (const float val) {spc3z        = val; return;}
  void set_mcid         (const int val)   {mcid   = val; return;}
  void set_dchid        (const int val)   {dchid  = val; return;}
  void set_emcrawtdc    (const int val)   {emcrawtdc      = val; return;}
  void set_emcrawadc    (const int val)   {emcrawadc      = val; return;}
  void set_emcrawadclg  (const int val)   {emcrawadclg    = val; return;}
  void set_se9          (const float val) {se9  = val; return;}
  void set_secent       (const float val) {secent       = val; return;}
  void set_aerindex     (const int val)   {aerindex       = val; return;}
  void set_aersindex    (const int val)   {aersindex      = val; return;}


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
  float get_ptofx      () const  { if(get_dcarm()==0) return  ptofx       ;else return -9999;}
  float get_ptofy      () const  { if(get_dcarm()==0) return  ptofy       ;else return -9999;}
  float get_ptofz      () const  { if(get_dcarm()==0) return  ptofz       ;else return -9999;}
  float get_pltof      () const  { if(get_dcarm()==0) return  pltof       ;else return -9999;}
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
  float get_re9        () const  { return  re9         ;}
  float get_emcchi2    () const  { return  emcchi2     ;}
  float get_secore     () const  { return  secore      ;}
  float get_semce      () const  { return  semce       ;}
  float get_semcdispy  () const  { return  semcdispy   ;}
  float get_semcdispz  () const  { return  semcdispz   ;}
  float get_stemc      () const  { return  stemc       ;}
  float get_sprob      () const  { return  sprob       ;}
  short get_stwrhit    () const  { return  stwrhit     ;}
  float get_semcchi2   () const  { return  semcchi2    ;}
  int   get_slat       () const  { if(get_dcarm()==0) return  slat        ;else return -9999;}
  float get_ttof       () const  { if(get_dcarm()==0) return  ttof        ;else return -9999;}
  float get_etof       () const  { if(get_dcarm()==0) return  etof        ;else return -9999;}
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
  float get_pc2sdphi   () const  { return  pc2sdphi    ;}
  float get_pc2sdz     () const  { return  pc2sdz      ;}
  float get_pc3sdphi   () const  { return  pc3sdphi    ;}
  float get_pc3sdz     () const  { return  pc3sdz      ;}
  float get_emcsdphi   () const  { return  emcsdphi    ;}
  float get_emcsdz     () const  { return  emcsdz      ;}
  float get_tofsdphi   () const  { if(get_dcarm()==0) return  tofsdphi    ;else return -9999;}
  float get_tofsdz     () const  { if(get_dcarm()==0) return  tofsdz      ;else return -9999;}
  float get_tecsdphi   () const  { return  tecsdphi    ;}
  float get_tecsdalpha () const  { return  tecsdalpha  ;}
  float get_spc2sdphi  () const  { return  spc2sdphi   ;}
  float get_spc2sdz    () const  { return  spc2sdz     ;}
  float get_spc3sdphi  () const  { return  spc3sdphi   ;}
  float get_spc3sdz    () const  { return  spc3sdz     ;}
  float get_semcsdphi  () const  { return  semcsdphi   ;}
  float get_semcsdz    () const  { return  semcsdz     ;}
  float get_stofsdphi  () const  { if(get_dcarm()==0) return  stofsdphi   ;else return -9999;}
  float get_stofsdz    () const  { if(get_dcarm()==0) return  stofsdz     ;else return -9999;}
  float get_stofdphi   () const  { if(get_dcarm()==0) return  stofdphi    ;else return -9999;}
  float get_stofdz     () const  { if(get_dcarm()==0) return  stofdz      ;else return -9999;}
  float get_stecsdphi  () const  { return  stecsdphi   ;}
  float get_stecsdalpha() const  { return  stecsdalpha  ;}
  float get_m2tof      () const  { if(get_dcarm()==0) return  m2tof       ;else return -9999;}
  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}
  float get_isPiTofw   () const  { return  isPiTofw    ;}
  float get_isKTofw    () const  { return  isKTofw     ;}
  float get_isPTofw    () const  { return  isPTofw     ;}
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
  short get_pc1id       () const { return pc1id ;}
  short get_pc2id       () const { return pc2id ;}
  short get_pc3id       () const { return pc3id ;}
  short get_emcid       () const { return emcid ;}
  short get_tofid       () const { if(get_dcarm()==0) return tofid ;else return -9999;}
  short get_tecid       () const { return tecid ;}
  int get_ring  () const { return ring  ;}
  float get_pc2dphi     () const { return pc2dphi       ;}
  float get_pc2dz       () const { return pc2dz ;}
  float get_pc3dphi     () const { return pc3dphi       ;}
  float get_pc3dz       () const { return pc3dz ;}
  float get_emcdphi     () const { return emcdphi       ;}
  float get_emcdz       () const { return emcdz ;}
  float get_tofdphi     () const { if(get_dcarm()==0) return tofdphi       ;else return -9999;}
  float get_tofdz       () const { if(get_dcarm()==0) return tofdz ;else return -9999;}
  float get_tecdphi     () const { return tecdphi       ;}
  float get_tecdalpha   () const { return tecdalpha     ;}
  float get_tofph1      () const { if(get_dcarm()==0) return tofph1  ;else return -9999;}
  float get_tofph2      () const { if(get_dcarm()==0) return tofph2  ;else return -9999;}
  float get_toftdc1     () const { if(get_dcarm()==0) return toftdc1 ;else return -9999;}
  float get_toftdc2     () const { if(get_dcarm()==0) return toftdc2 ;else return -9999;}
  float get_scross_phi  () const { return scross_phi    ;}
  float get_scross_z    () const { return scross_z      ;}
  float get_spc3phi     () const { return spc3phi       ;}
  float get_spc3z       () const { return spc3z ;}
  int   get_mcid        () const { return mcid  ;}
  int   get_dchid       () const { return dchid ;}
  int   get_emcrawtdc   () const { return emcrawtdc     ;}
  int   get_emcrawadc   () const { return emcrawadc     ;}
  int   get_emcrawadclg () const { return emcrawadclg   ;}
  float get_se9         () const { return se9   ;}
  float get_secent      () const { return secent        ;}
  int   get_aerindex    () const { return aerindex        ;}
  int   get_aersindex   () const { return aersindex        ;}


  ///////////////////////
  // Methods from v19


  // Here are the very explicit set routines...
  void set_stecid       (const short val) {stecid       = val; return;}


  // Here are the very explicit "get" routines...
  short get_stecid      () const { return stecid ;}


  ///////////////////////
  // Methods from v21

  // tofw
  void set_tofwid     (const short val) {if(get_dcarm()==1) tofid     = val; return;}
  void set_striptofw  (const int   val) {if(get_dcarm()==1) slat      = val; return;}
  void set_ttofw      (const float val) {if(get_dcarm()==1) ttof      = val; return;}  
  void set_qtofw      (const float val) {if(get_dcarm()==1) etof      = val; return;}
  void set_tofwtdcup  (const float val) {if(get_dcarm()==1) toftdc1   = val; return;}
  void set_tofwtdcdw  (const float val) {if(get_dcarm()==1) toftdc2   = val; return;}
  void set_tofwadcup  (const float val) {if(get_dcarm()==1) tofph1    = val; return;}
  void set_tofwadcdw  (const float val) {if(get_dcarm()==1) tofph2    = val; return;}

  void set_ptofwx     (const float val) {if(get_dcarm()==1) ptofx     = val; return;}
  void set_ptofwy     (const float val) {if(get_dcarm()==1) ptofy     = val; return;}
  void set_ptofwz     (const float val) {if(get_dcarm()==1) ptofz     = val; return;}
  void set_pltofw     (const float val) {if(get_dcarm()==1) pltof     = val; return;}
 
  void set_tofwdphi   (const float val) {if(get_dcarm()==1) tofdphi   = val; return;}
  void set_tofwdz     (const float val) {if(get_dcarm()==1) tofdz     = val; return;}
  void set_tofwsdphi  (const float val) {if(get_dcarm()==1) tofsdphi  = val; return;}
  void set_tofwsdz    (const float val) {if(get_dcarm()==1) tofsdz    = val; return;}

  void set_m2tofw     (const float val) {if(get_dcarm()==1) m2tof     = val; return;}


  short get_tofwid     () const { if(get_dcarm()==1) return  tofid      ;else return -9999;}
  int   get_striptofw  () const { if(get_dcarm()==1) return  slat       ;else return -9999;}
  float get_ttofw      () const { if(get_dcarm()==1) return  ttof       ;else return -9999;}
  float get_qtofw      () const { if(get_dcarm()==1) return  etof       ;else return -9999;}
  float get_tofwtdcup  () const { if(get_dcarm()==1) return  toftdc1    ;else return -9999;}
  float get_tofwtdcdw  () const { if(get_dcarm()==1) return  toftdc2    ;else return -9999;}
  float get_tofwadcup  () const { if(get_dcarm()==1) return  tofph1     ;else return -9999;}
  float get_tofwadcdw  () const { if(get_dcarm()==1) return  tofph2     ;else return -9999;}

  float get_ptofwx     () const { if(get_dcarm()==1) return ptofx       ;else return -9999;}
  float get_ptofwy     () const { if(get_dcarm()==1) return ptofy       ;else return -9999;}
  float get_ptofwz     () const { if(get_dcarm()==1) return ptofz       ;else return -9999;}
  float get_pltofw     () const { if(get_dcarm()==1) return pltof       ;else return -9999;}

  float get_tofwdphi   () const { if(get_dcarm()==1) return tofdphi     ;else return -9999;}
  float get_tofwdz     () const { if(get_dcarm()==1) return tofdz       ;else return -9999;}
  float get_tofwsdphi  () const { if(get_dcarm()==1) return tofsdphi    ;else return -9999;}
  float get_tofwsdz    () const { if(get_dcarm()==1) return tofsdz      ;else return -9999;}

  float get_m2tofw     () const { if(get_dcarm()==1) return m2tof       ;else return -9999;}

  // hbd
  void set_hbdid      (const short val) {hbdid      = val; return;}
  void set_hbdsector  (const short val) {hbdsector  = val; return;}
  void set_hbdsize    (const short val) {hbdsize    = val; return;}
  void set_hbdcharge  (const float val) {hbdcharge  = val; return;}
  void set_hbdx       (const float val) {hbdx       = val; return;}
  void set_hbdy       (const float val) {hbdy       = val; return;}
  void set_hbdz       (const float val) {hbdz       = val; return;}
  void set_phbdx      (const float val) {phbdx      = val; return;}
  void set_phbdy      (const float val) {phbdy      = val; return;}
  void set_phbdz      (const float val) {phbdz      = val; return;}
  void set_hbddphi    (const float val) {hbddphi    = val; return;}
  void set_hbddz      (const float val) {hbddz      = val; return;}
  void set_hbdsdphi   (const float val) {hbdsdphi   = val; return;}
  void set_hbdsdz     (const float val) {hbdsdz     = val; return;}

  // hbd
  short get_hbdid() const {return hbdid;}
  short get_hbdsector  () const { return hbdsector    ;}
  short get_hbdsize    () const { return hbdsize      ;}
  float get_hbdcharge  () const { return hbdcharge    ;}
  float get_hbdx       () const { return hbdx         ;}
  float get_hbdy       () const { return hbdy         ;}
  float get_hbdz       () const { return hbdz         ;}
  float get_phbdx      () const { return phbdx        ;}
  float get_phbdy      () const { return phbdy        ;}
  float get_phbdz      () const { return phbdz        ;}
  float get_hbddphi    () const { return hbddphi      ;}
  float get_hbddz      () const { return hbddz        ;}
  float get_hbdsdphi   () const { return hbdsdphi     ;}
  float get_hbdsdz     () const { return hbdsdz       ;}


  ///////////////////////
  // Methods new on v22, swapping values

  void set_spc2dphi  (const float val) {spc2dphi  =val; return;}
  void set_spc2dz    (const float val) {spc2dz    =val; return;}
  void set_spc3dphi  (const float val) {spc3dphi  =val; return;}
  void set_spc3dz    (const float val) {spc3dz    =val; return;}
  void set_semcdphi  (const float val) {semcdphi  =val; return;}
  void set_semcdz    (const float val) {semcdz    =val; return;}

  float get_spc2dphi  () const  { return  spc2dphi   ;}
  float get_spc2dz    () const  { return  spc2dz     ;}
  float get_spc3dphi  () const  { return  spc3dphi   ;}
  float get_spc3dz    () const  { return  spc3dz     ;}
  float get_semcdphi  () const  { return  semcdphi   ;}
  float get_semcdz    () const  { return  semcdz     ;}

  void set_stofwdphi   (const float val) {if(get_dcarm()==1) stofdphi   = val; return;}
  void set_stofwdz     (const float val) {if(get_dcarm()==1) stofdz     = val; return;}
  void set_stofwsdphi  (const float val) {if(get_dcarm()==1) stofsdphi  = val; return;}
  void set_stofwsdz    (const float val) {if(get_dcarm()==1) stofsdz    = val; return;}

  float get_stofwdphi   () const { if(get_dcarm()==1) return stofdphi     ;else return -9999;}
  float get_stofwdz     () const { if(get_dcarm()==1) return stofdz       ;else return -9999;}
  float get_stofwsdphi  () const { if(get_dcarm()==1) return stofsdphi    ;else return -9999;}
  float get_stofwsdz    () const { if(get_dcarm()==1) return stofsdz      ;else return -9999;}


 protected:

  ///////////////////////
  // members from v17

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
  float ecore      ;
  float emce       ;
  float emcdispy   ;
  float emcdispz   ;
  float temc       ;
  float prob       ;
  float ecent      ;
  short twrhit     ;
  float e9         ;
  float re9        ;
  float emcchi2    ;
  float secore     ;
  float semce      ;
  float semcdispy  ;
  float semcdispz  ;
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
  float isPiTofw   ;
  float isKTofw    ;
  float isPTofw    ;
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
  short pc1id   ;
  short pc2id   ;
  short pc3id   ;
  short emcid   ;
  short tofid   ;
  short tecid   ;
  int ring      ;
  float pc2dphi ;
  float pc2dz   ;
  float pc3dphi ;
  float pc3dz   ;
  float emcdphi ;
  float emcdz   ;
  float tofdphi ;
  float tofdz   ;
  float tecdphi ;
  float tecdalpha;
  float tofph1  ;
  float tofph2  ;
  float toftdc1  ;
  float toftdc2  ;
  float scross_phi      ;
  float scross_z        ;
  float spc3phi ;
  float spc3z   ;
  int   mcid      ;
  int   dchid     ;
  int   emcrawtdc ;
  int   emcrawadc ;
  int   emcrawadclg ;
  float se9     ;
  float secent  ;
  int   aerindex  ;
  int   aersindex ;

  //////////////////
  //  Members from v19

  short stecid   ;


  //////////////////
  //  Members from v21

  // hbd
  short hbdid;
  short hbdsector;
  short hbdsize;
  float hbdcharge;
  float hbdx;
  float hbdy;
  float hbdz;
  float phbdx;
  float phbdy;
  float phbdz;
  float hbddphi;
  float hbddz;
  float hbdsdphi;
  float hbdsdz;


  ///////////////////////
  // Members new on v22, swapping values

  float spc2dphi  ;
  float spc2dz    ;
  float spc3dphi  ;
  float spc3dz    ;
  float semcdphi  ;
  float semcdz    ;
  float stofdphi ;
  float stofdz   ;

  ClassDef(PHSnglCentralTrackv23,1)
};

#endif /* PHHSNGLCENTRALTRACKV23 */

