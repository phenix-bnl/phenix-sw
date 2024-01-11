#ifndef __PHSNGLCENTRALTRACKV24_H_
#define __PHSNGLCENTRALTRACKV24_H_

#include <PHObject.h>
#include <PHSnglCentralTrack.h>

class PHSnglCentralTrackv24 : public PHSnglCentralTrack
{
 public:
  PHSnglCentralTrackv24();
  PHSnglCentralTrackv24(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv24() {}

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
  void set_sppc1x      (const float val) {sppc1x      =val; return;}
  void set_sppc1y      (const float val) {sppc1y      =val; return;}
  void set_sppc1z      (const float val) {sppc1z      =val; return;}
  void set_ppc2x      (const float val) {ppc2x      =val; return;}
  void set_ppc2y      (const float val) {ppc2y      =val; return;}
  void set_ppc2z      (const float val) {ppc2z      =val; return;}
  void set_sppc2x      (const float val) {sppc2x      =val; return;}
  void set_sppc2y      (const float val) {sppc2y      =val; return;}
  void set_sppc2z      (const float val) {sppc2z      =val; return;}
  void set_ptecx      (const float val) {ptecx      =val; return;}
  void set_ptecy      (const float val) {ptecy      =val; return;}
  void set_ptecz      (const float val) {ptecz      =val; return;}
  void set_ppc3x      (const float val) {ppc3x      =val; return;}
  void set_ppc3y      (const float val) {ppc3y      =val; return;}
  void set_ppc3z      (const float val) {ppc3z      =val; return;}
  void set_sppc3x      (const float val) {sppc3x      =val; return;}
  void set_sppc3y      (const float val) {sppc3y      =val; return;}
  void set_sppc3z      (const float val) {sppc3z      =val; return;}
  void set_pemcx      (const float val) {pemcx      =val; return;}
  void set_pemcy      (const float val) {pemcy      =val; return;}
  void set_pemcz      (const float val) {pemcz      =val; return;}
  void set_spemcx      (const float val) {spemcx      =val; return;}
  void set_spemcy      (const float val) {spemcy      =val; return;}
  void set_spemcz      (const float val) {spemcz      =val; return;}
  void set_ptofx      (const float val) {set_ptofex(val); return;}
  void set_ptofy      (const float val) {set_ptofey(val); return;}
  void set_ptofz      (const float val) {set_ptofez(val); return;}
  void set_ptofex      (const float val) {ptofex      =val; return;}
  void set_ptofey      (const float val) {ptofey      =val; return;}
  void set_ptofez      (const float val) {ptofez      =val; return;}
  void set_sptofex      (const float val) {sptofex      =val; return;}
  void set_sptofey      (const float val) {sptofey      =val; return;}
  void set_sptofez      (const float val) {sptofez      =val; return;}
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
  void set_semcdispy  (const float val) {semcdispy  =val; return;}
  void set_semcdispz  (const float val) {semcdispz  =val; return;}
  void set_stemc      (const float val) {stemc      =val; return;}
  void set_sprob      (const float val) {sprob      =val; return;}
  void set_stwrhit    (const short val) {stwrhit    =val; return;}
  void set_semcchi2   (const float val) {semcchi2   =val; return;}
  void set_slat       (const int   val) {slat       =val; return;}
  void set_ttof       (const float val) {ttofe       =val; return;}
  void set_etof       (const float val) {etofe       =val; return;}
  void set_sttof      (const float val) {sttofe      =val; return;}
  void set_setof      (const float val) {setofe      =val; return;}
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
  void set_tofsdphi   (const float val) {tofesdphi   =val; return;}
  void set_tofsdz     (const float val) {tofesdz     =val; return;}
  void set_tecsdphi   (const float val) {tecsdphi   =val; return;}
  void set_tecsdalpha (const float val) {tecsdalpha =val; return;}
  void set_spc2sdphi  (const float val) {spc2sdphi  =val; return;}
  void set_spc2sdz    (const float val) {spc2sdz    =val; return;}
  void set_spc3sdphi  (const float val) {spc3sdphi  =val; return;}
  void set_spc3sdz    (const float val) {spc3sdz    =val; return;}
  void set_semcsdphi  (const float val) {semcsdphi  =val; return;}
  void set_semcsdz    (const float val) {semcsdz    =val; return;}
  void set_stofsdphi  (const float val) {stofesdphi  =val; return;}
  void set_stofsdz    (const float val) {stofesdz    =val; return;}
  void set_stofdphi   (const float val) {stofedphi   =val; return;}
  void set_stofdz     (const float val) {stofedz     =val; return;}
  void set_stecsdphi  (const float val) {stecsdphi  =val; return;}
  void set_stecsdalpha(const float val) {stecsdalpha=val; return;}
  void set_m2tof      (const float val) {m2tofe      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}
  void set_isPiTofw   (const float val) {isPiTofw       =val; return;}
  void set_isKTofw    (const float val) {isKTofw        =val; return;}
  void set_isPTofw    (const float val) {isPTofw        =val; return;}
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
  void set_emcid        (const short val) {emcid        = val; return;}
  void set_tecid        (const short val) {tecid        = val; return;}
  void set_ring         (const int val)   {ring         = val; return;}
  void set_sring        (const int val)   {sring        = val; return;}
  void set_pc1dphi      (const float val) {pc1dphi      = val; return;}
  void set_pc1dz        (const float val) {pc1dz        = val; return;}
  void set_pc2dphi      (const float val) {pc2dphi      = val; return;}
  void set_pc2dz        (const float val) {pc2dz        = val; return;}
  void set_pc3dphi      (const float val) {pc3dphi      = val; return;}
  void set_pc3dz        (const float val) {pc3dz        = val; return;}
  void set_emcdphi      (const float val) {emcdphi      = val; return;}
  void set_emcdz        (const float val) {emcdz        = val; return;}
  void set_dep        (const float val) {dep        = val; return;}
  void set_tofdphi      (const float val) {tofedphi      = val; return;}
  void set_tofdz        (const float val) {tofedz        = val; return;}
  void set_tecdphi      (const float val) {tecdphi      = val; return;}
  void set_tecdalpha    (const float val) {tecdalpha    = val; return;}
  void set_tofph1       (const float val) {tofeph1       = val; return;}
  void set_tofph2       (const float val) {tofeph2       = val; return;}
  void set_toftdc1      (const float val) {tofetdc1      = val; return;}
  void set_toftdc2      (const float val) {tofetdc2      = val; return;}
  void set_scross_phi   (const float val) {scross_phi   = val; return;}
  void set_scross_z     (const float val) {scross_z     = val; return;}
  void set_mcid         (const int val)   {mcid   = val; return;}
  void set_dchid        (const int val)   {dchid  = val; return;}
  void set_sdchid        (const int val)   {sdchid  = val; return;}
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
  float get_spemcx      () const  { return  spemcx       ;}
  float get_spemcy      () const  { return  spemcy       ;}
  float get_spemcz      () const  { return  spemcz       ;}
  float get_ptofx      () const  {return  get_ptofex()       ;}
  float get_ptofy      () const  {return  get_ptofey()       ;}
  float get_ptofz      () const  {return  get_ptofez()       ;}
  float get_ptofex      () const  {return  ptofex       ;}
  float get_ptofey      () const  {return  ptofey       ;}
  float get_ptofez      () const  {return  ptofez       ;}
  float get_sptofex      () const  {return  sptofex       ;}
  float get_sptofey      () const  {return  sptofey       ;}
  float get_sptofez      () const  {return  sptofez       ;}
  float get_plemc      () const  { return  plemc       ;}
  short get_sect       () const  { return  sect        ;}
  short get_ysect      () const  { return  ysect       ;}
  short get_zsect      () const  { return  zsect       ;}
  float get_ecore      () const  { return  ecore       ;}
  float get_dep      () const  { return  dep       ;}
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
  float get_semcdispy  () const  { return  semcdispy   ;}
  float get_semcdispz  () const  { return  semcdispz   ;}
  float get_stemc      () const  { return  stemc       ;}
  float get_sprob      () const  { return  sprob       ;}
  short get_stwrhit    () const  { return  stwrhit     ;}
  float get_semcchi2   () const  { return  semcchi2    ;}
  int   get_slat       () const  { return  slat        ;}
  float get_ttof       () const  { return  ttofe        ;}
  float get_etof       () const  { return  etofe        ;}
  float get_sttof      () const  { return  sttofe       ;}
  float get_setof      () const  { return  setofe       ;}
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
  float get_sppc1x      () const  { return  sppc1x       ;}
  float get_sppc1y      () const  { return  sppc1y       ;}
  float get_sppc1z      () const  { return  sppc1z       ;}
  float get_sppc2x      () const  { return  sppc2x       ;}
  float get_sppc2y      () const  { return  sppc2y       ;}
  float get_sppc2z      () const  { return  sppc2z       ;}
  float get_sppc3x      () const  { return  sppc3x       ;}
  float get_sppc3y      () const  { return  sppc3y       ;}
  float get_sppc3z      () const  { return  sppc3z       ;}
  float get_pc2sdphi   () const  { return  pc2sdphi    ;}
  float get_pc2sdz     () const  { return  pc2sdz      ;}
  float get_pc3sdphi   () const  { return  pc3sdphi    ;}
  float get_pc3sdz     () const  { return  pc3sdz      ;}
  float get_emcsdphi   () const  { return  emcsdphi    ;}
  float get_emcsdz     () const  { return  emcsdz      ;}
  float get_tofsdphi   () const  { return  tofesdphi    ;}
  float get_tofsdz     () const  { return  tofesdz      ;}
  float get_tecsdphi   () const  { return  tecsdphi    ;}
  float get_tecsdalpha () const  { return  tecsdalpha  ;}
  float get_spc2sdphi  () const  { return  spc2sdphi   ;}
  float get_spc2sdz    () const  { return  spc2sdz     ;}
  float get_spc3sdphi  () const  { return  spc3sdphi   ;}
  float get_spc3sdz    () const  { return  spc3sdz     ;}
  float get_semcsdphi  () const  { return  semcsdphi   ;}
  float get_semcsdz    () const  { return  semcsdz     ;}
  float get_stofsdphi  () const  { return  stofesdphi   ;}
  float get_stofsdz    () const  { return  stofesdz     ;}
  float get_stofdphi   () const  { return  stofedphi    ;}
  float get_stofdz     () const  { return  stofedz      ;}
  float get_stecsdphi  () const  { return  stecsdphi   ;}
  float get_stecsdalpha() const  { return  stecsdalpha  ;}
  float get_m2tof      () const  { return  m2tofe       ;}
  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}
  float get_isPiTofw   () const  { return  isPiTofw    ;}
  float get_isKTofw    () const  { return  isKTofw     ;}
  float get_isPTofw    () const  { return  isPTofw     ;}
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

  // PC hit ids
  short get_pc1id       () const { return pc1id ;}
  short get_pc2id       () const { return pc2id ;}
  short get_pc3id       () const { return pc3id ;}
  void set_pc1id        (const short val) {pc1id        = val; return;}
  void set_pc2id        (const short val) {pc2id        = val; return;}
  void set_pc3id        (const short val) {pc3id        = val; return;}
 // slipped PC stuff
  short get_spc1id       () const { return spc1id ;}
  short get_spc2id       () const { return spc2id ;}
  short get_spc3id       () const { return spc3id ;}
  void set_spc1id        (const short val) {spc1id        = val; return;}
  void set_spc2id        (const short val) {spc2id        = val; return;}
  void set_spc3id        (const short val) {spc3id        = val; return;}


  // TOF East
  void set_tofid        (const short val) {set_tofeid(val);}
  void set_tofeid        (const short val) {tofeid        = val;}
  short get_tofid       () const { return get_tofeid();}
  short get_tofeid       () const { return tofeid ;}
  // slipped TOF East
  void set_stofeid        (const short val) {stofeid        = val;}
  short get_stofeid       () const { return stofeid ;}


  short get_emcid       () const { return emcid ;}
  short get_tecid       () const { return tecid ;}

  // slipped emc
  short get_semcid       () const { return semcid ;}
  void set_semcid(const short int val) { semcid = val;}

  short get_stecid       () const { return stecid ;}
  void set_stecid(const short int val) { stecid = val;}

  int get_ring          () const { return ring  ;}
  int get_sring         () const { return sring  ;}
  float get_pc1dphi     () const { return pc1dphi       ;}
  float get_pc1dz       () const { return pc1dz ;}
  float get_pc2dphi     () const { return pc2dphi       ;}
  float get_pc2dz       () const { return pc2dz ;}
  float get_pc3dphi     () const { return pc3dphi       ;}
  float get_pc3dz       () const { return pc3dz ;}
  float get_emcdphi     () const { return emcdphi       ;}
  float get_emcdz       () const { return emcdz ;}
  float get_tofdphi     () const { return tofedphi;}
  float get_tofdz       () const { return tofedz ;}
  float get_tecdphi     () const { return tecdphi       ;}
  float get_tecdalpha   () const { return tecdalpha     ;}
  float get_tofph1      () const { return tofeph1  ;}
  float get_tofph2      () const { return tofeph2  ;}
  float get_toftdc1     () const { return tofetdc1 ;}
  float get_toftdc2     () const { return tofetdc2 ;}
  float get_scross_phi  () const { return scross_phi    ;}
  float get_scross_z    () const { return scross_z      ;}
  int   get_mcid        () const { return mcid  ;}
  int   get_dchid       () const { return dchid ;}
  int   get_sdchid       () const { return sdchid ;}
  int   get_emcrawtdc   () const { return emcrawtdc     ;}
  int   get_emcrawadc   () const { return emcrawadc     ;}
  int   get_emcrawadclg () const { return emcrawadclg   ;}
  float get_se9         () const { return se9   ;}
  float get_secent      () const { return secent        ;}
  int   get_aerindex    () const { return aerindex        ;}
  int   get_aersindex   () const { return aersindex        ;}


  // TOF West
  void set_tofwid     (const short int val) {tofwid = val;}
  short get_tofwid     () const {return  tofwid;}

  // slipped TOF West
  void set_stofwid     (const short int val) {stofwid = val;}
  short get_stofwid     () const {return  stofwid;}

  void set_striptofw  (const int   val) {striptofw       = val; return;}
  void set_ttofw      (const float val) {ttofw      = val; return;}  
  void set_qtofw      (const float val) {etofw      = val; return;}
  void set_tofwtdcup  (const float val) {tofwtdcup   = val; return;}
  void set_tofwtdcdw  (const float val) {tofwtdcdw  = val; return;}
  void set_tofwadcup  (const float val) {tofwadcup  = val; return;}
  void set_tofwadcdw  (const float val) {tofwadcdw  = val; return;}

  void set_ptofwx     (const float val) {ptofwx     = val; return;}
  void set_ptofwy     (const float val) {ptofwy     = val; return;}
  void set_ptofwz     (const float val) {ptofwz     = val; return;}
  void set_sptofwx     (const float val) {sptofwx     = val; return;}
  void set_sptofwy     (const float val) {sptofwy     = val; return;}
  void set_sptofwz     (const float val) {sptofwz     = val; return;}
  void set_pltof     (const float val) {pltofe     = val; return;}
  void set_pltofe     (const float val) {pltofe     = val; return;}
  void set_pltofw     (const float val) {pltofw     = val; return;}
 
  void set_tofwdphi   (const float val) {tofwdphi   = val; return;}
  void set_tofwdz     (const float val) {tofwdz     = val; return;}
  void set_tofwsdphi  (const float val) {tofwsdphi  = val; return;}
  void set_tofwsdz    (const float val) {tofwsdz    = val; return;}

  void set_m2tofw     (const float val) {m2tofw     = val; return;}


  int   get_striptofw  () const { return striptofw        ;}
  float get_ttofw      () const { return  ttofw       ;}
  float get_qtofw      () const { return  etofw       ;}
  float get_tofwtdcup  () const { return tofwtdcup ;}
  float get_tofwtdcdw  () const { return tofwtdcdw;}
  float get_tofwadcup  () const { return tofwadcup;}
  float get_tofwadcdw  () const { return tofwadcdw;}

  float get_ptofwx     () const { return ptofwx       ;}
  float get_ptofwy     () const { return ptofwy       ;}
  float get_ptofwz     () const { return ptofwz       ;}
  float get_sptofwx     () const { return sptofwx       ;}
  float get_sptofwy     () const { return sptofwy       ;}
  float get_sptofwz     () const { return sptofwz       ;}
  float get_pltof     () const {return get_pltofe();}
  float get_pltofe     () const {return pltofe;}
  float get_pltofw     () const {return pltofw;}

  float get_tofwdphi   () const { return tofwdphi     ;}
  float get_tofwdz     () const { return tofwdz       ;}
  float get_tofwsdphi  () const { return tofwsdphi    ;}
  float get_tofwsdz    () const { return tofwsdz      ;}

  float get_m2tofw     () const { return m2tofw       ;}

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
  

  void set_hbdlocalmax(const float val) {hbdlocalmax     = val; return;}
  void set_hbdscharge (const float val) {hbdscharge  = val; return;}


  void set_hbdhubcharge     (const float val) {hbdhubcharge  = val; return;}	 
  void set_hbdspokecharge1  (const float val) {hbdspokecharge1  = val; return;}	 
  void set_hbdspokecharge2  (const float val) {hbdspokecharge2  = val; return;}

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

  float get_hbdscharge() const { return hbdscharge    ;}
  float get_hbdlocalmax() const {return hbdlocalmax   ;}

  float get_hbdhubcharge      () const { return hbdhubcharge        ;}	 
  float get_hbdspokecharge1   () const { return hbdspokecharge1      ;}	 
  float get_hbdspokecharge2   () const { return hbdspokecharge2        ;}

  ///////////////////////
  // Methods new on v22, swapping values

  void set_spc1dphi  (const float val) {spc1dphi  =val; return;}
  void set_spc1dz    (const float val) {spc1dz    =val; return;}
  void set_spc2dphi  (const float val) {spc2dphi  =val; return;}
  void set_spc2dz    (const float val) {spc2dz    =val; return;}
  void set_spc3dphi  (const float val) {spc3dphi  =val; return;}
  void set_spc3dz    (const float val) {spc3dz    =val; return;}
  void set_semcdphi  (const float val) {semcdphi  =val; return;}
  void set_semcdz    (const float val) {semcdz    =val; return;}

  float get_spc1dphi  () const  { return  spc1dphi   ;}
  float get_spc1dz    () const  { return  spc1dz     ;}
  float get_spc2dphi  () const  { return  spc2dphi   ;}
  float get_spc2dz    () const  { return  spc2dz     ;}
  float get_spc3dphi  () const  { return  spc3dphi   ;}
  float get_spc3dz    () const  { return  spc3dz     ;}
  float get_semcdphi  () const  { return  semcdphi   ;}
  float get_semcdz    () const  { return  semcdz     ;}

  void set_stofwdphi   (const float val) {stofwdphi   = val; return;}
  void set_stofwdz     (const float val) {stofwdz     = val; return;}
  void set_stofwsdphi  (const float val) {stofwsdphi  = val; return;}
  void set_stofwsdz    (const float val) {stofwsdz    = val; return;}

  float get_stofwdphi   () const { return stofwdphi     ;}
  float get_stofwdz     () const { return stofwdz       ;}
  float get_stofwsdphi  () const { return stofwsdphi    ;}
  float get_stofwsdz    () const { return stofwsdz      ;}

  void set_teccharge      (const unsigned int iplane,  const float val) { teccharge[iplane] = val; return;}
  void set_tecntimebins   (const unsigned int iplane,  const int val) { tecntimebins[iplane] = val; return;}
  void set_tecavgtimebin  (const unsigned int iplane,  const int val) { tecavgtimebin[iplane] = val; return;}
  void set_tecdphiplane   (const unsigned int iplane,  const float val) { tecdphiplane[iplane] = val; return;}
  void set_tecMomentum    (const float val)                    { tecMomentum = val; return;}
  void set_tectrlike      (const float val)                    { tectrlike = val; return;}

  void set_steccharge      (const unsigned int iplane,  const float val) { steccharge[iplane] = val; return;}
  void set_stecntimebins   (const unsigned int iplane,  const int val) { stecntimebins[iplane] = val; return;}
  void set_stecavgtimebin  (const unsigned int iplane,  const int val) { stecavgtimebin[iplane] = val; return;}
  void set_stecdphiplane   (const unsigned int iplane,  const float val) { stecdphiplane[iplane] = val; return;}
  void set_stecMomentum    (const float val)                    { stecMomentum = val; return;}
  void set_stectrlike      (const float val)                    { stectrlike = val; return;}

  float get_teccharge     (const unsigned int iplane) const { return teccharge[iplane];}
  int get_tecntimebins    (const unsigned int iplane) const { return tecntimebins[iplane];}
  int get_tecavgtimebin   (const unsigned int iplane) const { return tecavgtimebin[iplane];}
  float get_tecdphiplane  (const unsigned int iplane) const { return tecdphiplane[iplane];}
  float get_tecMomentum   ()                 const { return tecMomentum;}
  float get_tectrlike     ()                 const { return tectrlike;}

  float get_steccharge     (const unsigned int iplane) const { return steccharge[iplane];}
  int get_stecntimebins    (const unsigned int iplane) const { return stecntimebins[iplane];}
  int get_stecavgtimebin   (const unsigned int iplane) const { return stecavgtimebin[iplane];}
  float get_stecdphiplane  (const unsigned int iplane) const { return stecdphiplane[iplane];}
  float get_stecMomentum   ()                 const { return stecMomentum;}
  float get_stectrlike     ()                 const { return stectrlike;}

  // Silicon Vertex (SVX) stuff
  void set_svxdphi            (const unsigned int ilayer, const float val) { svxdphi[ilayer]=val; return;} 
  void set_svxdz              (const unsigned int ilayer, const float val) { svxdz[ilayer]=val; return;} 
  void set_svxid              (const unsigned int ilayer, const int val)   { svxid[ilayer]=val; return;}
  void set_svxsdphi           (const unsigned int ilayer, const float val) { svxsdphi[ilayer]=val; return;}
  void set_svxsdz             (const unsigned int ilayer, const float val) { svxsdz[ilayer]=val; return;}
  void set_svxsid             (const unsigned int ilayer, const int val)   { svxsid[ilayer]=val; return;}
  void set_psvxx              (const unsigned int ilayer, const float val) { psvxx[ilayer]=val; return;}
  void set_psvxy              (const unsigned int ilayer, const float val) { psvxy[ilayer]=val; return;}
  void set_psvxz              (const unsigned int ilayer, const float val) { psvxz[ilayer]=val; return;}
  void set_spsvxx              (const unsigned int ilayer, const float val) { spsvxx[ilayer]=val; return;}
  void set_spsvxy              (const unsigned int ilayer, const float val) { spsvxy[ilayer]=val; return;}
  void set_spsvxz              (const unsigned int ilayer, const float val) { spsvxz[ilayer]=val; return;}
  // entries above are for svx clusters, entries below ate for svx standalone tracks
  void set_svxtrackid         (const int val)    { svxtrackid=val; return;}
  void set_svxtrackquality           (const float val)  { svxtrackquality=val; return;} // standalone
  void set_svxdca2d           (const float val)  { svxdca2d=val; return;} // standalone
  void set_svxdca3d           (const float val)  { svxdca3d=val; return;}
  void set_svxdca2dkf         (const float val)  { svxdca2dkf=val; return;} // KalFit
  void set_svxdca3dkf         (const float val)  { svxdca3dkf=val; return;}
  // The entries below represent a point of closest approach, and
  // track momentum components ar this point.
  // They are needed for secondary vertex determination for a pair of tracks
  // They come in two flavors: one determined by svx standalone tracking (0)
  // and the second one determined by KalFit (kf)
  void set_svxxyz0            (const unsigned int icoor, const float val)  { svxxyz0[icoor]=val; return; } 
  void set_svxpxyz0           (const unsigned int icoor, const float val)  { svxpxyz0[icoor]=val; return; }
  void set_svxxyzkf           (const unsigned int icoor, const float val)  { svxxyzkf[icoor]=val; return; }
  void set_svxpxyzkf          (const unsigned int icoor, const float val)  { svxpxyzkf[icoor]=val; return; }

  float get_svxdphi           (const unsigned int ilayer) const { return svxdphi[ilayer];} 
  float get_svxdz             (const unsigned int ilayer) const { return svxdz[ilayer];}
  int   get_svxid             (const unsigned int ilayer) const { return svxid[ilayer];}
  float get_svxsdphi          (const unsigned int ilayer) const { return svxsdphi[ilayer];}
  float get_svxsdz            (const unsigned int ilayer) const { return svxsdz[ilayer];}
  int   get_svxsid            (const unsigned int ilayer) const { return svxsid[ilayer];}
  float get_psvxx             (const unsigned int ilayer) const { return psvxx[ilayer];}
  float get_psvxy             (const unsigned int ilayer) const { return psvxy[ilayer];}
  float get_psvxz             (const unsigned int ilayer) const { return psvxz[ilayer];}
  float get_spsvxx             (const unsigned int ilayer) const { return spsvxx[ilayer];}
  float get_spsvxy             (const unsigned int ilayer) const { return spsvxy[ilayer];}
  float get_spsvxz             (const unsigned int ilayer) const { return spsvxz[ilayer];}
  int   get_svxtrackid        () const { return svxtrackid;}
  float get_svxdca2d          () const { return svxdca2d;}
  float get_svxtrackquality          () const { return svxtrackquality;}
  float get_svxdca3d          () const { return svxdca3d;}
  float get_svxdca2dkf        () const { return svxdca2dkf;}
  float get_svxdca3dkf        () const { return svxdca3dkf;}
  float get_svxxyz0           (const unsigned int icoor) const { return svxxyz0[icoor];}
  float get_svxpxyz0          (const unsigned int icoor) const { return svxpxyz0[icoor];}
  float get_svxxyzkf          (const unsigned int icoor) const { return svxxyzkf[icoor];}
  float get_svxpxyzkf         (const unsigned int icoor) const { return svxpxyzkf[icoor];}




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
  float sppc1x      ;
  float sppc1y      ;
  float sppc1z      ;
  float ppc2x      ;
  float ppc2y      ;
  float ppc2z      ;
  float sppc2x      ;
  float sppc2y      ;
  float sppc2z      ;
  float ptecx      ;
  float ptecy      ;
  float ptecz      ;
  float ppc3x      ;
  float ppc3y      ;
  float ppc3z      ;
  float sppc3x      ;
  float sppc3y      ;
  float sppc3z      ;
  float pemcx      ;
  float pemcy      ;
  float pemcz      ;
  float spemcx      ;
  float spemcy      ;
  float spemcz      ;
  float ptofex      ;
  float ptofey      ;
  float ptofez      ;
  float sptofex      ;
  float sptofey      ;
  float sptofez      ;
  float ptofwx      ;
  float ptofwy      ;
  float ptofwz      ;
  float sptofwx      ;
  float sptofwy      ;
  float sptofwz      ;
  float pltofe     ;
  float pltofw     ;
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
  float semcdispy  ;
  float semcdispz  ;
  float stemc      ;
  float sprob      ;
  short stwrhit    ;
  float semcchi2   ;
  int   slat       ;
  int striptofw;
  float ttofe       ;
  float etofe       ;
  float sttofe      ;
  float setofe      ;
  float ttofw       ;
  float etofw       ;
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
  float tofesdphi   ;
  float tofesdz     ;
  float tofwsdphi   ;
  float tofwsdz     ;
  float tecsdphi   ;
  float tecsdalpha ;
  float spc2sdphi  ;
  float spc2sdz    ;
  float spc3sdphi  ;
  float spc3sdz    ;
  float semcsdphi  ;
  float semcsdz    ;
  float stofesdphi  ;
  float stofesdz    ;
  float stofwsdphi  ;
  float stofwsdz    ;
  float stecsdphi  ;
  float stecsdalpha;
  float m2tofe      ;
  float m2tofw      ;
  float m2emc      ;
  float isPi       ;
  float isK        ;
  float isP        ;
  float isPiTofw   ;
  float isKTofw    ;
  float isPTofw    ;
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
  // PC stuff
  short pc1id   ;
  short pc2id   ;
  short pc3id   ;
  // slipped PC stuff
  short spc1id   ;
  short spc2id   ;
  short spc3id   ;

  // EMC
  short emcid   ;
  // slipped EMC
  short semcid   ;

  // TOF East stuff
  short tofeid   ;
  // slipped TOF East stuff
  short stofeid   ;


  // TOF West stuff
  short tofwid;
  // slipped TOF West stuff
  short stofwid;

  short tecid   ;
  int ring      ;
  int sring      ;
  float pc1dphi ;
  float pc1dz   ;
  float pc2dphi ;
  float pc2dz   ;
  float pc3dphi ;
  float pc3dz   ;
  float emcdphi ;
  float emcdz   ;
  float tofedphi ;
  float tofedz   ;
  float tofwdphi ;
  float tofwdz   ;
  float tecdphi ;
  float tecdalpha;
  float tofeph1  ;
  float tofeph2  ;
  float tofetdc1  ;
  float tofetdc2  ;
  float tofwtdcup;
  float tofwtdcdw;
  float tofwadcup;
  float tofwadcdw;
  float scross_phi      ;
  float scross_z        ;
  int   mcid      ;
  int   dchid     ;
  int   sdchid     ;
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

  float hbdhubcharge;
  float hbdspokecharge1;
  float hbdspokecharge2;

   float hbdlocalmax;
   float hbdscharge;

  ///////////////////////
  // Members new on v22, swapping values

  float spc1dphi  ;
  float spc1dz    ;
  float spc2dphi  ;
  float spc2dz    ;
  float spc3dphi  ;
  float spc3dz    ;
  float semcdphi  ;
  float semcdz    ;
  float stofedphi ;
  float stofedz   ;
  float stofwdphi ;
  float stofwdz   ;

  float teccharge[6];
  int tecntimebins[6];
  int tecavgtimebin[6];
  float tecdphiplane[6];
  float tecMomentum;
  float tectrlike;

  float steccharge[6];
  int stecntimebins[6];
  int stecavgtimebin[6];
  float stecdphiplane[6];
  float stecMomentum;
  float stectrlike;

  // Silicon Vetrex (SVX) stuff
  float svxdphi[4];
  float svxdz[4];
  int   svxid[4];
  float svxsdphi[4];
  float svxsdz[4];
  int   svxsid[4];
  float psvxx[4];
  float psvxy[4];
  float psvxz[4];
  float spsvxx[4];
  float spsvxy[4];
  float spsvxz[4];
  int   svxtrackid;
  float svxtrackquality;
  float svxdca2d;
  float svxdca3d;
  float svxdca2dkf;
  float svxdca3dkf;
  float svxxyz0[3] ;
  float svxpxyz0[3];
  float svxxyzkf[3];
  float svxpxyzkf[3];
  float dep;

  ClassDef(PHSnglCentralTrackv24,1)
};

#endif /* PHHSNGLCENTRALTRACKV24 */

