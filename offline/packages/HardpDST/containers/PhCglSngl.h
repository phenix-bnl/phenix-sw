#ifndef _PhCglSngl_h_
#define _PhCglSngl_h_

#include "PHObject.h"
#include "PHSnglCentralTrack.h"
#include "phool.h"
#include <iostream>

class PhCglSngl : public PHSnglCentralTrack
{
 public:
  PhCglSngl();
  PhCglSngl(const PhCglSngl &track);  
  virtual ~PhCglSngl() {}

  // Here are the very explicit set routines...
  void set_charge     (const short val) {charge     =val; return;}
  void set_quality    (const short val) {quality    =val; return;}
  void set_zed        (const float val) {zed        =val; return;}
  void set_phi        (const float val) {phi        =val; return;}
  void set_alpha      (const float val) {alpha      =val; return;}
  void set_phi0       (const float val) {phi0       =val; return;}
  void set_the0       (const float val) {the0       =val; return;}
  void set_mom        (const float val) {mom        =val; return;}
  void set_ppc1x      (const float val) {ppc1x      =val; return;}
  void set_ppc1y      (const float val) {ppc1y      =val; return;}
  void set_ppc1z      (const float val) {ppc1z      =val; return;}
  void set_ppc3x      (const float val) {ppc3x      =val; return;}
  void set_ppc3y      (const float val) {ppc3y      =val; return;}
  void set_ppc3z      (const float val) {ppc3z      =val; return;}
  void set_pltof      (const float val) {pltof      =val; return;}
  void set_plemc      (const float val) {plemc      =val; return;}
  void set_sect       (const short val) {sect       =val; return;}
  void set_ysect      (const short val) {ysect      =val; return;}
  void set_zsect      (const short val) {zsect      =val; return;}
  void set_emce       (const float val) {emce       =val; return;}
  void set_temc       (const float val) {temc       =val; return;}
  void set_emcrawtdc  (const int   val) {emcrawtdc  =val; return;}
  void set_emcrawadc  (const int   val) {emcrawadc  =val; return;}
  void set_emcrawadclg (const int   val) {emcrawadclg  =val; return;}
  void set_prob       (const float val) {prob       =val; return;}
  void set_ecent      (const float val) {ecent      =val; return;}
  void set_emcchi2    (const float val) {emcchi2    =val; return;}
  void set_slat       (const int   val) {slat       =val; return;}
  void set_ttof       (const float val) {ttof       =val; return;}
  void set_n0         (const short val) {n0         =val; return;}
  void set_npe0       (const float val) {npe0       =val; return;}
  void set_n1         (const short val) {n1         =val; return;}
  void set_npe1       (const float val) {npe1       =val; return;}
  void set_chi2       (const float val) {chi2       =val; return;}
  void set_disp       (const float val) {disp       =val; return;}
  void set_pc2sdphi   (const float val) {pc2sdphi   =val; return;}
  void set_pc2sdz     (const float val) {pc2sdz     =val; return;}
  void set_pc3sdphi   (const float val) {pc3sdphi   =val; return;}
  void set_pc3sdz     (const float val) {pc3sdz     =val; return;}
  void set_emcsdphi   (const float val) {emcsdphi   =val; return;}
  void set_emcsdz     (const float val) {emcsdz     =val; return;}
  void set_tofsdphi   (const float val) {tofsdphi   =val; return;}
  void set_tofsdz     (const float val) {tofsdz     =val; return;}
  void set_m2tof      (const float val) {m2tof      =val; return;}
  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}
  void set_dcarm      (const short val) {dcarm	    = val; return;}
  void set_deadmap    (const int val)   {deadmap    = val; return;}
  void set_warnmap    (const int val)   {warnmap    = val; return;}

  // Here are the very explicit "get" routines...
  short get_charge     () const  { return  charge      ;}
  short get_quality    () const  { return  quality     ;}
  float get_zed        () const  { return  zed         ;}
  float get_phi        () const  { return  phi         ;}
  float get_alpha      () const  { return  alpha       ;}
  float get_phi0       () const  { return  phi0        ;}
  float get_the0       () const  { return  the0        ;}
  float get_mom        () const  { return  mom         ;}
  float get_ppc1x      () const  { return  ppc1x       ;}
  float get_ppc1y      () const  { return  ppc1y       ;}
  float get_ppc1z      () const  { return  ppc1z       ;}
  float get_ppc3x      () const  { return  ppc3x       ;}
  float get_ppc3y      () const  { return  ppc3y       ;}
  float get_ppc3z      () const  { return  ppc3z       ;}
  float get_pltof      () const  { return  pltof       ;}
  float get_plemc      () const  { return  plemc       ;}
  short get_sect       () const  { return  sect        ;}
  short get_ysect      () const  { return  ysect        ;}
  short get_zsect      () const  { return  zsect        ;}
  float get_emce       () const  { return  emce       ;}
  float get_temc       () const  { return  temc        ;}
  int   get_emcrawtdc  () const  { return  emcrawtdc   ;}
  int   get_emcrawadc  () const  { return  emcrawadc   ;}
  int   get_emcrawadclg  () const  { return  emcrawadclg   ;}
  float get_prob       () const  { return  prob        ;}
  float get_ecent      () const  { return  ecent       ;}
  float get_emcchi2    () const  { return  emcchi2     ;}
  int   get_slat       () const  { return  slat        ;}
  float get_ttof       () const  { return  ttof        ;}
  short get_n0         () const  { return  n0          ;}
  float get_npe0       () const  { return  npe0        ;}
  short get_n1         () const  { return  n1          ;}
  float get_npe1       () const  { return  npe1        ;}
  float get_chi2       () const  { return  chi2        ;}
  float get_disp       () const  { return  disp        ;}
  float get_pc2sdphi   () const  { return  pc2sdphi    ;}
  float get_pc2sdz     () const  { return  pc2sdz      ;}
  float get_pc3sdphi   () const  { return  pc3sdphi    ;}
  float get_pc3sdz     () const  { return  pc3sdz      ;}
  float get_emcsdphi   () const  { return  emcsdphi    ;}
  float get_emcsdz     () const  { return  emcsdz      ;}
  float get_tofsdphi   () const  { return  tofsdphi    ;}
  float get_tofsdz     () const  { return  tofsdz      ;}
  float get_m2tof      () const  { return  m2tof       ;}
  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}
  short get_dcarm	() const { return dcarm	        ;}
  int   get_deadmap	() const { return deadmap	;}
  int   get_warnmap	() const { return warnmap	;}

 protected:
  short charge     ;
  short quality    ;
  float zed        ;
  float phi        ;
  float alpha      ;
  float phi0       ;
  float the0       ;
  float mom        ;
  float ppc1x      ;
  float ppc1y      ;
  float ppc1z      ;
  float ppc3x      ;
  float ppc3y      ;
  float ppc3z      ;
  float pltof      ;
  float plemc      ;
  short sect       ;
  short ysect      ;
  short zsect      ;
  float emce       ;
  float temc       ;
  int   emcrawtdc  ;
  int   emcrawadc  ;
  int   emcrawadclg;
  float prob       ;
  float ecent      ;
  float emcchi2    ;
  int   slat       ;
  float ttof       ;
  short n0         ;
  float npe0       ;
  short n1         ;
  float npe1       ;
  float chi2       ;
  float disp       ;
  float pc2sdphi   ;
  float pc2sdz     ;
  float pc3sdphi   ;
  float pc3sdz     ;
  float emcsdphi   ;
  float emcsdz     ;
  float tofsdphi   ;
  float tofsdz     ;
  float m2tof      ;
  float m2emc      ;
  float isPi       ;
  float isK        ;
  float isP        ;
  short dcarm	;
  int deadmap	;
  int warnmap	;

  ClassDef(PhCglSngl,1)
};

#endif 

