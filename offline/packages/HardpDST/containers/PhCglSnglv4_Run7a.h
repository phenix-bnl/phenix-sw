#ifndef _PhCglSnglv4_Run7a_h_
#define _PhCglSnglv4_Run7a_h_

#include "PHObject.h"
#include "PHSnglCentralTrack.h"
#include "phool.h"
#include <iostream>

class PhCglSnglv4_Run7a : public PHSnglCentralTrack
{
 public:
  PhCglSnglv4_Run7a();
  PhCglSnglv4_Run7a(const PhCglSnglv4_Run7a &track);  
  virtual ~PhCglSnglv4_Run7a() {}
  void Copy(const PHSnglCentralTrack &src);

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
  void set_ecore      (const float val) {ecore      =val; return;}
  void set_emcchi2    (const float val) {emcchi2    =val; return;}
  void set_twrhit     (const short val) {twrhit     =val; return;}

  void set_n0         (const short val) {n0         =val; return;}
  void set_npe0       (const float val) {npe0       =val; return;}
  void set_n1         (const short val) {n1         =val; return;}
  void set_npe1       (const float val) {npe1       =val; return;}
  void set_chi2       (const float val) {chi2       =val; return;}
  void set_disp       (const float val) {disp       =val; return;}

  /////// add tofw and aerogel ///////////////////
  void set_pltof      (const float val) {if(get_dcarm()==0) pltof     =val; return;}
  void set_pltofw     (const float val) {if(get_dcarm()==1) pltof     =val; return;}
  void set_slat       (const int   val) {if(get_dcarm()==0)slat       =val; return;}
  void set_striptofw  (const int   val) {if(get_dcarm()==1)slat       =val; return;}
  void set_ttof       (const float val) {if(get_dcarm()==0)ttof       =val; return;}
  void set_ttofw      (const float val) {if(get_dcarm()==1)ttof       =val; return;}
  void set_etof       (const float val) {if(get_dcarm()==0)etof       =val; return;}
  void set_qtofw      (const float val) {if(get_dcarm()==1)etof       =val; return;}
  void set_tofdphi    (const float val) {if(get_dcarm()==0)tofdphi    =val; return;}
  void set_tofwdphi   (const float val) {if(get_dcarm()==1)tofdphi    =val; return;}
  void set_tofdz      (const float val) {if(get_dcarm()==0)tofdz      =val; return;}
  void set_tofwdz     (const float val) {if(get_dcarm()==1)tofdz      =val; return;}
  void set_stofdphi   (const float val) {if(get_dcarm()==0)stofdphi    =val; return;}
  void set_stofwdphi  (const float val) {if(get_dcarm()==1)stofdphi    =val; return;}
  void set_stofdz     (const float val) {if(get_dcarm()==0)stofdz      =val; return;}
  void set_stofwdz    (const float val) {if(get_dcarm()==1)stofdz      =val; return;}
  void set_m2tof      (const float val) {if(get_dcarm()==0)m2tof      =val; return;}
  void set_m2tofw     (const float val) {if(get_dcarm()==1)m2tof      =val; return;}
  void set_aerindex   (const int val)   {aerindex   =val; return;}
  void set_aersindex  (const int val)   {aersindex  =val; return;}

  //adding these -man
  void set_pc2dphi   (const float val) {pc2dphi   =val; return;}
  void set_pc2dz     (const float val) {pc2dz     =val; return;}
  void set_pc3dphi   (const float val) {pc3dphi   =val; return;}
  void set_pc3dz     (const float val) {pc3dz     =val; return;}
  void set_emcdphi   (const float val) {emcdphi   =val; return;}
  void set_emcdz     (const float val) {emcdz     =val; return;}

  void set_spc2dphi  (const float val) {spc2dphi  =val; return;}
  void set_spc2dz    (const float val) {spc2dz    =val; return;}
  void set_spc3dphi  (const float val) {spc3dphi  =val; return;}
  void set_spc3dz    (const float val) {spc3dz    =val; return;}
  void set_semcdphi  (const float val) {semcdphi  =val; return;}
  void set_semcdz    (const float val) {semcdz    =val; return;}

  void set_m2emc      (const float val) {m2emc      =val; return;}
  void set_isPi       (const float val) {isPi       =val; return;}
  void set_isK        (const float val) {isK        =val; return;}
  void set_isP        (const float val) {isP        =val; return;}
  void set_dcarm      (const short val) {dcarm	    = val; return;}
  void set_deadmap    (const int val)   {deadmap    = val; return;}
  void set_warnmap    (const int val)   {warnmap    = val; return;}
  void set_emcid      (const short val) {emcid      = val; return;}

  //from HadronpDST
  void set_beta      (const float val)  {beta       = val; return;}
  void set_ppc2x     (const float val)  {ppc2x      = val; return;}
  void set_ppc2y     (const float val)  {ppc2y      = val; return;}
  void set_ppc2z     (const float val)  {ppc2z      = val; return;}
  void set_pemcx     (const float val)  {pemcx      = val; return;}
  void set_pemcy     (const float val)  {pemcy      = val; return;}
  void set_pemcz     (const float val)  {pemcz      = val; return;}
  void set_ptofx     (const float val)   {if(get_dcarm()==0) ptofx      = val; return;}
  void set_ptofwx     (const float val)  {if(get_dcarm()==1) ptofx      = val; return;}
  void set_ptofy     (const float val)   {if(get_dcarm()==0) ptofy      = val; return;}
  void set_ptofwy     (const float val)  {if(get_dcarm()==1) ptofy      = val; return;}
  void set_ptofz     (const float val)   {if(get_dcarm()==0) ptofz      = val; return;}
  void set_ptofwz     (const float val)  {if(get_dcarm()==1) ptofz      = val; return;}
  void set_tofph1    (const float val)   {if(get_dcarm()==0) tofph1     = val; return;}
  void set_tofwadcup    (const float val){if(get_dcarm()==1) tofph1     = val; return;}
  void set_tofph2    (const float val)   {if(get_dcarm()==0) tofph2     = val; return;}
  void set_tofwadcdw    (const float val){if(get_dcarm()==1) tofph2     = val; return;}
  void set_toftdc1   (const float val)   {if(get_dcarm()==0) toftdc1    = val; return;}
  void set_tofwtdcup   (const float val) {if(get_dcarm()==1) toftdc1    = val; return;}
  void set_toftdc2   (const float val)   {if(get_dcarm()==0) toftdc2    = val; return;}
  void set_tofwtdcdw   (const float val) {if(get_dcarm()==1) toftdc2    = val; return;}

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
  float get_plemc      () const  { return  plemc       ;}
  short get_sect       () const  { return  sect        ;}
  short get_ysect      () const  { return  ysect       ;}
  short get_zsect      () const  { return  zsect       ;}
  float get_emce       () const  { return  emce        ;}
  float get_temc       () const  { return  temc        ;}
  int   get_emcrawtdc  () const  { return  emcrawtdc   ;}
  int   get_emcrawadc  () const  { return  emcrawadc   ;}
  int   get_emcrawadclg  () const  { return  emcrawadclg   ;}
  float get_prob       () const  { return  prob        ;}
  float get_ecent      () const  { return  ecent       ;}
  float get_ecore      () const  { return  ecore       ;}
  float get_emcchi2    () const  { return  emcchi2     ;}
  short get_twrhit     () const  { return  twrhit      ;}

  short get_n0         () const  { return  n0          ;}
  float get_npe0       () const  { return  npe0        ;}
  short get_n1         () const  { return  n1          ;}
  float get_npe1       () const  { return  npe1        ;}
  float get_chi2       () const  { return  chi2        ;}
  float get_disp       () const  { return  disp        ;}

  ////// add tofw and aerogel/////////////////////
  float get_pltof      () const  { if(get_dcarm()==0) return  pltof; else return -9999;}
  float get_pltofw     () const  { if(get_dcarm()==1) return  pltof; else return -9999;}
  int   get_slat       () const  { if(get_dcarm()==0) return  slat; else return -9998;}
  int   get_striptofw  () const  { if(get_dcarm()==1) return  slat; else return -9998;}
  float get_ttof       () const  { if(get_dcarm()==0) return  ttof; else return -9999;}
  float get_ttofw      () const  { if(get_dcarm()==1) return  ttof; else return -9999;}
  float get_etof       () const  { if(get_dcarm()==0) return  etof; else return -9999;}
  float get_qtofw      () const  { if(get_dcarm()==1) return  etof; else return -9999;}
  float get_tofdphi    () const  { if(get_dcarm()==0) return  tofdphi; else return -9999;}
  float get_tofwdphi   () const  { if(get_dcarm()==1) return  tofdphi; else return -9999;}
  float get_tofdz      () const  { if(get_dcarm()==0) return  tofdz; else return -9999;}
  float get_tofwdz     () const  { if(get_dcarm()==1) return  tofdz; else return -9999;}
  float get_stofdphi   () const  { if(get_dcarm()==0) return  stofdphi; else return -9999;}
  float get_stofwdphi  () const  { if(get_dcarm()==1) return  stofdphi; else return -9999;}
  float get_stofdz     () const  { if(get_dcarm()==0) return  stofdz; else return -9999;}
  float get_stofwdz    () const  { if(get_dcarm()==1) return  stofdz; else return -9999;}
  float get_m2tof      () const  { if(get_dcarm()==0) return  m2tof; else return -9999;}
  float get_m2tofw     () const  { if(get_dcarm()==1) return  m2tof; else return -9999;}
  int   get_aerindex   () const  { return aerindex     ;}
  int   get_aersindex  () const  { return aersindex    ;}

  //adding these --man
  float get_pc2dphi   () const  { return  pc2dphi    ;}
  float get_pc2dz     () const  { return  pc2dz      ;}
  float get_pc3dphi   () const  { return  pc3dphi    ;}
  float get_pc3dz     () const  { return  pc3dz      ;}
  float get_emcdphi   () const  { return  emcdphi    ;}
  float get_emcdz     () const  { return  emcdz      ;}

  float get_spc2dphi  () const  { return  spc2dphi   ;}
  float get_spc2dz    () const  { return  spc2dz     ;}
  float get_spc3dphi  () const  { return  spc3dphi   ;}
  float get_spc3dz    () const  { return  spc3dz     ;}
  float get_semcdphi  () const  { return  semcdphi   ;}
  float get_semcdz    () const  { return  semcdz     ;}

  float get_m2emc      () const  { return  m2emc       ;}
  float get_isPi       () const  { return  isPi        ;}
  float get_isK        () const  { return  isK         ;}
  float get_isP        () const  { return  isP         ;}
  short get_dcarm	() const { return dcarm	       ;}
  int   get_deadmap	() const { return deadmap      ;}
  int   get_warnmap	() const { return warnmap      ;}
  short get_emcid      () const  { return emcid        ;}

  //from HadronpDST
  float get_beta      ()  const  {return beta        ;}
  float get_ppc2x     ()  const  {return ppc2x       ;}
  float get_ppc2y     ()  const  {return ppc2y       ;}
  float get_ppc2z     ()  const  {return ppc2z       ;}
  float get_pemcx     ()  const  {return pemcx       ;}
  float get_pemcy     ()  const  {return pemcy       ;}
  float get_pemcz     ()  const  {return pemcz       ;}
  float get_ptofx     ()  const  {if(get_dcarm()==0) return ptofx       ;else return -9999;}
  float get_ptofwx    ()  const  {if(get_dcarm()==1) return ptofx       ;else return -9999;}
  float get_ptofy     ()  const  {if(get_dcarm()==0) return ptofy       ;else return -9999;}
  float get_ptofwy    ()  const  {if(get_dcarm()==1) return ptofy       ;else return -9999;}
  float get_ptofz     ()  const  {if(get_dcarm()==0) return ptofz       ;else return -9999;}
  float get_ptofwz    ()  const  {if(get_dcarm()==1) return ptofz       ;else return -9999;}
  float get_tofph1    ()  const  {if(get_dcarm()==0) return tofph1      ;else return -9999;}
  float get_tofwadcup ()  const  {if(get_dcarm()==1) return tofph1      ;else return -9999;}
  float get_tofph2    ()  const  {if(get_dcarm()==0) return tofph2      ;else return -9999;}
  float get_tofwadcdw ()  const  {if(get_dcarm()==1) return tofph2      ;else return -9999;}
  float get_toftdc1   ()  const  {if(get_dcarm()==0) return toftdc1     ;else return -9999;}
  float get_tofwtdcup ()  const  {if(get_dcarm()==1) return toftdc1     ;else return -9999;}
  float get_toftdc2   ()  const  {if(get_dcarm()==0) return toftdc2     ;else return -9999;}
  float get_tofwtdcdw ()  const  {if(get_dcarm()==1) return toftdc2     ;else return -9999;}

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
  float ecore      ;
  float emcchi2    ;
  short twrhit     ;

  short n0         ;
  float npe0       ;
  short n1         ;
  float npe1       ;
  float chi2       ;
  float disp       ;

  //////add tofw and aerogel //////
  float pltof      ;
  int   slat       ;
  float ttof       ;
  float etof       ;
  float tofdphi    ;
  float tofdz      ;
  float stofdphi    ;
  float stofdz      ;
  float m2tof      ;
  int   aerindex   ;
  int   aersindex  ;

  //adding these -man
  float pc2dphi   ;
  float pc2dz     ;
  float pc3dphi   ;
  float pc3dz     ;
  float emcdphi   ;
  float emcdz     ;

  float spc2dphi  ;
  float spc2dz    ;
  float spc3dphi  ;
  float spc3dz    ;
  float semcdphi  ;
  float semcdz    ;

  float m2emc      ;
  float isPi       ;
  float isK        ;
  float isP        ;
  short dcarm	;
  int deadmap	;
  int warnmap	;
  int emcid     ;

  //from HadronpDST
  float beta;
  float ppc2x;
  float ppc2y;
  float ppc2z;
  float pemcx;
  float pemcy;
  float pemcz;
  float ptofx;
  float ptofy;
  float ptofz;
  float tofph1;
  float tofph2;
  float toftdc1;
  float toftdc2;
  
  ClassDef(PhCglSnglv4_Run7a,1)
};

#endif 

