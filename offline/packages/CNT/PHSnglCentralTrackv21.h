#ifndef __PHSNGLCENTRALTRACKV21_H_
#define __PHSNGLCENTRALTRACKV21_H_

#include "PHSnglCentralTrackv19.h"

class PHSnglCentralTrackv21 : public PHSnglCentralTrackv19
{
 public:
  PHSnglCentralTrackv21();
  PHSnglCentralTrackv21(const PHSnglCentralTrack &track);  
  virtual ~PHSnglCentralTrackv21() {}

  void identify(std::ostream &os=std::cout) const;

  // tofw
  void set_tofwid     (const short val) {tofwid     = val; return;}
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

  void set_ptofwx     (const float val) {ptofwx     = val; return;}
  void set_ptofwy     (const float val) {ptofwy     = val; return;}
  void set_ptofwz     (const float val) {ptofwz     = val; return;}
  void set_pltofw     (const float val) {pltofw     = val; return;}
 
  void set_tofwdphi   (const float val) {tofwdphi   = val; return;}
  void set_tofwdz     (const float val) {tofwdz     = val; return;}
  void set_tofwsdphi  (const float val) {tofwsdphi  = val; return;}
  void set_tofwsdz    (const float val) {tofwsdz    = val; return;}



  void set_m2tofw     (const float val) {m2tofw     = val; return;}

  short get_tofwid     () const { return tofwid       ;}
  int   get_striptofw  () const { return  striptofw   ;}
  float get_tofwx      () const { return  tofwx       ;}
  float get_tofwy      () const { return  tofwy       ;}
  float get_tofwz      () const { return  tofwz       ;}
  float get_ttofw      () const { return  ttofw       ;}
  float get_qtofw      () const { return  qtofw       ;}
  float get_tofwtdcup  () const { return  tofwtdcup   ;}
  float get_tofwtdcdw  () const { return  tofwtdcdw   ;}
  float get_tofwadcup  () const { return  tofwadcup   ;}
  float get_tofwadcdw  () const { return  tofwadcdw   ;}

  float get_ptofwx     () const { return ptofwx       ;}
  float get_ptofwy     () const { return ptofwy       ;}
  float get_ptofwz     () const { return ptofwz       ;}
  float get_pltofw     () const { return pltofw       ;}

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

 protected:

  // tofw
  short tofwid;
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

  float ptofwx;
  float ptofwy;
  float ptofwz;
  float pltofw;

  float tofwdphi;
  float tofwdz;
  float tofwsdphi;
  float tofwsdz;

  float m2tofw;

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

  ClassDef(PHSnglCentralTrackv21,1)
};

#endif /* PHHSNGLCENTRALTRACKV21 */

