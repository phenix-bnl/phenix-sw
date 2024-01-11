#ifndef __TOFHITV1_H
#define __TOFHITV1_H

#include "PHObject.h"

class TofHitv1: public TObject
{
 public:
  TofHitv1();
  virtual ~TofHitv1() {}

  void set_id(const short ival) {id = ival; return;}
  short get_id() const {return id;}

  void set_panel(const short ival) {panel = ival; return;}
  short get_panel() const {return panel;}

  void set_sector(const short ival) {sector = ival; return;}
  short get_sector() const {return sector;}

  void set_side(const short ival) {side = ival; return;}
  short get_side() const {return side;}

  void set_slat(const short ival) {slat = ival; return;}
  short get_slat() const {return slat;}

  void set_slatid(const short ival) {slatid = ival; return;}
  short get_slatid() const {return slatid;}



  void set_qvc(const short ival, const short i) {qvc[i] = ival; return;}
  short get_qvc(const short i) const {return qvc[i];}

  void set_tvc(const short ival, const short i) {tvc[i] = ival; return;}
  short get_tvc(const short i) const {return tvc[i];}


  void set_eloss(const float rval) {eloss = rval; return;}
  float get_eloss() const {return eloss;}

  void set_eloss_err(const float rval) {eloss_err = rval; return;}
  float get_eloss_err() const {return eloss_err;}

  void set_tof(const float rval) {tof = rval; return;}
  float get_tof() const {return tof;}

  void set_tof_err(const float rval) {tof_err = rval; return;}
  float get_tof_err() const {return tof_err;}

  void set_xtof(const float rval, const short ival) {xtof[ival] = rval; return;}
  float get_xtof(const short ival) const {return xtof[ival];}

  void set_xtof_err(const float rval, const short ival) {xtof_err[ival] = rval; return;}
  float get_xtof_err(const short ival) const {return xtof_err[ival];}

 protected:
  short id;
  short panel;
  short sector;
  short side;
  short slat;
  short slatid;

  short qvc[2];
  short tvc[2];

  float eloss;
  float eloss_err;
  float tof;
  float tof_err;

  float xtof[3];
  float xtof_err[3];

  ClassDef(TofHitv1,1)
};

#endif /* __TOFHITV1_H */

