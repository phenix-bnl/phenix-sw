#ifndef __CRKSNGLPROJV1_H
#define __CRKSNGLPROJV1_H

#include "PHObject.h"

class CrkSnglProjv1 : public TObject
{
 public:
  CrkSnglProjv1();  
  virtual ~CrkSnglProjv1() {}

  float get_pstartx() const {return pstartx;}
  void set_pstartx(const float ival) {pstartx = ival; return;}

  float get_pstarty() const {return pstarty;}
  void set_pstarty(const float ival) {pstarty = ival; return;}

  float get_pstartz() const {return pstartz;}
  void set_pstartz(const float ival) {pstartz = ival; return;}

  float get_pendx() const {return pendx;}
  void set_pendx(const float ival) {pendx = ival; return;}

  float get_pendy() const {return pendy;}
  void set_pendy(const float ival) {pendy = ival; return;}

  float get_pendz() const {return pendz;}
  void set_pendz(const float ival) {pendz = ival; return;}

  short get_ringid() const {return ringid;}
  void set_ringid(const short rval) {ringid = rval; return;}

  short get_cgltrackid() const {return cgltrkid;}
  void set_cgltrackid(const short itrk) {cgltrkid = itrk; return;}

 protected:
  float pstartx;
  float pstarty;
  float pstartz;
  float pendx;
  float pendy;
  float pendz;
  short ringid;
  short cgltrkid;
  
  ClassDef(CrkSnglProjv1,1)
};

#endif /* __CRKSNGLPROJV1_H */











