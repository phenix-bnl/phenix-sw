#ifndef __TECSNGLPROJV1_H
#define __TECSNGLPROJV1_H

#include "PHObject.h"

class TecSnglProjv1 : public TObject
{
 public:
  TecSnglProjv1();  
  virtual ~TecSnglProjv1() {}

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

  short get_cgltrackid() const {return cgltrkid;}
  void set_cgltrackid(const short itrk) {cgltrkid = itrk; return;}

  int get_tecclusterid(const unsigned iplane) {return associated[iplane];} 
  void set_tecclusterid(const unsigned iplane, const int tecclusterid) {associated[iplane] = tecclusterid; return;}

 protected:
  float pstartx;
  float pstarty;
  float pstartz;
  float pendx;
  float pendy;
  float pendz;
  short cgltrkid;
  int associated[6];

  ClassDef(TecSnglProjv1,1)
};

#endif /* __TECSNGLPROJV1_H */











