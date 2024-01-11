#ifndef __CRKPROJ_H
#define __CRKPROJ_H

#include <PHObject.h>
#include <iostream>

class CrkProj : public PHObject
{
 public:
  virtual ~CrkProj() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  virtual unsigned int get_CrkNProj() const {return 0;}
  virtual void set_CrkNProj(const unsigned int nring) {return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddCrkProj(const unsigned int iring) {return;}

  virtual float get_pstartx(const unsigned int iring) const {warning("pstartx  "); return -999;}
  virtual void set_pstartx(const unsigned int iring, const float ival) {warning("pstartx  "); return;}

  virtual float get_pstarty(const unsigned int iring) const {warning("pstarty  "); return -999;}
  virtual void set_pstarty(const unsigned int iring, const float ival) {warning("pstarty  "); return;}

  virtual float get_pstartz(const unsigned int iring) const {warning("pstartz  "); return -999;}
  virtual void set_pstartz(const unsigned int iring, const float ival) {warning("pstartz  "); return;}

  virtual float get_pendx(const unsigned int iring) const {warning("pendx  "); return -999;}
  virtual void set_pendx(const unsigned int iring, const float ival) {warning("pendx  "); return;}

  virtual float get_pendy(const unsigned int iring) const {warning("pendy  "); return -999;}
  virtual void set_pendy(const unsigned int iring, const float ival) {warning("pendy  "); return;}

  virtual float get_pendz(const unsigned int iring) const {warning("pendz  "); return -999;}
  virtual void set_pendz(const unsigned int iring, const float ival) {warning("pendz  "); return;}

  virtual short get_ringid(const unsigned int iring) const {warning("ringid  "); return -999;} 
  virtual void  set_ringid(const unsigned int iring, const unsigned int ival) {warning("ringid  "); return;}

  virtual short get_cgltrackid(const unsigned int iring) const {warning("track  "); return -999;} 
  virtual void  set_cgltrackid(const unsigned int iring, const unsigned int ival) {warning("trackid  "); return;} 

 private:
  void warning(const char* field) const;

  ClassDef(CrkProj,1)

};

#endif /* __CRKPROJ_H */
