#ifndef __CRKRING_H
#define __CRKRING_H

#include "PHObject.h"
#include <iostream>

class CrkRing : public PHObject
{
 public:
  virtual ~CrkRing() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  virtual unsigned int get_CrkNRing() const {return 0;}
  virtual void set_CrkNRing(const unsigned int nring) {return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddCrkRing(const unsigned int iring) {return;}

  virtual short get_panel(const unsigned int iring) const {warning("panel  "); return -999;}
  virtual void set_panel(const unsigned int iring, const short ival) {warning("panel  "); return;}

  virtual short get_accepted(const unsigned int iring) const {warning("accepted  "); return -999;}
  virtual void set_accepted(const unsigned int iring, const short ival) {warning("accepted  "); return;}

  virtual short get_npmt0(const unsigned int iring) const {warning("npmt0  "); return -999;}
  virtual void set_npmt0(const unsigned int iring, const short ival) {warning("npmt0  "); return;}

  virtual short get_npmt1(const unsigned int iring) const {warning("npmt1  "); return -999;}
  virtual void set_npmt1(const unsigned int iring, const short ival) {warning("npmt1  "); return;}

  virtual short get_npmt2(const unsigned int iring) const {warning("npmt2  "); return -999;}
  virtual void set_npmt2(const unsigned int iring, const short ival) {warning("npmt2  "); return;}

  virtual short get_npmt3(const unsigned int iring) const {warning("npmt3  "); return -999;}
  virtual void set_npmt3(const unsigned int iring, const short ival) {warning("npmt3  "); return;}

  virtual float get_npe0(const unsigned int iring) const {warning("npe0  "); return -999;}
  virtual void set_npe0(const unsigned int iring, const float rval) {warning("npe0  "); return;}

  virtual float get_npe1(const unsigned int iring) const {warning("npe1  "); return -999;}
  virtual void set_npe1(const unsigned int iring, const float rval) {warning("npe1  "); return;}

  virtual float get_npe2(const unsigned int iring) const {warning("npe2  "); return -999;}
  virtual void set_npe2(const unsigned int iring, const float rval) {warning("npe2  "); return;}

  virtual float get_npe3(const unsigned int iring) const {warning("npe3  "); return -999;}
  virtual void set_npe3(const unsigned int iring, const float rval) {warning("npe3  "); return;}

  virtual float get_chi2(const unsigned int iring) const {warning("chi2  "); return -999;}
  virtual void set_chi2(const unsigned int iring, const float rval) {warning("chi2  "); return;}

  virtual float get_disp(const unsigned int iring) const {warning("disp  "); return -999;}
  virtual void set_disp(const unsigned int iring, const float rval) {warning("disp  "); return;}

  virtual float get_cross_phi(const unsigned int iring) const {warning("cross_phi  "); return -999;}
  virtual void set_cross_phi(const unsigned int iring, const float rval) {warning("corss_phi  "); return;}

  virtual float get_cross_z(const unsigned int iring) const {warning("cross_z  "); return -999;}
  virtual void set_cross_z(const unsigned int iring, const float rval) {warning("cross_z  "); return;}

  virtual float get_center_phi(const unsigned int iring) const {warning("center_phi  "); return -999;}
  virtual void set_center_phi(const unsigned int iring, const float rval) {warning("center_phi  "); return;}

  virtual float get_center_z(const unsigned int iring) const {warning("center_z  "); return -999;}
  virtual void set_center_z(const unsigned int iring, const float rval) {warning("center_z  "); return;}

  //  Time missing prior to v3 version.   TKH 5-20-2002.
  virtual float get_tcrk(const unsigned int iring) const {warning("tcrk  "); return -999;}
  virtual void  set_tcrk(const unsigned int iring, const float rval) {warning("tcrk  "); return;}

  virtual short get_cgltrackid(const unsigned int iring) const {warning("tcrk  "); return -999;}  //FM 
  virtual void  set_cgltrackid(const unsigned int iring, const unsigned int itrk) {warning("tcrk  "); return;}  //FM

 private:
  void warning(const char* field) const;

  ClassDef(CrkRing,1)

};

#endif /* __CRKRING_H */
