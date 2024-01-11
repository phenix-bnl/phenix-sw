#ifndef __HBDRAW_HH_
#define __HBDRAW_HH_

#include <iostream>
#include <PHObject.h>
#include <phool.h>
#include <PHPoint.h>

// **************************************************************
//
// Implementation of TPC reconstructed clusters
//
// Created on 9/3/03 by Jeffery Mitchell.
//
// **************************************************************

class HbdRaw : public PHObject
{

 public:
  virtual ~HbdRaw() {}

  // Set the values in the HbdRaw ...

  virtual void set_padid (const int val)  {warning("padid        ");}
  virtual void set_clock  (const int sample, const short val) {warning("clock      ");}
  virtual void set_rawadc (const int sample, const short val) {warning("rawadc     ");}
  virtual void set_charge (const int sample, const float val) {warning("charge     ");}

  // Get the values from the HbdRaw ...
  virtual int get_padid () const {warning("padid    "); return -9999;}
  virtual short get_clock  (const int sample) const {warning("clock   "); return -9999;}
  virtual short get_rawadc (const int sample) const {warning("rawadc  "); return -9999;}
  virtual float get_charge (const int sample) const {warning("charge  "); return -9999.;}

  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual HbdRaw object" << std::endl;
    return;
  }

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "HbdRaw Offending field == " << field << std::endl;
  }

  ClassDef(HbdRaw,1)

};

#endif /* __HBDRAW_HH_ */
