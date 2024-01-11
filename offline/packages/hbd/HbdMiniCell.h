#ifndef __HBDMINICELL_HH_
#define __HBDMINICELL_HH_

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

class HbdMiniCell : public PHObject
{

 public:
  virtual ~HbdMiniCell() {}

  // Set the values in the HbdMiniCell ...
  virtual void set_adcch (const short val)   {warning("adcch        ");}
  virtual void set_charge (const short val)   {warning("charge      ");}

  // Get the values from the HbdMiniCell ...
  virtual short get_adcch () const {warning("adcch    "); return -9999;}
  virtual short get_charge () const {warning("charge  "); return -9999;}
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual HbdMiniCell object" << std::endl;
    return;
  }

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "HbdMiniCell Offending field == " << field << std::endl;
  }

  ClassDef(HbdMiniCell,1)

};

#endif /* __HBDMINICELL_HH_ */
