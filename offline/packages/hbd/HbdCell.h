#ifndef __HBDCELL_HH_
#define __HBDCELL_HH_

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

class HbdCell : public PHObject
{

 public:
  virtual ~HbdCell() {}

  // Set the values in the HbdCell ...

  virtual void set_padnum (const int val)   {warning("padnum        ");}
  virtual void set_sector (const int val)  {warning("sector   ");}
  virtual void set_arm (const int val)  {warning("arm   ");}
  virtual void set_side (const int val)  {warning("side   ");}
  virtual void set_secchar (const char* secname)  {warning("secname   ");}
  virtual void set_charge (const float val)   {warning("charge     ");}
  virtual void set_clusterid(const int val)
  {warning("set_clusterid ");}

  // Get the values from the HbdCell ...
  virtual int get_padnum () const {warning("padnum    "); return -9999;}
  virtual int get_sector () const {warning("sector    "); return -9999;}
  virtual int get_arm () const {warning("arm    "); return -9999;}
  virtual int get_side () const {warning("side    "); return -9999;}
  virtual int get_secchar (char* secname) const {warning("secchar    "); return -9999;}
  virtual float get_charge () const {warning("charge  "); return -9999.;}
  virtual int get_clusterid  ()
    const {warning("get_clusterid      "); return -9999;}
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual HbdCell object" << std::endl;
    return;
  }

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "HbdCell Offending field == " << field << std::endl;
  }

  ClassDef(HbdCell,1)

};

#endif /* __HBDCELL_HH_ */
