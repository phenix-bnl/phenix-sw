
#ifndef __ACCSNGLHIT_H_
#define __ACCSNGLHIT_H_

#include <iostream>
#include "PHObject.h"
#include "phool.h"

class AccSnglHit : public PHObject
{

 public:
  AccSnglHit() {}
  virtual ~AccSnglHit() {}  

  // set the values in the SnglHit
  virtual void set_boxid(const int val)              {warning("boxid");}
  virtual void set_npe(const float val)              {warning("npe");}
  virtual void set_tof(const float val)              {warning("tof");}
  virtual void set_tdiff(const float val)            {warning("tdiff");}
  virtual void set_xyz(const int i, const float val) {warning("xyz");}

  // get the values from the SnglHit
  virtual int get_boxid()            const {warning("boxid");  return -9999;}
  virtual float get_npe()            const {warning("npe");  return -9999;}
  virtual float get_tof()            const {warning("tof"); return -9999;}
  virtual float get_tdiff()          const {warning("tdiff"); return -9999;}
  virtual float get_xyz(const int i) const {warning("xyz");  return -9999;}

 private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout <<"Single ACC HIT Offending field == " << field << std::endl;
  }

  ClassDef(AccSnglHit,1)
};

#endif
