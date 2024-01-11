#ifndef __TOFHIT_H__
#define __TOFHIT_H__

#include "PHObject.h"
#include <iostream>

class TofHit: public PHObject
{
 public:
  TofHit() {}
  virtual ~TofHit() {}

  virtual void identify(std::ostream& os = std::cout) const 
    {
      os << "virtual TofHit object";
      return;
    }

  virtual void set_id(const short ival) {return;}
  virtual short get_id() const {return -9999;}

  virtual void set_panel(const short ival) {return;}
  virtual short get_panel() const {return -9999;}

  virtual void set_sector(const short ival) {return;}
  virtual short get_sector() const {return -9999;}

  virtual void set_side(const short ival) {return;}
  virtual short get_side() const {return -9999;}

  virtual void set_slat(const short ival) {return;}
  virtual short get_slat() const {return -9999;}

  virtual void set_slatid(const short ival) {return;}
  virtual short get_slatid() const {return -9999;}



  virtual void set_qvc(const short ival, const short i) {return;}
  virtual short get_qvc(const short i) const {return -9999;}

  virtual void set_tvc(const short ival, const short i) {return;}
  virtual short get_tvc(const short i) const {return -9999;}


  virtual void set_eloss(const float rval) {return;}
  virtual float get_eloss() const;

  virtual void set_eloss_err(const float rval) {return;}
  virtual float get_eloss_err() const;

  virtual void set_tof(const float rval) {return;}
  virtual float get_tof() const;

  virtual void set_tof_err(const float rval) {return;}
  virtual float get_tof_err() const;

  virtual void set_xtof(const float rval, const short ival) {return;}
  virtual float get_xtof(const short ival) const;

  virtual void set_xtof_err(const float rval, const short ival) {return;}
  virtual float get_xtof_err(const short ival) const;

  virtual void set_tdiff(const float rval) {return;}
  virtual float get_tdiff() const;

 protected:
  void virtual_warning(const char *funcname) const;

 private:

  ClassDef(TofHit,1)
};

#endif /* __TOFHIT_H */

