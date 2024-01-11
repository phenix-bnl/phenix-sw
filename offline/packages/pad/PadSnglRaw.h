#ifndef __PADSNGLRAW_H
#define __PADSNGLRAW_H

#include "PHObject.h"

class PadSnglRaw: public PHObject
{
 public:
  PadSnglRaw() {}
  virtual ~PadSnglRaw() {}

  virtual short get_arm() const {return -999;}
  virtual short get_id() const {return -999;}
  virtual short get_padtype() const {return -999;}
  virtual short get_padx() const {return -999;}
  virtual short get_padz() const {return -999;}
  virtual short get_sector() const {return -999;}
  virtual short get_side() const {return -999;}

  virtual void set_arm(const short iarm) {return;}
  virtual void set_id(const short iid) {return;}
  virtual void set_padtype(const short ipadtype) {return;}
  virtual void set_padx(const short ipadx) {return;}
  virtual void set_padz(const short ipadz) {return;}
  virtual void set_sector(const short isec) {return;}
  virtual void set_side(const short isize) {return;}

  ClassDef(PadSnglRaw,1)
};

#endif
