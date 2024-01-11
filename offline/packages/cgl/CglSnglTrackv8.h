#ifndef __CGLSNGLTRACKV8_H
#define __CGLSNGLTRACKV8_H

#include "CglSnglTrackv7.h"

// v8 adds tofw.  Inherit from v7
class CglSnglTrackv8 : public CglSnglTrackv7
{
 public:
  CglSnglTrackv8();
  virtual ~CglSnglTrackv8() {}
  
  short get_tofwrecid() const {return tofwrecid;}
  void set_tofwrecid(const short ival) {tofwrecid = ival; return;}

 protected:
  short tofwrecid;

  ClassDef(CglSnglTrackv8,1)
};

#endif /* __CGLSNGLTRACKV8_H */
