#ifndef __CGLSNGLTRACKV7_H
#define __CGLSNGLTRACKV7_H

#include "CglSnglTrackv4.h"

// v7 adds hbd.  Inherit from v4, the base detector with mrpc (v5) and svx (v6).
class CglSnglTrackv7 : public CglSnglTrackv4
{
 public:
  CglSnglTrackv7();
  virtual ~CglSnglTrackv7() {}

  short get_hbdblobid() const {return hbdblobid;}
  void set_hbdblobid(const short ival) {hbdblobid = ival; return;}

 protected:
  short hbdblobid;

  ClassDef(CglSnglTrackv7,1)
};

#endif /* __CGLSNGLTRACKV7_H */
