#ifndef __CGLSNGLTRACKV9_H
#define __CGLSNGLTRACKV9_H

#include "CglSnglTrackv8.h"

#define TECPLANES 6

// v9 adds tofw.  Inherit from v7
class CglSnglTrackv9 : public CglSnglTrackv8
{
 public:
  CglSnglTrackv9();
  virtual ~CglSnglTrackv9() {}
  
  int get_tecplaneid(short i) const {return tecplaneid[i];}
  void set_tecplaneid(short i, const int ival) {tecplaneid[i] = ival; return;}

 protected:
  int tecplaneid[TECPLANES];

  ClassDef(CglSnglTrackv9,1)
};

#endif /* __CGLSNGLTRACKV9_H */
