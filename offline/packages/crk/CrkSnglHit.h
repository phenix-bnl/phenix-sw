#ifndef __CRKSNGLHIT_H__
#define __CRKSNGLHIT_H__

#include "PHObject.h"

class CrkSnglHit : public PHObject
{
 public:

  virtual ~CrkSnglHit() {}

  virtual short get_pmt() const {return 0;}
  virtual void set_pmt(const short ival) {return;}

  virtual float get_npe() const {return 0.;}
  virtual void set_npe(const float rval) {return;}

  virtual float get_time() const {return 0.;}
  virtual void set_time(const float rval) {return;}

   ClassDef(CrkSnglHit,1)
};

#endif /* __CRKSNGLHIT_H__ */
