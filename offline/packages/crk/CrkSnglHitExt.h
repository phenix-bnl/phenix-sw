#ifndef __CRKSNGLHITEXT_H__
#define __CRKSNGLHITEXT_H__

#include "PHObject.h"

class CrkSnglHitExt : public PHObject
{
 public:

  virtual ~CrkSnglHitExt() {}

  virtual short get_pmt() const {return 0;}
  virtual void set_pmt(const short ival) {return;}

  virtual float get_npe() const {return 0.;}
  virtual void set_npe(const float rval) {return;}

  virtual float get_time() const {return 0.;}
  virtual void set_time(const float rval) {return;}

  virtual float get_posX() const {return 0.;}
  virtual void set_posX(const float rval) {return;}

  virtual float get_posY() const {return 0.;}
  virtual void set_posY(const float rval) {return;}

  virtual float get_posZ() const {return 0.;}
  virtual void set_posZ(const float rval) {return;}

   ClassDef(CrkSnglHitExt,1)
};

#endif /* __CRKSNGLHITEXT_H__ */
