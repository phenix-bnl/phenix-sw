#ifndef __CRKSNGLHITV1_H__
#define __CRKSNGLHITV1_H__

#include "CrkSnglHit.h"

class CrkSnglHitv1 : public CrkSnglHit
{
 public:
  CrkSnglHitv1();  
  virtual ~CrkSnglHitv1() {}

  short get_pmt() const {return pmt;}
  void set_pmt(const short ival) {pmt = ival; return;}

  float get_npe() const {return npe;}
  void set_npe(const float rval) {npe = rval; return;}

  float get_time() const {return time;}
  void set_time(const float rval) {time = rval; return;}

 protected:
  short pmt;
  float npe;
  float time;

  ClassDef(CrkSnglHitv1,1)
};

#endif /* __CRKSNGLHITV1_H__ */
