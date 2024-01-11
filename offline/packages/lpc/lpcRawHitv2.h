#ifndef __LPCRAWHITV2_H
#define __LPCRAWHITV2_H

#include "lpcRawHit.h"

/// Contains the raw hits (AdcPost, AdcPre, Tdc0)
class lpcRawHitv2 : public lpcRawHit
{

 public:
  lpcRawHitv2() { }
  lpcRawHitv2(short adc_post, short adc_pre, short tdc0);
  virtual ~lpcRawHitv2() { }

  short get_AdcPost()  const {return AdcPost;}
  short get_AdcPre()  const {return AdcPre;}
  short get_Tdc0() const {return Tdc0;}

  void identify(std::ostream& os = std::cout) const;


 protected:
  short AdcPost;
  short AdcPre;
  short Tdc0;

  ClassDef(lpcRawHitv2,1)
};

#endif
