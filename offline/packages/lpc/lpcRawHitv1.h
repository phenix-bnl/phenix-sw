#ifndef __LPCRAWHITV1_H
#define __LPCRAWHITV1_H

#include "lpcRawHit.h"

/// Contains the raw hits (Adc, Tdc0)
class lpcRawHitv1 : public lpcRawHit
{
  
 public:
  lpcRawHitv1() { }
  lpcRawHitv1(short adc, short tdc0);
  virtual ~lpcRawHitv1() { }

  short get_Adc()  const {return Adc;}
  short get_Tdc0() const {return Tdc0;}

  void identify(std::ostream& os = std::cout) const;


 protected:
  short Adc;
  short Tdc0;

  ClassDef(lpcRawHitv1,2)
};

#endif
