#ifndef __LPOLRAWHITV1_H
#define __LPOLRAWHITV1_H

#include "LPolRawHit.h"

/// Contains the raw hits (Adc, Tdc0)
class LPolRawHitv1 : public LPolRawHit
{
  
 public:
  LPolRawHitv1() { }
  LPolRawHitv1(short adc, short tdc0, short tdc1);
  virtual ~LPolRawHitv1() { }

  short get_Adc()  const {return Adc;}
  short get_Tdc0() const {return Tdc0;}
  short get_Tdc1() const {return Tdc1;}

  void identify(std::ostream& os = std::cout) const;


 protected:
  short Adc;
  short Tdc0;
  short Tdc1;

  ClassDef(LPolRawHitv1,2)
};

#endif
