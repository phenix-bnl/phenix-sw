#ifndef __ZDCRAWHIT_H
#define __ZDCRAWHIT_H

#include "PHObject.h"  // okay we get TObject from here inheritance
                       // from TObject via PHObject didn't work for
                       // TClonesArrays at the time of development
#include <iostream>

/// Contains the raw DCM hits (Adc, Tdc0, Tdc1)
class ZdcRawHit : public TObject
{

 public:
  ZdcRawHit() { }
  ZdcRawHit(short adc, short tdc0, short tdc1);
  virtual ~ZdcRawHit() { }

  short get_Adc()  const {return Adc;}
  short get_Tdc0() const {return Tdc0;}
  short get_Tdc1() const {return Tdc1;}

  void identify(std::ostream& os = std::cout) const;


 protected:
  short Adc;
  short Tdc0;
  short Tdc1;

  ClassDef(ZdcRawHit,1)
};

#endif
