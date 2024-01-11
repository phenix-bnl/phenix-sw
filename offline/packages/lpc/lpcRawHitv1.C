#include <iostream>
#include "lpcRawHitv1.h"

ClassImp(lpcRawHitv1)

lpcRawHitv1::lpcRawHitv1(short adc, short tdc0)
{
  Adc = adc;
  Tdc0 = tdc0;
}

void lpcRawHitv1::identify(std::ostream& out) const
{
  out << "identify yourself: I am a lpcRawHitv1 object" << std::endl;
}


