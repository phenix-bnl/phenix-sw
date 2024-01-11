#include <iostream>
#include "LPolRawHitv1.h"

ClassImp(LPolRawHitv1)

LPolRawHitv1::LPolRawHitv1(short adc, short tdc0, short tdc1)
{
  Adc = adc;
  Tdc0 = tdc0;
  Tdc1 = tdc1;
}

void LPolRawHitv1::identify(std::ostream& out) const
{
  out << "identify yourself: I am a LPolRawHitv1 object" << std::endl;
}

