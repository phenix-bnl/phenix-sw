#include <iostream>
#include "lpcRawHitv2.h"

ClassImp(lpcRawHitv2)

lpcRawHitv2::lpcRawHitv2(short adc_post, short adc_pre, short tdc0)
{
  AdcPost = adc_post;
  AdcPre = adc_pre;
  Tdc0 = tdc0;
}

void lpcRawHitv2::identify(std::ostream& out) const
{
  out << "identify yourself: I am a lpcRawHitv2 object" << std::endl;
}


