#include <iostream>
#include "ZdcRawHit.h"

using namespace std;

ClassImp(ZdcRawHit)

ZdcRawHit::ZdcRawHit(short adc, short tdc0, short tdc1)
{
  Adc = adc;
  Tdc0 = tdc0;
  Tdc1 = tdc1;
}

void ZdcRawHit::identify(std::ostream& out) const
{
  out << "identify yourself: I am a ZdcRawHit object" << std::endl;
}


