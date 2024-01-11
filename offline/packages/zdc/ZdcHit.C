#include <iostream>
#include "ZdcHit.h"

ClassImp(ZdcHit)

ZdcHit::ZdcHit(float charge, float time0, float time1)
{
  Charge = charge;
  Time0 = time0;
  Time1 = time1;
}

void ZdcHit::identify(std::ostream& out) const
{
  out << "identify yourself: I am a ZdcHit object" << std::endl;
}

