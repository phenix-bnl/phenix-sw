#include <iostream>
#include "SmdHit.h"

ClassImp(SmdHit)

SmdHit::SmdHit(float charge, float time0, float time1)
{
  Charge = charge;
  Time0 = time0;
  Time1 = time1;
}

void SmdHit::identify(std::ostream& out) const
{
  out << "identify yourself: I am a SmdHit object" << std::endl;
}

