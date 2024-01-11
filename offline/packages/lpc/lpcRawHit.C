#include <iostream>
#include "lpcRawHit.h"

ClassImp(lpcRawHit)

void lpcRawHit::identify(std::ostream& out) const
{
  out << "identify yourself: I am a lpcRawHit object" << std::endl;
}


