#include <iostream>
#include "LPolRawHit.h"

ClassImp(LPolRawHit)

void LPolRawHit::identify(std::ostream& out) const
{
  out << "identify yourself: I am a LPolRawHit object" << std::endl;
}
