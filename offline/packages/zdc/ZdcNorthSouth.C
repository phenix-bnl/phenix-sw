#include "ZdcNorthSouth.h"
#include <iostream>

ClassImp(ZdcNorthSouth)

ZdcNorthSouth::ZdcNorthSouth(float energy, float timing)
{
  Energy = energy;
  Timing = timing;
}


void ZdcNorthSouth::identify(std::ostream& out) const
{
  out << "identify yourself: I am a ZdcNorthSouth object" << std::endl;
}
