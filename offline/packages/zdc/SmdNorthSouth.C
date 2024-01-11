#include "SmdNorthSouth.h"
#include <iostream>

ClassImp(SmdNorthSouth);

using namespace std;

SmdNorthSouth::SmdNorthSouth(const float xpos, const float ypos, const float energy)
{
  Xpos = xpos;
  Ypos = ypos;
  Energy = energy;
}

void SmdNorthSouth::identify(ostream& out) const
{
  out << "identify yourself: I am a SmdNorthSouth object" << endl;
}
