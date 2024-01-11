#include <cmath>

#include "PadSnglClusterv2.h"

ClassImp(PadSnglClusterv2)

PadSnglClusterv2::PadSnglClusterv2()
{
  cell = -999;
  id = -999;
  index = 0;
  type = -999;
  wire = -999;

  for (short i =0;i<3;i++)
    {
      dxyz[i] = NAN;
      padxyz[i] = NAN;
    }
  return;
}

short PadSnglClusterv2::get_arm() const
{
  return (index&0x1);
}

void PadSnglClusterv2::set_arm(const short iarm)
{
  // set lowest bit to zero: E = 1110
  index &= 0xFFFE;
  // since arm is 0/1 adding it sets the bit right
  index += iarm;
  return;
}


short PadSnglClusterv2::get_sector() const
{
  return (index/10);
}

void PadSnglClusterv2::set_sector(const short isec)
{
  short iarm = get_arm();
  // set lowest bit to zero: E = 1110
  index = isec*10;
  // add arm back to index
  index += iarm;
  return;
}
