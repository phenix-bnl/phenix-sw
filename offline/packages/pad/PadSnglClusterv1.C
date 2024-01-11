#include "PadSnglClusterv1.h"

ClassImp(PadSnglClusterv1)

PadSnglClusterv1::PadSnglClusterv1()
{
  cell = -999;
  id = -999;
  index = 0;
  type = -999;
  wire = -999;

  for (short i =0;i<3;i++)
    {
      dxyz[i] = -999.9;
      xyz[i] = -999.9;
    }
  return;
}

short PadSnglClusterv1::get_arm() const
{
  return (index&0x1);
}

short PadSnglClusterv1::get_sector() const
{
  return (index/10);
}
