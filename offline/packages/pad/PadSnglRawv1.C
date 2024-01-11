#include "PadSnglRawv1.h"

ClassImp(PadSnglRawv1)

PadSnglRawv1::PadSnglRawv1()
{
  id = -999;
  index = 9999;
  padtype = -999;
  padx = -999;
  padz = -999;

  return;
}
// Here I do some bit magic since arm and side are either 0 or 1
// and sector goes from 0-4 we can store this in a single short
// the lowest bit is for arm

short PadSnglRawv1::get_arm() const
{
  // return lowest bit
  return (index&0x1);
}

void PadSnglRawv1::set_arm(const short iarm)
{
  // set lowest bit to zero: E = 1110
  index &= 0xFFFE;
  // since arm is 0/1 adding it sets the bit right
  index += iarm;
  return;
}

short PadSnglRawv1::get_side() const
{
  // get the second bit
  short iside = index&0x2;
  // shift it by 1 to make it 0/1
  iside >>= 1;
  return (iside);
}

void PadSnglRawv1::set_side(const short iside)
{
  // mask out 2nd lowest bit D = 1101
  index &= 0xFFFD;
  // shift value by 1 bit (not to overwrite arm bit
  short tmp = iside << 1;
  // add bitshifted side
  index += tmp;
  return;
}

short PadSnglRawv1::get_sector() const
{
  // sector all upper bits, bitshift by 2
  // recovers it
  return (index>>2);
}

void PadSnglRawv1::set_sector(const short isec)
{
  // bitshift sector by 2 to stay clear of arm and side
  short int ishiftsec = isec << 2;
  // mask all upper bits
  index &= 0x3;
  // add bitshifted sector
  index += ishiftsec;
  return;
}
