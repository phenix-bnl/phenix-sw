#include "MvdSnglRPhiZv1.h"
#include <iostream>

ClassImp(MvdSnglRPhiZv1)

using namespace std;

MvdSnglRPhiZv1::MvdSnglRPhiZv1(MvdSnglRPhiZv1* newrphiz)
{
  rphiz = 0;
  set_r(newrphiz->get_r());
  set_phi(newrphiz->get_phi());
  set_z(newrphiz->get_z());
  set_adc(newrphiz->get_adc());
}

void 
MvdSnglRPhiZv1::identify(std::ostream& os) const
{
  os << "identify yourself: I am a MvdSnglRPhiZv1 Object" << std::endl;
  return ;
}

short
MvdSnglRPhiZv1::set_r(const unsigned short r)
{
  if (!checkR(r))
    {
      cerr << "r==" << r << " is out of range!" << endl;
      return 1;
    }
  rphiz = rphiz & ~(RMASK << RSHIFT);       // clear the "r" bits
  rphiz = rphiz | ((r & RMASK) << RSHIFT);  // shift in the "r" bits
  return 0;
}

short
MvdSnglRPhiZv1::set_phi(const unsigned short phi)
{
  if (!checkPhi(phi))
    {
      cerr << "phi==" << phi << " is out of range!" << endl;
      return 1;
    }
  rphiz = rphiz & ~(PHIMASK << PHISHIFT);       // clear the "phi" bits
  rphiz = rphiz | ((phi & PHIMASK) << PHISHIFT);  // shift in the "phi" bits
  return 0;
}

short
MvdSnglRPhiZv1::set_z(const unsigned short z)
{
  if (!checkZ(z))
    {
      cerr << "z==" << z << " is out of range!" << endl;
      return 1;
    }
  rphiz = rphiz & ~(ZMASK << ZSHIFT);       // clear the "z" bits
  rphiz = rphiz | ((z & ZMASK) << ZSHIFT);  // shift in the "z" bits
  return 0;
}
