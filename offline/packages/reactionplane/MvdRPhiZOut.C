#include "MvdRPhiZOut.h"
#include "phool.h"
#include <iostream>
#include "MvdGeometry.hh"

ClassImp(MvdRPhiZOut)

using namespace std;

void MvdRPhiZOut::Reset()
{
  cout << PHWHERE << "ERROR Reset not implemented by daughter class" << endl;
  return ;
}

void MvdRPhiZOut::identify(ostream& os) const
{
  os << "identify yourself:  MvdRPhiZOut object" << endl;
  return ;
}

int MvdRPhiZOut::isValid() const
{
  cout << PHWHERE << "isValid not implemented by daughter class" << endl;
  return 0;
}

float 
MvdRPhiZOut::getR(unsigned int i) const
{
  return MvcGeometry::instance()->Radius(get_r(i));
}

float 
MvdRPhiZOut::getPhi(unsigned int i) const
{
  unsigned short iPhi = get_phi(i);
  return MvcGeometry::instance()->Phi(iPhi/12, iPhi%12);
}

float 
MvdRPhiZOut::getZ(unsigned int i) const
{
  return MvcGeometry::instance()->Z(get_z(i));
}
