#include "pi0base.h"

ClassImp(pi0base)

using namespace std;
pi0base::pi0base()
{
  Reset();
  return;
}

//this is a default constructor of the class
//pi0base::pi0base(const pi0base &pi)
//{
//  *this = pi;
//  return;
//}

void pi0base::Reset()
{
  event = -9999;
  run = -9999;
  zvtx = -9999;
  
  //particle variables
  frozen = 0;
  e = -9999;
  px = -9999;
  py = -9999;
  pz = -9999;
  pt = -9999;
  mass = -9999;
  theta = -9999;
  phi = -9999;
  //geometrical variables
  arm = -9999;
  //cut variables
  ia= -9999;
  ib=-9999;
  a = -9999;
  b = -9999;
  z = -9999;
  return;
}



