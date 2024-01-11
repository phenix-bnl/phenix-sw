#include "CrkHit.h"
#include "phool.h"
#include <iostream>

ClassImp(CrkHit)

using namespace std;

void
CrkHit::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int
CrkHit::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void
CrkHit::identify(ostream &os) const
{
  os << "identify yourself:  CrkHit object" << endl;
  return ;
}
