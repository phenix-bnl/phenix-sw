#include "CrkHitExt.h"
#include "phool.h"
#include <iostream>

ClassImp(CrkHitExt)

using namespace std;

void
CrkHitExt::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int
CrkHitExt::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void
CrkHitExt::identify(ostream &os) const
{
  os << "identify yourself:  CrkHitExt object" << endl;
  return ;
}
