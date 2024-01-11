#include "TecOut.hh"
#include "phool.h"
#include <iostream>

ClassImp(TecOut)
using namespace std;

void
TecOut::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}
int
TecOut::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter class" << endl;
  return 0;
}

void
TecOut::identify(ostream& os) const
{
  os << "virtual TecOut object." << endl;
}
