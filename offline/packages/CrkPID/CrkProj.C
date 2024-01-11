#include "CrkProj.h"
#include "phool.h"
#include <iostream>

ClassImp(CrkProj)

using namespace std;

void
CrkProj::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function"
       << endl;
  return;
}


int
CrkProj::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void
CrkProj::identify(ostream &os) const
{
  os << "identify yourself: CrkProj object" << endl;
  return;
}

void
CrkProj::warning(const char* field) const 
{
  cout << PHWHERE << "using virtual function, doing nothing" << endl;
  cout << "Offending field == " << field << endl;
}

