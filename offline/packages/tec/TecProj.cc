#include "TecProj.hh"
#include "phool.h"
#include <iostream>

ClassImp(TecProj)

using namespace std;

void
TecProj::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function"
       << endl;
  return;
}


int
TecProj::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void
TecProj::identify(ostream &os) const
{
  os << "identify yourself: TecProj object" << endl;
  return;
}

void
TecProj::warning(const char* field) const 
{
  cout << PHWHERE << "using virtual function, doing nothing" << endl;
  cout << "Offending field == " << field << endl;
}

