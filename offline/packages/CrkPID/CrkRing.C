#include "CrkRing.h"
#include "phool.h"
#include <iostream>

ClassImp(CrkRing)

using namespace std;

void
CrkRing::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function"
       << endl;
  return;
}


int
CrkRing::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void
CrkRing::identify(ostream &os) const
{
  os << "identify yourself: CrkRing object" << endl;
  return;
}

void
CrkRing::warning(const char* field) const 
{
  cout << PHWHERE << "using virtual function, doing nothing" << endl;
  cout << "Offending field == " << field << endl;
}

