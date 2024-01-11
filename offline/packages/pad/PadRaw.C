#include "PadRaw.h"
#include "phool.h"
#include <iostream>

ClassImp(PadRaw)

using namespace std;

void
PadRaw::identify(ostream& os) const
{
  os << " PadRaw object" << endl;
  return ;
}

void
PadRaw::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}

int
PadRaw::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter class" << endl;
  return 0;
}
