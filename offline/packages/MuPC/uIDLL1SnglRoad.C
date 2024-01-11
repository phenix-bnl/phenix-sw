#include "uIDLL1SnglRoad.h"
#include "phool.h"
#include <iostream>

ClassImp(uIDLL1SnglRoad)

using namespace std;

void
uIDLL1SnglRoad::warning(const char* field) const
{
  cout << PHWHERE << "using virtual function, doing nothing" << endl;
  cout << "Single uIDLL1 road Offending field == " << field << endl;
}
