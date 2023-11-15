#include "PdbMpcPinMap.hh"

#include <iostream>

using namespace std;


PdbMpcPinMap::PdbMpcPinMap()
{
  Reset();
}

void PdbMpcPinMap::print() const
{
  cout << fee576 << "\t"
       << pinfee576 << endl;
}

void PdbMpcPinMap::Reset()
{
  fee576 = -9999;
  pinfee576 = -9999;
}
