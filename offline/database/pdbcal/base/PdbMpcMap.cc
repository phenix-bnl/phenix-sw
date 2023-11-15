#include "PdbMpcMap.hh"

#include <iostream>

using namespace std;

PdbMpcMap::PdbMpcMap()
{
  Reset();
}

void PdbMpcMap::print() const
{
  cout << fee576 << "\t"
       << driver << "\t"
       << gridx << "\t"
       << gridy << "\t"
       << x << "\t"
       << y << "\t"
       << z << endl;
}

void PdbMpcMap::Reset()
{
  fee576 = -9999;
  driver = -9999;
  gridx = -9999;
  gridy = -9999;
  x = 0.;
  y = 0.;
  z = 0.;
}
