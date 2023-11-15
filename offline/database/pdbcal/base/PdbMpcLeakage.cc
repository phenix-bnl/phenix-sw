//
// $Id: PdbMpcLeakage.cc,v 1.1 2009/02/26 00:55:42 chiu Exp $
//

#include <PdbMpcLeakage.hh>
//#include <iomanip>

using namespace std;

PdbMpcLeakage::PdbMpcLeakage()
{
  reset();
}

PdbMpcLeakage& PdbMpcLeakage::operator=(const PdbMpcLeakage& p)
{
  x = p.getX();
  y = p.getY();
  leakage = p.getLeakage();
  dleakage = p.getLeakageError();

  return *this;
}

void PdbMpcLeakage::reset()
{
  x = 0xffff;
  y = 0xffff;
  leakage = 0xffff;
  dleakage = 0xffff;
}

void PdbMpcLeakage::print() const
{
  cout << "mpcleakage" << endl;
}

