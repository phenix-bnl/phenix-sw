#include <PHGlobal_Central.h>

#include <phool.h>

#include <cmath>

static int shutup = 0;

using namespace std;

ClassImp(PHGlobal_Central)

  void
PHGlobal_Central::ShutUp(const int i)
{
  shutup = i;
  return ;
}

void
PHGlobal_Central::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

void
PHGlobal_Central::identify(ostream& os) const
{
  os << "identify yourself: virtual PHGlobal_Central Object" << std::endl;
  return ;
}

int
PHGlobal_Central::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

float
PHGlobal_Central::getEmcEnergyW() const
{
  warning("getEmcEnergyW");
  return NAN;
}

float
PHGlobal_Central::getEmcEnergyE() const
{
  warning("getEmcEnergyE");
  return NAN;
}

float
PHGlobal_Central::getEmcEnergy() const
{
  warning("getEmcEnergy");
  return NAN;
}

void
PHGlobal_Central::warning(const char* field) const
{
  if (!shutup)
    {
      cout << "PHGlobal_Central::using virtual function, doing nothing" << endl;
      cout << "Offending field == " << field << endl;
    }
  return ;
}

