
#include "HbdMiniCellv1.h"

ClassImp(HbdMiniCellv1)

using namespace std;

HbdMiniCellv1::HbdMiniCellv1()
{

  adcch = -9999;
  charge = -9999;

  return;
}

HbdMiniCellv1::HbdMiniCellv1(HbdMiniCellv1 *cell)
{

  if (!cell) return;

  adcch = cell->get_adcch();
  charge = cell->get_charge();

  return;

}

void HbdMiniCellv1::identify(ostream& os) const
{
  os << "identify yourself: HbdMiniCellv1 Object\n" << std::endl;
  return;
}

void HbdMiniCellv1::Reset()
{

  adcch = -9999;
  charge = -9999;
  return;
}

int HbdMiniCellv1::isValid() const
{
  return 1;
}

