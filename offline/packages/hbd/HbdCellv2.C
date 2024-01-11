
#include "HbdCellv2.h"

ClassImp(HbdCellv2)

using namespace std;

HbdCellv2::HbdCellv2()
{


  padnum = -9999;
  sector = -9999;
  arm    = -9999;
  side   = -9999;
  strcpy(secname," ");
  charge = -9999.;
  clusterid = -1;

  return;

}

HbdCellv2::HbdCellv2(HbdCellv2 *cell)
{

  if (!cell) return;

  padnum = cell->get_padnum();
  sector = cell->get_sector();
  arm    = cell->get_arm();
  side   = cell->get_side();
  charge = cell->get_charge();
  clusterid = cell->get_clusterid();


  char tmpname[100];
  cell->get_secchar(tmpname);
  strcpy(secname,tmpname);

  return;

}

void HbdCellv2::identify(ostream& os) const
{
  os << "identify yourself: HbdCellv2 Object\n" << std::endl;
  return;
}

void HbdCellv2::Reset()
{


  padnum = -9999;
  sector = -9999;
  arm    = -9999;
  side   = -9999;
  strcpy(secname," ");
  charge = -9999.;
  clusterid = -1;
  return;
}

int HbdCellv2::isValid() const
{
  return 1;
}

