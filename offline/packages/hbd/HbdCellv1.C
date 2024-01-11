
#include "HbdCellv1.h"

ClassImp(HbdCellv1)

using namespace std;

HbdCellv1::HbdCellv1()
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

HbdCellv1::HbdCellv1(HbdCellv1 *cell)
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

void HbdCellv1::identify(ostream& os) const
{
  os << "identify yourself: HbdCellv1 Object\n" << std::endl;
  return;
}

void HbdCellv1::Reset()
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

int HbdCellv1::isValid() const
{
  return 1;
}

