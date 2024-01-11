#include <cmath>
#include "TofHitv2.h"

ClassImp(TofHitv2);
using namespace std;

TofHitv2::TofHitv2()
{
  short int i;
  id = -9999;
  panel = -9999;
  sector = -9999;
  side = -9999;
  slat = -9999;
  slatid = -9999;
  for (i=0;i<2;i++)
    {
      qvc[i] = -9999;
      tvc[i] = -9999;
    }
  eloss = NAN;
  eloss_err = NAN;
  tof = NAN;
  tof_err = NAN;
  for (i=0;i<3;i++)
    {
      xtof[i] = NAN;
      xtof_err[i] = NAN;
    }
  return;
}

void TofHitv2::identify(ostream& os) const
{
  short int i;
  os << "TofHitv2 Object Content:" << endl;
  os << "id: " << get_id() 
     << ", panel: " << get_panel()
     << ", sector: " << get_sector()
     << ", side: " << get_side()
     << ", slat: " << get_slat()
     << ", slatid: " << get_slatid()
     << endl;
  os << "tof: " << get_tof()
     << ", toferr: " << get_tof_err()
     << ", eloss: " << get_eloss()
     << ", eloss_err: " << get_eloss_err()
     << endl;

  for (i=0;i<3;i++)
    {
      os << "qvc(" << i << "): " << get_qvc(i);
      os << ", ";
    }
  os << endl;

  for (i=0;i<3;i++)
    {
      os << "tvc(" << i << "): " << get_tvc(i);
      os << ", ";
    }
  os << endl;

  for (i=0;i<3;i++)
    {
      os << "xtof(" << i << "): " << get_xtof(i);
      os << ", xtof_err(" << i << "): " << get_xtof_err(i);
      os << endl;
    }
  return;
}


