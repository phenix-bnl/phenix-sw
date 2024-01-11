#include "TofHitv1.h"

ClassImp(TofHitv1)

  TofHitv1::TofHitv1()
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
  eloss = -9999.9;
  eloss_err = -9999.9;
  tof = -9999.9;
  tof_err = -9999.9;
  for (i=0;i<3;i++)
    {
      xtof[i] = -9999.9;
      xtof_err[i] = -9999.9;
    }
  return;
}
