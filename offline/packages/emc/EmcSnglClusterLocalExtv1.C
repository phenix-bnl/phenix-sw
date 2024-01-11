#include "EmcSnglClusterLocalExtv1.h"

ClassImp(EmcSnglClusterLocalExtv1)

  EmcSnglClusterLocalExtv1::EmcSnglClusterLocalExtv1()
{
  short i;
  clusno = -999;
  id = -999;
  method = -999;
  nsh = -999;
  twrhit = -999;
  type = -999;

  index = -9999;
  warnmap = -9999;
  deadmap = -9999;

  for (i = 0;i < 16;i++)
    {
      twrlist[i] = -999;
    }

  chi2 = -9999.9;
  chi2_sh = -9999.9;
  de = -9999.9;
  dtof = -9999.9;
  e = -9999.9;
  ecore = -9999.9;
  ecorr = -9999.9;
  ecent = -9999.9;
  etofmax = -9999.9;
  etofmin = -9999.9;
  e9 = -9999.9;
  phi = -9999.9;
  pid = -9999.9;
  prob_neuhad = -9999.9;
  prob_photon = -9999.9;
  prob_photon_sh = -9999.9;
  qual = -9999.9;
  re9 = -9999.9;
  theta = -9999.9;
  tof = -9999.9;
  tofcorr = -9999.9;
  tofmax = -9999.9;
  tofmaxcorr = -9999.9;
  tofmean = -9999.9;
  tofmin = -9999.9;
  tofmincorr = -9999.9;
  for (i = 0;i < 2;i++)
    {
      de_sh[i] = -9999.9;
      disp[i] = -9999.9;
      ecorr_sh[i] = -9999.9;
      e_sh[i] = -9999.9;
      padisp[i] = -9999.9;
      yz_cg[i] = -9999.9;
      for (short j = 0;j < 3;j++)
        {
          dxyz_sh[i][j] = -9999.9;
          xyz_sh[i][j] = -9999.9;
        }
    }
  for (i = 0;i < 3;i++)
    {
      dxyz[i] = -9999.9;
      unitv[i] = -9999.9;
      xyz[i] = -9999.9;
    }
  for (i = 0;i < 16;i++)
    {
      partesum[i] = -9999.9;
    }

  return ;
}
