#include "EmcSnglClusterLocalExtMicrov4.h"

ClassImp(EmcSnglClusterLocalExtMicrov4)

EmcSnglClusterLocalExtMicrov4::EmcSnglClusterLocalExtMicrov4()
{
  short i;
  twrhit = -999;
  index = -9999;
  warnmap = -9999;
  deadmap = -9999;
  qual = -9999.9;
  chi2 = -9999.9;
  chi2_sh = -9999.9;
  e = -9999.9;
  ecore = -9999.9;
  ecorr = -9999.9;
  ecent = -9999.9;
  e9 = -9999.9;
  prob_photon = -9999.9;
  prob_photon_sh = -9999.9;
  re9 = -9999.9;
  tofcorr = -9999.9;
  tofmin = -9999.9;
  tofmax = -9999.9;
  for (i=0;i<2;i++)
    {
      disp[i] = -9999.9;
      padisp[i] = -9999.9;
      yz_cg[i] = -9999.9;
    }
  for (i=0;i<3;i++)
    {
      xyz[i] = -9999.9;
    }  
  return;
}
