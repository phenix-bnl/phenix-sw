
#include "MuPCSnglClusterv1.h"

ClassImp(MuPCSnglClusterv1)

MuPCSnglClusterv1::MuPCSnglClusterv1()
{

  idpart = -9999;
  idparent = -9999;
  idorigin = -9999;
  phi = -9999;
  xglo = -9999;
  yglo = -9999;
  zglo = -9999;
  ptot = -9999;
  ptheta = -9999;
  pphi = -9999;
  r_vertex = -9999;
  z_vertex = -9999;
  ptotpri = -9999;
  pphipri = -9999;
  pthetpri = -9999;
  z0vertex = -9999;


  return;
}

MuPCSnglClusterv1::MuPCSnglClusterv1(MuPCSnglClusterv1 *clus)
{

  if (!clus) return;

  idpart = clus->get_idpart();
  idparent = clus->get_idparent();
  idorigin = clus->get_idorigin();
  phi = clus->get_phi();
  xglo = clus->get_xglo();
  yglo = clus->get_yglo();
  zglo = clus->get_zglo();
  ptot = clus->get_ptot();
  ptheta = clus->get_ptheta();
  pphi = clus->get_pphi();
  r_vertex = clus->get_r_vertex();
  z_vertex = clus->get_z_vertex();
  ptotpri = clus->get_ptotpri();
  pphipri = clus->get_pphipri();
  pthetpri = clus->get_pthetpri();
  z0vertex = clus->get_z0vertex();

  return;
}
