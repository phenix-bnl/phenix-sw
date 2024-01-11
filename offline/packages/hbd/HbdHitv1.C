
#include "HbdHitv1.h"

ClassImp(HbdHitv1)

using namespace std;

HbdHitv1::HbdHitv1()
{

  int i;

  track = -9999;
  idPart = -9999;
  idParent = -9999;
  itOrigin = -9999;
  idOrigin = -9999;
  Ptheta = -9999.;
  Peta = -9999.;
  Pphi = -9999.;
  Rvertex = -9999.;
  Zvertex = -9999.;
  Thetavertex = -9999.;
  Phivertex = -9999.;
  sector = -9999;
  npe = -9999;
  nentr = -9999;
  detflag = -9999;
  ptot = -9999.;
  theta = -9999.;
  eta = -9999.;
  phi = -9999.;
  pt = -9999.;
  tof = -9999.;
  dy = -9999.;
  dz = -9999.;
  rmsy = -9999.;
  rmsz = -9999.;
  charge = -9999.;
  time = -9999.;
  size = -9999;
  nct = -9999;
  part = -9999.;
  id = -9999;
  for (i=0; i<3; i++) {
    xyzin[i] = -9999.;
    pxyz[i] = -9999.;
    pxyzini[i] = -9999.;
    xyzloc[i] = -9999.;
    xyze[i] = -9999.;
  }

  return;

}

HbdHitv1::HbdHitv1(HbdHitv1 *hit)
{

  int i;

  if (!hit) return;

  track = hit->get_track();
  idPart = hit->get_idPart();
  idParent = hit->get_idParent();
  itOrigin = hit->get_itOrigin();
  idOrigin = hit->get_idOrigin();
  Ptheta = hit->get_Ptheta();
  Peta = hit->get_Peta();
  Pphi = hit->get_Pphi();
  Rvertex = hit->get_Rvertex();
  Zvertex = hit->get_Zvertex();
  Thetavertex = hit->get_Thetavertex();
  Phivertex = hit->get_Phivertex();
  sector = hit->get_sector();
  npe = hit->get_npe();
  nentr = hit->get_nentr();
  detflag = hit->get_detflag();
  ptot = hit->get_ptot();
  theta = hit->get_theta();
  eta = hit->get_eta();
  phi = hit->get_phi();
  pt = hit->get_pt();
  tof = hit->get_tof();
  dy = hit->get_dy();
  dz = hit->get_dz();
  rmsy = hit->get_rmsy();
  rmsz = hit->get_rmsz();
  charge = hit->get_charge();
  time = hit->get_time();
  size = hit->get_size();
  nct = hit->get_nct();
  part = hit->get_part();
  id = hit->get_id();
  for (i=0; i<3; i++) {
    xyzin[i] = hit->get_xyzin(i);
    pxyz[i] = hit->get_pxyz(i);
    pxyzini[i] = hit->get_pxyzini(i);
    xyzloc[i] = hit->get_xyzloc(i);
    xyze[i] = hit->get_xyze(i);
  }

  return;

}

void HbdHitv1::identify(ostream& os) const
{
  os << "identify yourself: HbdHitv1 Object\n" << std::endl;
  return;
}

void HbdHitv1::Reset()
{

  int i;

  track = -9999;
  idPart = -9999;
  idParent = -9999;
  itOrigin = -9999;
  idOrigin = -9999;
  Ptheta = -9999.;
  Peta = -9999.;
  Pphi = -9999.;
  Rvertex = -9999.;
  Zvertex = -9999.;
  Thetavertex = -9999.;
  Phivertex = -9999.;
  sector = -9999;
  npe = -9999;
  nentr = -9999;
  detflag = -9999;
  ptot = -9999.;
  theta = -9999.;
  eta = -9999.;
  phi = -9999.;
  pt = -9999.;
  tof = -9999.;
  dy = -9999.;
  dz = -9999.;
  rmsy = -9999.;
  rmsz = -9999.;
  charge = -9999.;
  time = -9999.;
  size = -9999;
  nct = -9999;
  part = -9999.;
  id = -9999;
  for (i=0; i<3; i++) {
    xyzin[i] = -9999.;
    pxyz[i] = -9999.;
    pxyzini[i] = -9999.;
    xyzloc[i] = -9999.;
    xyze[i] = -9999.;
  }


  return;
}

int HbdHitv1::isValid() const
{
  return 1;
}

void HbdHitv1::print()
{

  cout << "HbdHitv1:" << endl;
  cout << "  track = " << track << endl;
  cout << "  idPart = " << idPart << endl;
  cout << "  idParent = " << idParent << endl;
  cout << "  itOrigin = " << itOrigin << endl;
  cout << "  idOrigin = " << idOrigin << endl;
  cout << "  Ptheta = " << Ptheta << endl;
  cout << "  Peta = " << Peta << endl;
  cout << "  Pphi = " << Pphi << endl;
  cout << "  Rvertex = " << Rvertex << endl;
  cout << "  Zvertex = " << Zvertex << endl;
  cout << "  Thetavertex = " << Thetavertex << endl;
  cout << "  Phivertex = " << Phivertex << endl;
  cout << "  sector = " << sector << endl;
  cout << "  npe = " << npe << endl;
  cout << "  nentr = " << nentr << endl;
  cout << "  detflag = " << detflag << endl;
  cout << "  xyzin = (" << xyzin[0] << "," << xyzin[1]
       << "," << xyzin[2] << ")" << endl;
  cout << "  pxyz = (" << pxyz[0] << "," << pxyz[1]
       << "," << pxyz[2] << ")" << endl;
  cout << endl;
  cout << "  ptot = " << ptot << endl;
  cout << "  theta = " << theta << endl;
  cout << "  eta = " << eta << endl;
  cout << "  phi = " << phi << endl;
  cout << "  xyzloc = (" << xyzloc[0] << "," << xyzloc[1]
       << "," << xyzloc[2] << ")" << endl;
  cout << "  xyze = (" << xyze[0] << "," << xyze[1]
       << "," << xyze[2] << ")" << endl;
  cout << "  pt = " << pt << endl;
  cout << "  tof = " << tof << endl;
  cout << "  dy = " << dy << endl;
  cout << "  dz = " << dz << endl;
  cout << "  rmsy = " << rmsy << endl;
  cout << "  rmsz = " << rmsz << endl;
  cout << "  charge = " << charge << endl;
  cout << "  time = " << time << endl;
  cout << "  size = " << size << endl;
  cout << "  nct = " << nct << endl;
  cout << "  part = " << part << endl;
  cout << "  id = " << id << endl;
  cout << "  pxyzini = (" << pxyzini[0] << "," << pxyzini[1]
       << "," << pxyzini[2] << ")" << endl;

  return;

}









