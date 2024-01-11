
#include "HbdGhitv1.h"

ClassImp(HbdGhitv1)

using namespace std;

HbdGhitv1::HbdGhitv1()
{

  int i;

  mctrack = -9999;
  tof = -9999.;
  idPart = -9999;
  itParent = -9999;
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
  track = -9999;
  dele = -9999.;
  pathLength = -9999.;
  detector = -9999;
  sector = -9999;
  detflag = -9999;
  isubevent = -9999;
  nfile = -9999;
  ptot = -9999.;
  pt = -9999.;
  theta = -9999.;
  eta = -9999.;
  phi = -9999.;
  npe = -9999;
  nentr = -9999;
  side = -9999;

  for (i=0; i<3; i++) {
    xyzin[i] = -9999.;
    xyzout[i] = -9999.;
    pxyz[i] = -9999.;
    pxyzini[i] = -9999.;
    xyzloc[i] = -9999.;
    xyze[i] = -9999.;
  }

  return;

}

HbdGhitv1::HbdGhitv1(HbdGhitv1 *ghit)
{

  int i;

  if (!ghit) return;

  mctrack = ghit->get_mctrack();
  tof = ghit->get_tof();
  idPart = ghit->get_idPart();
  itParent = ghit->get_itParent();
  idParent = ghit->get_idParent();
  itOrigin = ghit->get_itOrigin();
  idOrigin = ghit->get_idOrigin();
  Ptheta = ghit->get_Ptheta();
  Peta = ghit->get_Peta();
  Pphi = ghit->get_Pphi();
  Rvertex = ghit->get_Rvertex();
  Zvertex = ghit->get_Zvertex();
  Thetavertex = ghit->get_Thetavertex();
  Phivertex = ghit->get_Phivertex();
  track = ghit->get_track();
  dele = ghit->get_dele();
  pathLength = ghit->get_pathLength();
  detector = ghit->get_detector();
  sector = ghit->get_sector();
  side = ghit->get_side();
  detflag = ghit->get_detflag();
  isubevent = ghit->get_isubevent();
  nfile = ghit->get_nfile();
  ptot = ghit->get_ptot();
  pt = ghit->get_pt();
  theta = ghit->get_theta();
  eta = ghit->get_eta();
  phi = ghit->get_phi();
  npe = ghit->get_npe();
  nentr = ghit->get_nentr();

  for (i=0; i<3; i++) {
    xyzin[i] = ghit->get_xyzin(i);
    xyzout[i] = ghit->get_xyzout(i);
    pxyz[i] = ghit->get_pxyz(i);
    pxyzini[i] = ghit->get_pxyzini(i);
    xyzloc[i] = ghit->get_xyzloc(i);
    xyze[i] = ghit->get_xyze(i);
  }

  return;

}

void HbdGhitv1::identify(ostream& os) const
{
  os << "identify yourself: HbdGhitv1 Object\n" << std::endl;
  return;
}

void HbdGhitv1::Reset()
{

  int i;

  mctrack = -9999;
  idPart = -9999;
  itParent = -9999;
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
  track = -9999;
  dele = -9999.;
  tof = -9999.;
  pathLength = -9999.;
  detector = -9999;
  sector = -9999;
  detflag = -9999;
  isubevent = -9999;
  nfile = -9999;
  ptot = -9999.;
  pt = -9999.;
  theta = -9999.;
  eta = -9999.;
  phi = -9999.;
  npe = -9999;
  nentr = -9999;
  side = -999;

  for (i=0; i<3; i++) {
    xyzin[i] = -9999.;
    xyzout[i] = -9999.;
    pxyz[i] = -9999.;
    pxyzini[i] = -9999.;
    xyzloc[i] = -9999.;
    xyze[i] = -9999.;
  }

  return;
}

int HbdGhitv1::isValid() const
{
  return 1;
}

void HbdGhitv1::print()
{

  cout << "HbdGhitv1:" << endl;
  cout << "  mctrack = " << mctrack << endl;
  cout << "  idPart = " << idPart << endl;
  cout << "  itParent = " << itParent << endl;
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
  cout << "  track = " << track << endl;
  cout << "  dele = " << dele << endl;
  cout << "  tof = " << tof << endl;
  cout << "  detector = " << detector << endl;
  cout << "  sector = " << sector << endl;
  cout << "  side = " << side << endl;
  cout << "  detflag = " << detflag << endl;
  cout << "  isubevent = " << isubevent << endl;
  cout << "  nfile = " << nfile << endl;
  cout << "  xyzin = (" << xyzin[0] << "," << xyzin[1]
       << "," << xyzin[2] << ")" << endl;
  cout << "  xyzout = (" << xyzout[0] << "," << xyzout[1]
       << "," << xyzout[2] << ")" << endl;
  cout << "  pxyz = (" << pxyz[0] << "," << pxyz[1]
       << "," << pxyz[2] << ")" << endl;
  cout << endl;
  cout << "  ptot = " << ptot << endl;
  cout << "  pt = " << pt << endl;
  cout << "  theta = " << theta << endl;
  cout << "  eta = " << eta << endl;
  cout << "  phi = " << phi << endl;
  cout << "  npe = " << npe << endl;
  cout << "  nentr = " << nentr << endl;
  cout << "  xyzloc = (" << xyzloc[0] << "," << xyzloc[1]
       << "," << xyzloc[2] << ")" << endl;
  cout << "  xyze = (" << xyze[0] << "," << xyze[1]
       << "," << xyze[2] << ")" << endl;
  cout << "  pxyzini = (" << pxyzini[0] << "," << pxyzini[1]
       << "," << pxyzini[2] << ")" << endl;

  return;

}









