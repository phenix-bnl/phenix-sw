
#include "SvxGeoTrack.h"
#include "TString.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TMath.h"

#if !defined(__CINT__)
ClassImp(SvxGeoTrack);
ClassImp(SvxGeoHit);
#endif

SvxGeoTrack::SvxGeoTrack() :
  nhits(0),
  charge(0),
  vx(0.),
  vy(0.),
  vz(0.),
  mom(0.),
  phi0(0.),
  the0(0.),
  phirot(0.),
  bfield(0.),
  xydca(0.),
  zdca(0.),
  yp0(0.),
  z0(0.)
{
}

SvxGeoHit::SvxGeoHit() :
  layer(-1),
  ladder(-1),
  sensor(-1),
  component(-1),
  tile(-1),
  x(0.),
  y(0.),
  z(0.),
  xs(0.),
  ys(0.),
  zs(0.),
  ds(-9999.),
  dz(-9999.),
  xsigma(0.),
  zsigma(0.),
  status(-1),
  trkid(-1),
  livefrac(-1),
  node(0)
{
}

void
SvxGeoHit::GetCurrentXYZ(double &x, double &y, double &z)
{
  // Assigns global hit x,y,z in current geometry using local hit position.
  // This point is different from SvxGeoHit::x,y,z if the geometry has been
  // changed after SvxGeoHit::x,y,z were assigned.
  if (node)
  {
    double lxyz[3] = {xs, ys, zs};
    double gxyz[3] = {0};
    node->GetMatrix()->LocalToMaster(lxyz, gxyz);
    x = gxyz[0];
    y = gxyz[1];
    z = gxyz[2];
  }
  else
    Printf("SvxGeoHit::GetCurrentXYZ(): No TGeoNode found for this hit.");

  return;
}

void
SvxGeoHit::Update()
{
  // Update global hit x,y,z position in current geometry.
  // Local xs,ys,zs position on sensor remains unchanged.

  double xc=0, yc=0, zc=0;
  GetCurrentXYZ(xc,yc,zc);

  x = xc;
  y = yc;
  z = zc;

  return;
}

void
SvxGeoHit::UpdateResiduals()
{
  // Get stored position
  double oldphi = TMath::ATan2(y, x);
  double oldrad = TMath::Sqrt(x*x + y*y);

  // Get current position
  double xc=0, yc=0, zc=0;
  GetCurrentXYZ(xc,yc,zc);
  double newphi = TMath::ATan2(yc, xc);
  double newrad = TMath::Sqrt(xc*xc + yc*yc);

  // Update residuals here
  ds -= 0.5*(oldrad + newrad)*(newphi - oldphi);
  dz -= zc - z;

  return;
}

void
SvxGeoTrack::UpdateHits()
{
  for (int i=0; i<nhits; i++)
    hits[i].Update();

  return;
}

SvxGeoHit
SvxGeoTrack::GetHit(int i)
{
  if (i<0 || i>=(int)hits.size())
  {
    Printf("Error in SvxGeoTrack::GetHit(): Hit index %d "
           "outside valid range (0-%d).\n", i, (int)hits.size()-1);
  }
  return hits.at(i);
}


void SvxGeoHit::Print()
{
  Printf("\tlayer,ladder,sensor,component,tile: %d %d %d %d %d",
         layer, ladder, sensor, component, tile);
  Printf("\tstatic x,y,z: %.3f, %.3f, %.3f", x, y, z);
  Printf("\txs,ys,zs: %.3f, %.3f, %.3f", xs, ys, zs);
  Printf("\tds,dz: %.3f, %.3f", ds, dz);
  Printf("\txsigma,zsigma: %.3f, %.3f", xsigma, zsigma);
  Printf("\tstatus,livefrac: %d, %.3f", status, livefrac);
  Printf("\ttrkid: %ld", trkid);
  Printf("\tTGeoNode: %p", node);
  if (node)
  {
    double x=0, y=0, z=0;
    GetCurrentXYZ(x,y,z);
    Printf("\tcurrent x,y,z: %.3f, %.3f, %.3f", x, y, z);
  }
  return;
}

void SvxGeoTrack::Print()
{
  Printf("nhits: %d", nhits);
  Printf("charge: %d", charge);
  Printf("vertex: (%.3f, %.3f, %.3f)", vx, vy, vz);
  Printf("mom, phi0, the0, %.3f, %.3f, %.3f", mom, phi0, the0);
  Printf("phirot %.3f", phirot);
  Printf("bfield %.3f", bfield);
  Printf("DCA %.3f (x-y), %.3f (z)", xydca, zdca);

  for (int j=0; j<nhits; j++)
  {
    Printf("hit %d", j);
    hits[j].Print();
  }
  return;
}
