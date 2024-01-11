
#include "SvxProj.h"
#include <cassert>
#include <iostream>

#include <TObjString.h>
#include <TMath.h>

#include <TGeoVolume.h>
#include <TGeoManager.h>
#include <TGeoNavigator.h>
#include <TGeoMatrix.h>

#if !defined(__CINT__)
ClassImp(SvxProj);
#endif

using namespace std;

SvxProj::SvxProj() :
  fVerbosity(0)
{
}

SvxProj::~SvxProj()
{
}

double
SvxProj::BendDelta(const double r,  // Straight-line proj. distance [cm]
                   const double pt, // p_{T} of particle [GeV/c]
                   const int Z,     // Charge/e (so usually +/- 1)
                   const double B)  // B-field [T]. Sign = polarity.
{
  // Transverse deflection of a particle with pt, Z in B over distance r.
  // Assuming uniform solenoidal field, no scattering, etc.
  if (B==0 || r <= 0)
    return 0.0;

  assert(pt > 0);

  double e = 0.3028221;   // Charge in natural units: e = \sqrt{4\pi\alpha}.

  return 0.01*Z*e*B*r/pt; // Projection distance r / cyclotron radius R.
}

double
SvxProj::BendDPhi(const double r,  // Straight-line proj. distance [cm]
                  const double pt, // p_{T} of particle [GeV/c]
                  const int Z,     // Charge/e (so usually +/- 1)
                  const double B)  // B-field [T]. Sign = polarity.
{
  double d = BendDelta(r, pt, Z, B);

  return TMath::ATan2(d, r);
}

void
SvxProj::FindHitsFromVertex(SvxGeoTrack &track, SvxTGeo *tgeo)
{
  TGeoManager *mgr = tgeo->GeoManager();
  double pt = track.mom*TMath::Sin(track.the0);
  double hits[4] = {-1,-1,-1,-1};
  double phi = track.phi0 + BendDPhi(tgeo->LayerRadius(0), pt, track.charge,
                                     track.bfield);

  // Initialize direction and postion
  double nx = TMath::Sin(track.the0)*TMath::Cos(phi);
  double ny = TMath::Sin(track.the0)*TMath::Sin(phi);
  double nz = TMath::Cos(track.the0);
  mgr->InitTrack(track.vx,track.vy,track.vz,nx,ny,nz);

  if (fVerbosity)
    Printf("Initialized track at (vx,vy,vz) %.2f,%.2f,%.2f with "
           "(nx,ny,nz) %.2f,%.2f,%.2f "
           "charge %d, magField %.2f, mom %.2f, phi bend %.2f",
           mgr->GetCurrentPoint()[0],
           mgr->GetCurrentPoint()[1],
           mgr->GetCurrentPoint()[2],
           mgr->GetCurrentDirection()[0],
           mgr->GetCurrentDirection()[1],
           mgr->GetCurrentDirection()[2],
           track.charge,
           track.bfield,
           track.mom,
           BendDPhi(tgeo->LayerRadius(0), pt, track.charge, track.bfield)
          );

  int nSteps = 0;
  while (!mgr->IsOutside())
  {
    TGeoVolume *vol = mgr->GetCurrentVolume();

    nSteps++;
    if (nSteps > 10)
    {
      if (fVerbosity)
        cout << "Forcing exit after 10 steps.\n" << endl;
      return;
    }

    // Check for traversal of a sensor.
    if (TString(vol->GetName()).Contains("sensor"))
    {
      SvxGeoHit hit;
      TGeoNode *currentNode = mgr->GetCurrentNode();
      SensorAddress(currentNode, hit);
      int layer = hit.layer;

      hits[layer] += 1;

      hit.x      = mgr->GetCurrentPoint()[0];
      hit.y      = mgr->GetCurrentPoint()[1];
      hit.z      = mgr->GetCurrentPoint()[2];
      hit.status = hits[layer];
      //hit.livefrac = GetTileGoodFrac(hit.layer, hit.ladder, hit.sensor, hit.component, hit.tile);
      hit.node   = currentNode;

      // Compute local hit position (wrt center of sensor) from global x,y,z
      if (currentNode)
      {
        TGeoMatrix *M = currentNode->GetMatrix();
        double xyzglobal[3] = {hit.x, hit.y, hit.z};
        double xyzsensor[3] = {0,0,0};
        M->MasterToLocal(xyzglobal, xyzsensor);
        hit.xs = xyzsensor[0];
        hit.ys = xyzsensor[1];
        hit.zs = xyzsensor[2];
      }

      if (TMath::IsNaN(hit.x) || TMath::IsNaN(hit.y) || TMath::IsNaN(hit.z))
        return;

      track.hits.push_back(hit);
      track.nhits++;

      // Print summary of this hit
      if (fVerbosity)
        Printf("   Node: %24s\txyz: (%#6.2f, %#6.2f, %#6.2f)\tstatus %d",
               currentNode->GetName(), hit.x, hit.y, hit.z, hit.status);

      // Step into the next volume & update the TGeoManager.
      // But first adjust the angle if there is a B field
      if (fabs(track.bfield) > 0. && layer < 3)
      {
        double dr = tgeo->LayerRadius(layer+1) - tgeo->LayerRadius(layer);
        nx = mgr->GetCurrentDirection()[0];
        ny = mgr->GetCurrentDirection()[1];
        phi = TMath::ATan2(ny,nx) + BendDPhi(dr, pt, track.charge, track.bfield);
        nx = TMath::Sin(track.the0)*TMath::Cos(phi);
        ny = TMath::Sin(track.the0)*TMath::Sin(phi);
        nz = TMath::Cos(track.the0);
        mgr->SetCurrentDirection(nx,ny,nz);
      }
    }

    TGeoNode *nextNode = mgr->FindNextBoundaryAndStep();

    if (!nextNode)
    {
      if (fVerbosity) cout << endl;
      break;
    }
  }

  return;
}

void
SvxProj::SensorAddress(TGeoNode *node, SvxGeoHit &hit)
{
  TObjArray *arr = TString(node->GetName()).Tokenize("_");

  hit.layer     = ((TObjString *)arr->At(1))->GetString().Atoi();
  hit.ladder    = ((TObjString *)arr->At(2))->GetString().Atoi();
  hit.sensor    = ((TObjString *)arr->At(3))->GetString().Atoi();

  delete arr;
  return;
}
