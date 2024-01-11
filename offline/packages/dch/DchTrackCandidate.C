// PHENIX Drift Chamber Software: Stony Brook Mon Jul  6 15:32:11 1998
// Source file of the Class: DchTrackCandidate
// Created by: Federica Ceretto
// 
// Last update: Mon Jul  6 15:32:11 1998
// Last update: Mon Oct 22 15:32:11 2001 -- Enlarged initial UV array size.
// Last update: Nov 25 2001 -- Enlarged intial HitLine Lengths for stereo Hits.
// Last update: Jan 29 2002 -- added correct support for Z coordinate.
// 
//---------------------------------------------------------
 
#include "DchTrackCandidate.hh" 

#include "DchTrackCandidate.hh"
#include "PHGeometry.h"

#include <iostream>

using namespace std;
using namespace PHGeometry;

DchTrackCandidate::DchTrackCandidate()
{
  // The default constructor does not produce a useful result!
  // I recommend that you do not use it.
  // The values are both meaningless and inconsistent!
  //                       TKH 1-29-2002.
  arm          = -1;
  side         = 2;
  trackId      = -1;
  trackInfo    = 0;
  localAlpha   = -9999;
  localPhi     = -9999;
  localSlope   = -9999;
  localIntercept = -9999;

  globalAlpha = 0;
  globalPhi   = 0;
  localBeta   = Pi/2.;
  localZed    = 0;
  globalBeta  = Pi/2.;
  globalZed   = 0;
  xHough      = 0;
  uvHough     = 0;
  finalPc1Id  = -1;

  quality = 0;

  chi2OfFit = -9999;
  numberOfFittedHits = 0;

  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i]  = -1;
    associatedChi2[i] = 10000.; 
  }
  uv1       = new PHPointerList<DchHitLine>(20);
  uv2       = new PHPointerList<DchHitLine>(20);
  UV        = new PHPointerList<DchHitLine>(20);
  closestUV = new PHPointerList<DchHitLine>(32);
  x1        = new PHPointerList<DchHitLine>(50);
  x2        = new PHPointerList<DchHitLine>(50);
  X         = new PHPointerList<DchHitLine>(50);
  closestX  = new PHPointerList<DchHitLine>(24);
  pc1       = new PHPointerList<DchPc1Hit>(2);

}

DchTrackCandidate::DchTrackCandidate(short a, short s, float alpha, float phi, float slope, float refRad1)
{
  // This constructor is used just after the Hough Plane is solved.
  //        This constructor is called at a stage in the analysis
  //        where the Z solution is not yet known!  In this case, we place 
  //        all south candidates at Z=-50 cm and all north candidates at Z=+50cm.
  //        We also set the Beta = Pi/2, radial in cyl(!) coords.
  //                                 TKH 1-29-2002
  arm = a;
  side = s;
  trackId = -1;
  trackInfo = 0;

  // Fill the local floats with meaningful values...
  localAlpha = asin(sin(alpha) * refRad1 / referenceRadius);
  localPhi = phi + localAlpha - alpha;
  localSlope = slope;
  localIntercept = sin(localPhi) * referenceRadius - 
    localSlope * cos(localPhi) * referenceRadius;
  localBeta   = Pi/2.;  // Outward -- TKH
  if (side==(int) SOUTH) {
    localZed  = -50;
  }
  else {
    localZed  = 50;
  }
  calculateLocalPoints();  // Transfer local floats to local PHPoints...
  calculateLocalPlanes();  // Transfer local Points to local Planes...

  // Fill the global floats and various counters with MEANINGLESS values...
  // This meaninglessness is corrected by the candidate finder since the 
  // candidate itself has no access to the geometry...
  globalAlpha = 0;
  globalPhi   = 0;
  globalBeta  = 0;
  globalZed   = 0;
  xHough = 0;
  uvHough = 0;
  quality = 0;
  finalPc1Id = -1;
  chi2OfFit          = -9999;
  numberOfFittedHits = 0;
  distanceToAxis     = -9999;

  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i]  = -1;
    associatedChi2[i] = 10000.; 
    associatedPoint[i] = PHPoint(0,0,0);
  }

  // Make new pointer lists to hold results of association...
  uv1       = new PHPointerList<DchHitLine>(20);
  uv2       = new PHPointerList<DchHitLine>(20);
  UV        = new PHPointerList<DchHitLine>(20);
  closestUV = new PHPointerList<DchHitLine>(32);
  x1        = new PHPointerList<DchHitLine>(50);
  x2        = new PHPointerList<DchHitLine>(50);
  X         = new PHPointerList<DchHitLine>(50);
  closestX  = new PHPointerList<DchHitLine>(24);
  pc1       = new PHPointerList<DchPc1Hit>(2);

}

DchTrackCandidate::DchTrackCandidate(short a, short s, 
				     PHPoint point, PHVector vector)
{
  // I'm not sure who calls this constructor, but similarly to above,
  // it puts meaningful values in the local floats, points, and vectors.
  // It puts MEANINGLESS values into the global floats, points, and vectors.
  //                          TKH-- 1-29-2002.

  arm            = a;
  side           = s;
  trackId        = -1;
  trackInfo      = 0;
  localPoint     = point;
  cylLocalPoint  = (PHCylPoint)point;
  localVector    = vector;
  calculateLocal();       // calculate local "floats" from Point, Vector.
  calculateLocalPlanes(); // calculate local Planes from Points & Vectors.  
  
  // Meaningless global and counters at instantiation.
  globalAlpha = 0;
  globalPhi   = 0;
  globalBeta  = 0;
  globalZed   = 0;
  xHough      = 0;
  uvHough     = 0;
  quality     = 0;
  finalPc1Id  = -1;
  chi2OfFit = -9999;
  numberOfFittedHits = 0;
  distanceToAxis     = -9999;

  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i]  = -1;
    associatedChi2[i] = 10000.; 
  }
  uv1       = new PHPointerList<DchHitLine>(20);
  uv2       = new PHPointerList<DchHitLine>(20);
  UV        = new PHPointerList<DchHitLine>(20);
  closestUV = new PHPointerList<DchHitLine>(32);
  x1        = new PHPointerList<DchHitLine>(50);
  x2        = new PHPointerList<DchHitLine>(50);
  X         = new PHPointerList<DchHitLine>(50);
  closestX  = new PHPointerList<DchHitLine>(24);
  pc1       = new PHPointerList<DchPc1Hit>(2);

}

DchTrackCandidate::~DchTrackCandidate()
{
  uv1->clear();
  uv2->clear();
  UV->clear();
  closestUV->clear();
  x1->clear();
  x2->clear();
  X->clear();
  closestX->clear();
  pc1->clear();

  delete uv1;
  delete uv2;
  delete UV;
  delete closestUV;
  delete x1;
  delete x2;
  delete X;
  delete closestX;
  delete pc1;
}

int
DchTrackCandidate::numberOfHits()
{
  int total = 0;
  for (int i = 0; i < numberOfPlanes; i++)
    {
      if (associatedHit[i] > -1)
        {  // if hit id is bigger than -1
          total++;
        }
    }
  return total;
}

void
DchTrackCandidate::update(PHPoint localp, PHVector localv,
                          PHPoint globalp, PHVector globalv)
{
  // Redefines the candidate from scratch (all parameters!) based upon new
  // points and vectors in both global and local.
  localPoint = localp;
  cylLocalPoint = (PHCylPoint)localPoint;
  localVector = localv;
  calculateLocal();
  calculateLocalPlanes();

  globalPoint = globalp;
  cylGlobalPoint = (PHCylPoint)globalPoint;
  globalVector = globalv;
  calculateGlobal();
  calculateGlobalPlanes();
}

void
DchTrackCandidate::calculateLocalPoints()
{
  // Calculate the "PHPoints" using the floats which are
  // redundant with them.
  // convention is that local vector always points out radially
  // local point for the candidate
  cylLocalPoint = PHCylPoint(referenceRadius, localPhi, localZed);
  localPoint = (PHPoint)cylLocalPoint;
  PHVector local(sin(localBeta)*cos(localPhi - localAlpha), 
                 sin(localBeta)*sin(localPhi - localAlpha), 
		 cos(localBeta));
  localVector = local;
}

void
DchTrackCandidate::calculateLocal()
{
  // Calculate the "floats" using the "PHPoints" which are redundant with them.
  // Also copy the PHPoint into the PHCylPoint.
  localPhi = atan2(localPoint.getY(), localPoint.getX());
  if (localPhi < -Pi / 2.0) localPhi = localPhi + 2 * Pi;

  float tmp = atan2(localVector.getY(), localVector.getX());
  if (tmp < -Pi / 2) tmp = tmp + 2 * Pi;
  localAlpha = localPhi - tmp;

  if (localPhi > Pi / 2) localSlope = tan(localPhi - localAlpha - Pi); // if EAST Arm
  else                   localSlope = tan(localPhi - localAlpha);
  localIntercept = sin(localPhi) * referenceRadius -
                   localSlope * cos(localPhi) * referenceRadius;

  localZed = localPoint.getZ();
  localBeta = acos(localVector.getZ()/localVector.length());
}

void
DchTrackCandidate::calculateGlobalPoints()
{
  // Calculate the "PHPoints" using the floats which are
  // redundant with them.
  // convention is that local vector always points out radially

  cylGlobalPoint = PHCylPoint(referenceRadius, globalPhi, globalZed);
  globalPoint = (PHPoint)cylGlobalPoint;
  PHVector global(sin(globalBeta)*cos(globalPhi - globalAlpha), 
                 sin(globalBeta)*sin(globalPhi - globalAlpha), 
		 cos(globalBeta));
  globalVector = global;
}

void
DchTrackCandidate::calculateGlobal()
{
  globalPhi = atan2(globalPoint.getY(), globalPoint.getX());
  if (globalPhi < -Pi / 2.0)
    {
      globalPhi = globalPhi + 2 * Pi;
    }
  float tmp = atan2(globalVector.getY(), globalVector.getX());
  if (tmp < -Pi / 2)
    {
      tmp = tmp + 2 * Pi;
    }
  globalAlpha = globalPhi - tmp;

  globalZed = globalPoint.getZ();
  globalBeta = acos(globalVector.getZ()/globalVector.length());
}

void
DchTrackCandidate::setLocalPlane(short wireType, PHPlane plane)
{
  if (wireType == X1Wire || wireType == X2Wire)
    {  // if a X wire then .... (get the XY plane)
      localPlaneX = plane;
    }
  else if (wireType == UV1Wire || wireType == UV2Wire)
    {  //
      localPlaneUV = plane;
    }
}

PHPlane
DchTrackCandidate::getLocalPlane(short wireType)
{
  if (wireType == X1Wire || wireType == X2Wire)
    {  
      // if a X wire then .... (get the XY plane)
      return localPlaneX;
    }
  else if (wireType == UV1Wire || wireType == UV2Wire)
    {
      return localPlaneUV;
    }
  else
    {
      PHMessage("DchTrackCandidate::getPlane", PHWarning, "Wire Type Wrong ");
      PHPlane null;
      return null;
    }
}

void
DchTrackCandidate::setGlobalPlane(short wireType, PHPlane plane)
{
  if (wireType == X1Wire || wireType == X2Wire)
    {  
      // if a X wire then .... (get the XY plane)
      globalPlaneX = plane;
    }
  else if (wireType == UV1Wire || wireType == UV2Wire)
    {
      globalPlaneUV = plane;
    }
}

PHPlane
DchTrackCandidate::getGlobalPlane(short wireType)
{
  if (wireType == X1Wire || wireType == X2Wire)
    {  
      // if a X wire then .... (get the XY plane)
      return globalPlaneX;
    }
  else if (wireType == UV1Wire || wireType == UV2Wire)
    {  //
      return globalPlaneUV;
    }
  else
    {
      PHMessage("DchTrackCandidate::getPlane", PHWarning, "Wire Type Wrong ");
      PHPlane null;
      return null;
    }
}

void
DchTrackCandidate::calculateLocalPlanes()
{
  // Calculate the Local Coordinate planes using the LocalPoint
  // and the conventions:
  //      localPlaneX plane is defined to be at constant Z.
  //      localPlane UV is defined to be parallel to Z.
  // Calculate IN LOCAL COORDINATES the track-plane defined by the X
  // wires (in Local Coordinates)
  PHVector normalX(0, 0, 1);  //convention
  PHPlane localX(localPoint, normalX);
  localPlaneX = localX;
  PHVector normalUV( -localVector.getY(), localVector.getX(), 0); //Z==0 by convention.
  normalUV.normalize();

  PHPlane localUV(localPoint, normalUV);
  localPlaneUV = localUV;
}

void
DchTrackCandidate::calculateGlobalPlanes()
{
  // Calculate the Global Coorinate planes using the GlobalPoint
  // and the conventions:
  //      globalPlaneX plane is defined to be at constant Z.
  //      globalPlane UV is defined to be parallel to Z.
  // Calculate IN GLOBAL COORDINATES the track-plane defined by the X
  // wires (in Global Coordinates)
  PHVector normalX(0, 0, 1);  //convention
  PHPlane globalX(globalPoint, normalX);
  globalPlaneX = globalX;
  PHVector normalUV( -globalVector.getY(), globalVector.getX(), 0); //Z==0 by convention.
  normalUV.normalize();

  PHPlane globalUV(globalPoint, normalUV);
  globalPlaneUV = globalUV;
}

// for the candidate finder
PHBoolean
DchTrackCandidate::intersectUVwithXPlane(DchHitLine* hitline, PHPoint& intersection)
{
  // By using the basepoint and dir of the iht, we are automatically in
  // global coordinates for the stereo wire manipulation!
   PHPlane tmpPlane = getGlobalPlane(UV1Wire); // plane for UV wire

  PHBoolean flaggy = intersectionLinePlane((PHLine)(*hitline), tmpPlane, intersection);

  return flaggy;
}

// This fuction has been written to display the UV intersection
// points.  The idea is to display the UV intersection points with the
// track-plane defined by the X wire.  To make the display useful one
// need to rotate the track-plane(and all the hit on it) so that it
// coincide with the XY plane
//
//  It may be obsolete with the new stereo code, ut since its only a display
//  thing, it will wait :)
//                             TKH -- 1-29-2002
PHPoint
DchTrackCandidate::rotateAndTranslate(const PHPoint& point)
{
  PHPoint outPoint;
  outPoint = transformPoint(rotation, translation, point);

  return outPoint;
}

void
DchTrackCandidate::print()
{
  cout << "Arm :   " << arm << " Side: " << side << endl;
  cout << "LocalPoint:  " << localPoint << " LocalVector: " << localVector << endl;
  cout << "GlobalPoint:  " << globalPoint << " GlobalVector: " << globalVector << endl;
  cout << "Phi:    " << localPhi << " Alpha: " << localAlpha << endl;
  cout << "Beta:   " << localBeta << " Zed: " << localZed << endl;
  cout << "xHeight: " << xHough << " UV: " << uvHough << endl;
  cout << "X-hits: " << X->length() << " UV-hits: " << UV->length() << endl;
}

void
DchTrackCandidate::resetAssociated(short plane)
{
  associatedHit[plane] = -1;
  if (wireType[plane] == X1Wire || wireType[plane] == X2Wire)
    {
      associatedChi2[plane] = 10000.0;
    }
  else
    {
      associatedChi2[plane] = 10000.0;
    }
}

