#ifndef __DCHCANDIDATE_C__
#define __DCHCANDIDATE_C__
// Source file of the Class: DchCandidate 
// 
// Created by: Federica Ceretto
 
#include "DchCandidate.hh" 
#include "PHGeometry.h"
#include <iostream>

using namespace std;

DchCandidate::DchCandidate(PHPoint point, PHVector vector)
{
  inGlobal = False;
  
  phi       = 0;
  alpha     = 0;
  sinPhi    = 0;
    
  beta      = 0;
  zed       = 0;
  xHeight   = 0;
  uvHeight  = 0;
  intercept = 0;
  slope     = 0;
  trackInfo = 0;
  quality = 1;
  trackId = -1;

  pointLocal = point;
  vectorLocal = vector;
  
  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i]=-1;
    if (wireType[i]==X1Wire || wireType[i]==X2Wire) {
      associatedChi2[i]=initXChi*initXChi;  
    }else{
      associatedChi2[i]=initUVChi;
    }
  }
  uv1 = new PHPointerList<DchHitLine>(10);
  uv2 = new PHPointerList<DchHitLine>(10);
  calculateLocalPlanes();   
}

DchCandidate::DchCandidate(float sP, float a, float p, float s, float inter, short xh, short ar, short si)
{
  inGlobal = False;
  sinPhi = sP;
  alpha  = a;
  phi    = p;
  slope  = s;
  intercept = inter;
  xHeight   = xh;
  arm       = ar;
  side      = si;
  trackInfo = 0; 
  quality = 1;
  trackId = -1;
  
  beta = 0;
  zed = 0;

  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i] = -1;
    if (wireType[i]==X1Wire || wireType[i]==X2Wire)
      associatedChi2[i]=initXChi*initXChi;  
    else
      associatedChi2[i]=initUVChi;
  }

  uv1 = new PHPointerList<DchHitLine>(10);
  uv2 = new PHPointerList<DchHitLine>(10);

}

DchCandidate::DchCandidate(float sP, float a, float p, float s, short xh, short ar, short si)
{
  inGlobal = False;
  sinPhi = sP;
  alpha  = a;
  phi    = p;
  slope  = s;
  xHeight = xh;
  arm     = ar;
  side    = si;
  
  beta = 0;
  zed = 0;
  trackInfo = 0;
  quality = 1;
  trackId = -1;
  
  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i]=-1;
    if (wireType[i]==X1Wire || wireType[i]==X2Wire){
	associatedChi2[i]=initXChi*initXChi;  
    }else{
	associatedChi2[i]=initUVChi;
    }
  }
  uv1 = new PHPointerList<DchHitLine>(10);
  uv2 = new PHPointerList<DchHitLine>(10);

}

DchCandidate::DchCandidate() 
{
  inGlobal = False;
  phi       = 0;
  alpha     = 0;
  sinPhi    = 0;
    
  beta      = 0;
  zed       = 0;
  xHeight   = 0;
  uvHeight  = 0;
  intercept = 0;
  slope     = 0;
  trackInfo = 0;
  quality = 1;
  trackId = -1;
  
  for (int i=0;i<numberOfPlanes;i++){
    associatedHit[i]=-1;
    if (wireType[i]==X1Wire || wireType[i]==X2Wire) {
      associatedChi2[i]=initXChi*initXChi;  
    }else{
      associatedChi2[i]=initUVChi;
    }
  }
  uv1 = new PHPointerList<DchHitLine>(10);
  uv2 = new PHPointerList<DchHitLine>(10);
    
}

DchCandidate::~DchCandidate() 
{
  uv1->clear();
  uv2->clear();

  delete uv1;
  delete uv2;

}

int DchCandidate::numberOfHits()
{
  int total=0;
  for (int i=0;i<numberOfPlanes;i++){
    if(associatedHit[i]> -1) {  // if hit id is bigger that -1 
      total++;
    }
  }
  return total;
}

void DchCandidate::calculateLocal()
{
  phi = atan2(pointLocal.getY(), pointLocal.getX());
  alpha = phi - atan2(vectorLocal.getY(),vectorLocal.getX());
  sinPhi = sin(phi);

  if (phi > Pi/2) {  // if EAST Arm 
    slope = tan(phi - alpha - Pi);
  }else {
    slope  = tan(phi -alpha);
  }
  intercept = sinPhi*referenceRadius - slope*cos(phi)*referenceRadius;

}

void DchCandidate::calculateGlobal()
{
  phi = atan2(Point.getY(), Point.getX());
  alpha = phi - atan2(Vector.getY(),Vector.getX());
  sinPhi = sin(phi);

  if (phi > Pi/2) {  // if EAST Arm 
    slope = tan(phi - alpha - Pi);
  }else {
    slope  = tan(phi -alpha);
  }

  intercept = 0; // check 
}

void DchCandidate::setLocalPlane(short wireType, PHPlane plane)
{
  if (wireType == X1Wire || wireType == X2Wire) {  // if a X wire then .... (get the XY plane)
    localPlaneX = plane;
  }else if (wireType == UV1Wire || wireType == UV2Wire) {  //
    localPlaneUV = plane;
  }
}

PHPlane DchCandidate::getLocalPlane(short wireType)
{
  if (wireType == X1Wire || wireType == X2Wire) {  // if a X wire then .... (get the XY plane)
    return localPlaneX;
  }else if (wireType == UV1Wire || wireType == UV2Wire) {  //
    return localPlaneUV;
  }else {
    PHMessage("DchCandidate::getPlane",PHWarning,"Wire Type Wrong ");
    PHPlane null;
    return null;
  }
}

void DchCandidate::setGlobalPlane(short wireType,PHPlane plane)
{
  if (wireType == X1Wire || wireType == X2Wire) {  // if a X wire then .... (get the XY plane)
    globalPlaneX = plane;
  }else if (wireType == UV1Wire || wireType == UV2Wire) {  //
    globalPlaneUV = plane;
  }
}
PHPlane DchCandidate::getGlobalPlane(short wireType)
{
  if (wireType == X1Wire || wireType == X2Wire) {  // if a X wire then .... (get the XY plane)
    return globalPlaneX;
  }else if (wireType == UV1Wire || wireType == UV2Wire) {  //
    return globalPlaneUV;
  }else {
    PHMessage("DchCandidate::getPlane",PHWarning,"Wire Type Wrong ");
    PHPlane null;
    return null;
  }
}

void DchCandidate::calculateLocalPlanes()
{
  //-------------------------------------------------------------------------------------------------
  // it calculate IN LOCAL COORDINATES the track-plane defined by the X wires (in Local Coordinates)
  //--------------------------------------------------------------------------------------------------

  // These are local variables of calculateLocalPlanes() which are never used
  // I commented them out, STJL

  //  PHVector normalX(0,0,1);
  //  PHPlane localPlaneX(pointLocal, normalX);
  
  //  PHVector normalUV(-vectorLocal.getX(),vectorLocal.getY(),0);
  //  PHPlane localPlaneUV(pointLocal,normalUV); 

}

// For the candidate finder
PHBoolean 
DchCandidate::intersectXPlane(DchHitLine* hitline, PHPoint& intersection)
{
  // plane for UV wire
  PHPlane tmpPlane = getGlobalPlane(UV1Wire); 
  PHPoint tmp;
  PHBoolean flaggy = PHGeometry::intersectionLinePlane((PHLine)(*hitline),
						       tmpPlane,tmp);
  
  if (flaggy) 
    {
      intersection = tmp;
    }

  return flaggy;
}

PHPoint 
DchCandidate::intersectXPlane(const PHLine& uvline)
{
  //
  // intersect a line (hitline) with the plane defined by the x hits.
  //

  // plane for UV wire
  PHPlane tmpPlane = getPlane(UV1Wire); 
  PHPoint tmp,transi;
  PHBoolean flaggy = PHGeometry::intersectionLinePlane(uvline,tmpPlane,tmp);
 
  if (flaggy) 
    {
      transi = translateAndRotate(tmp);
    }
  
  return transi;
}

PHPoint DchCandidate::translateAndRotate(const PHPoint& point)
// This fuction has been written to display the UV intersection
// points.  The idea is to display the UV intersection points with the
// track-plane defined by the X wire.  To make the display usefull one
// need to rotate the track-plane(and all the hit on it) so that it
// coincide with the XY plane
{
  // To BE DEBUGGED
  cout << "translateAndRotate::TO BE DEBUGGED "<< endl;
 
  PHPoint null;
  PHVector Xaxis(1.,0,0);
  PHVector Yaxis(0,1.,0);

  PHFrame XY(null,Xaxis,Yaxis);

  PHPlane trackPlane = getPlane(UV1Wire);
  PHPoint origin = trackPlane.getOrigin();
  PHVector axis1 = trackPlane.getNormal();

  PHVector axis2 = axis1.orthogonal();
  PHVector axis3 = axis1.cross(axis2);
  PHFrame trackFrame(origin,axis2,axis3);

  PHPoint outPoint = PHGeometry::transformPoint(XY, point, trackFrame);
 
  return outPoint;
}

void DchCandidate::print()
{
  cout << "ARm :   " << arm << " Side: " << side << endl;
  cout << "LocalPoint:  "<< pointLocal << " LocalVector: " << vectorLocal << endl;
  cout << "GlobalPoint:  "<< Point << " GlobalVector: " << Vector << endl;
  cout << "Phi:    " << phi << " Alpha: " << alpha << " SinPhi: " << sinPhi << endl;
  cout << "Beta:   " << beta << " Zed: " << zed << endl;
  cout << "Slope:  " << slope << " Intercept: " << intercept << endl;
  cout << "xHeight: " << xHeight << " UV: " << uvHeight << endl;
 
}
void DchCandidate::resetAssociated(short plane)
{
  associatedHit[plane]=-1;
  if (wireType[plane]==X1Wire || wireType[plane]==X2Wire) {
    associatedChi2[plane]=initXChi*initXChi;
  }else {
    associatedChi2[plane]=initUVChi;  
  }
}


#endif /* __DCHCANDIDATE_C__ */
