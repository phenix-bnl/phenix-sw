//Purpose: Candidate definition as returned from hough transform.
//--------------------------------------------------- 
// PHENIX Drift Chamber Software: Stony Brook Mon Jul  6 15:32:11 1998    
// 
// Implementation of the Class: DchCandidate 
// 
// Created by: Federica Ceretto  at Mon Jul  6 15:32:11 1998
// 
// Description:  This is the class for a drift chamber candidate.
// Each candidate has corresponding values determined from the hough
// transform (phi, alpha, beta, zed) as well as the associated height
// of the hough transform at that point.  Hits are associated with the 
// candidate.
// 
// Last update: Mon Jul  6 15:32:11 1998
//--------------------------------------------------- 
#ifndef __DCHCANDIDATE_H
#define __DCHCANDIDATE_H

#include "phool.h"
#include "DchAnaPar.h"
#include "DchHitLine.hh"
#include "PHPoint.h"
#include "PHVector.h"
#include "PHPlane.h"
#include "PHLine.h"
#include "PHPointerList.h"
#include "PHCylPoint.h"

class DchTrackInfo;

class DchCandidate { 

public:
  
  // Default constructor
  DchCandidate(); 
 
  DchCandidate(PHPoint localPoint, PHVector localVector);

  DchCandidate(float sP, float a, float p, float s,
	       short xh, short ar, short si);
  
  DchCandidate(float sP, float a, float p, float s, float inter,
	       short xh, short ar, short si);
  
  virtual ~DchCandidate(); 

protected:
  void calculateLocal();
  void calculateGlobal();
  
public: 
  float getPhi()            { return phi;}
  float getAlpha()          { return alpha;}
  float getSinPhi()         { return sinPhi;}
  short getXHeight()        { return xHeight;}
  short getUVHeight()       { return uvHeight;}
  float getIntercept()      { return intercept;}
  float getSlope()          { return slope;}
  
  float getBeta()           { return beta;}
  float getZed()            { return zed;}

  short getArm()            { return arm;}
  short getSide()           { return side;}

  void setPhi(float val)            { phi = val;}
  void setAlpha(float val)          { alpha = val;}
  void setSinPhi(float val)         { sinPhi = val;}
  void setXHeight(short val)        { xHeight = val;}
  void setUVHeight(short val)       { uvHeight = val;}
  void setIntercept(float val)      { intercept = val;}
  void setSlope(float val)          { slope = val;}
  void setBeta(float val)           { beta = val;}
  void setZed(float val)            { zed  = val;}
  void setArm(short val)            { arm = val;}
  void setSide(short val)           { side = val;}

  void setTrackInfo(DchTrackInfo* track) { trackInfo = track;}
  DchTrackInfo* getTrackInfo() {return trackInfo;}
  
  void setAssociatedHit(short pl, long hit) {associatedHit[pl] = hit;}
  long getAssociatedHit(short pl)           {return associatedHit[pl];}

  void setAssociatedChi2(short pl, float chi2) {associatedChi2[pl] = chi2;}
  float getAssociatedChi2(short pl)            {return associatedChi2[pl];}
  
  void setAssociatedPoint(short pl, PHPoint point){associatedPoint[pl] = point;}
  PHPoint  getAssociatedPoint(short pl)           {return associatedPoint[pl];}

  void setPoint(PHPoint point) {Point = point;}
  void setLocalPoint(PHPoint point) {pointLocal = point;}
  
  PHPoint getLocalPoint() {return pointLocal;}
  PHPoint getPoint()           {return Point;}

  void setLocalVector(PHVector vector) {vectorLocal = vector;}
  void setVector(PHVector vector) {Vector = vector;}
  PHVector getLocalVector()             {return vectorLocal;}
  PHVector getVector()             {return Vector;}

  PHPoint translateAndRotate(const PHPoint&);
  PHPoint intersectXPlane(const PHLine&);
  PHBoolean intersectXPlane(DchHitLine* hitLine, PHPoint& intersection);

  PHPlane getLocalPlane(short wireType);
  PHPlane getGlobalPlane(short wireType);

  void    setLocalPlane(short wireType, PHPlane);
  void    setGlobalPlane(short wireType,PHPlane);

  PHPlane getPlane(short wireType) { return getLocalPlane(wireType);} // temporarry
  
  void resetAssociated(short plane);

  PHCylPoint getCylLocalPoint() { return cylLocalPoint;}
  PHCylPoint getCylGlobalPoint() { return cylGlobalPoint;}

  void setCylLocalPoint(PHCylPoint val) { cylLocalPoint = val;}
  void setCylGlobalPoint(PHCylPoint val) { cylGlobalPoint = val;}
  
  int numberOfHits();
  int getQuality() { return quality;}

  int getTrackId() { return trackId;}
  void setTrackId(int i) {trackId = i;}
  
  void print();
  void calculateLocalPlanes();

public:
  
  PHPointerList<DchHitLine>* uv1;
  PHPointerList<DchHitLine>* uv2;
  PHPointerList<DchHitLine>* x1;
  PHPointerList<DchHitLine>* x2;

private:
  PHBoolean inGlobal;
   
  PHPlane  localPlaneX;  
  PHPlane  globalPlaneX;
  
  PHPlane  localPlaneUV;  // track-plane defined by the X wires
  PHPlane  globalPlaneUV; // track-plane defined by the X wires
  
  PHPoint  Point;
  PHVector Vector;
  PHPoint  pointLocal; // local coordinates
  PHVector vectorLocal;
  PHCylPoint cylLocalPoint;
  PHCylPoint cylGlobalPoint;
 
  float phi;
  float alpha;
  float sinPhi;
  float  beta;
  float  zed;
    
  short xHeight;
  short uvHeight;
  float intercept;
  float slope;
  short arm;
  short side;

  int trackId;
  int quality;
  DchTrackInfo* trackInfo;
  long associatedHit[numberOfPlanes]; // the list of point id's as function of plane
  float associatedChi2[numberOfPlanes]; // this is the associated chi2 
  // distance of the point from the line corresponding to the candidate
  PHPoint associatedPoint[numberOfPlanes]; // this is the set of points
  // corresponding to each of the hits through which the candidate
  // passed.  For the x wires this is trivially the end point of the
  // wire and the z-position at which they crossed the wire (not
  // necessarily filled right now).  For
  // the UV wires, it's the xyz point where they cross the plane.
  
}; 

#endif /* DCHCANDIDATE_H */ 
