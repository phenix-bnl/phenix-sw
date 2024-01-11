//----------------------------------------
// Created by Federica Messer/Ceretto
//----------------------------------------
#ifndef __DCHTRACKCANDIDATE_H
#define __DCHTRACKCANDIDATE_H

#include "DchDgoPar.h"
#include "DchAnaPar.h"
#include "DchHitLine.hh"
#include "DchPc1Hit.hh"

#include "PHFrame.h"
#include "PHMatrix.h"
#include "PHPlane.h"

#include "PHPointerList.h"

class DchTrackInfo;

class DchTrackCandidate { 

public:
  
  DchTrackCandidate();
  DchTrackCandidate(short arm, short side, float alpha, float phi, float slope, float refRad1);  
  DchTrackCandidate(short arm, short side, PHPoint globalPoint, PHVector globalVector);  
  virtual ~DchTrackCandidate(); 
  
protected:
  void calculateLocal();
  void calculateLocalPlanes();
  void calculateLocalPoints();
  void calculateGlobal();
  void calculateGlobalPlanes();
  void calculateGlobalPoints();
  
public: 
 
  void update(PHPoint localp, PHVector localv, PHPoint globalp, PHVector globalv); 
 
  void setTrackInfo(DchTrackInfo* track)          { trackInfo = track;}
  void setAssociatedHit(short pl, long hit)       { associatedHit[pl] = hit;}
  void setAssociatedChi2(short pl, float chi2)    { associatedChi2[pl] = chi2;}
  void setAssociatedPoint(short pl, PHPoint point){ associatedPoint[pl] = point;}
  void setGlobalPoint(PHPoint point)              { globalPoint = point;}
  void setLocalPoint(PHPoint point)               { localPoint = point;}
  void setLocalVector(PHVector vector)            { localVector = vector;}
  void setGlobalVector(PHVector vector)           { globalVector = vector;}
  void setLocalPlane(short wireType, PHPlane);
  void setGlobalPlane(short wireType,PHPlane);
  void setCylLocalPoint(PHCylPoint val)           { cylLocalPoint = val;}
  void setCylGlobalPoint(PHCylPoint val)          { cylGlobalPoint = val;}
  void setQuality(int q) {quality = q;}
  void setTrackId(int i) {trackId = i;}
  void setXHough(short val) {xHough = val;}
  void setUVHough(short val) {uvHough = val;}
  void setRotationAndTranslation(PHMatrix matrix, PHVector vect) { rotation = matrix; translation = vect;}
  void setGlobalBeta(float val) {globalBeta = val;}
  void setGlobalZed(float val) {globalZed = val;}
  void setLocalBeta(float val) {localBeta = val;}
  void setLocalZed(float val) {localZed = val;}
  void setChi2OfFit(float val) {chi2OfFit = val;}   
  void setNumberOfFittedHits(int i) {numberOfFittedHits = i;}   
  void setClosestApproachToBeamAxis(float v) {distanceToAxis = v;}

  int   getFinalPc1Id() {return finalPc1Id;}
  void  setFinalPc1Id(int val) { finalPc1Id = val;}
  short getXHough()      { return xHough;}
  short getUVHough()     { return uvHough;}
  int   getQuality()     { return quality;}
  int   getTrackId()     { return trackId;}
  float getLocalAlpha()  { return localAlpha;}
  float getGlobalAlpha() { return globalAlpha;}
  float getLocalPhi()    { return localPhi;}
  float getGlobalPhi()   { return globalPhi;}
  float getLocalBeta()   { return localBeta;}
  float getGlobalBeta()  { return globalBeta;}
  float getLocalZed()    { return localZed;}
  float getGlobalZed()   { return globalZed;}
  short getArm()         { return arm;}
  short getSide()        { return side;}
  float getChi2OfFit()   { return chi2OfFit;}   
  int   getNumberOfFittedHits() { return numberOfFittedHits;}   
  float getClosestApproachToBeamAxis() {return distanceToAxis;}

  void setTrackFrame(PHFrame val) {trackFrame = val;}

  DchTrackInfo* getTrackInfo() {return trackInfo;}
  long getAssociatedHit(short pl)              {return associatedHit[pl];}
  float getAssociatedChi2(short pl)            {return associatedChi2[pl];}
  PHPoint  getAssociatedPoint(short pl)        {return associatedPoint[pl];}  
  PHPoint  getLocalPoint()            {return localPoint;}
  PHPoint  getGlobalPoint()           {return globalPoint;}
  PHVector getLocalVector()           {return localVector;}
  PHVector getGlobalVector()          {return globalVector;}
  PHPlane  getLocalPlane(short wireType);
  PHPlane  getGlobalPlane(short wireType);
  PHPlane  getPlane(short wireType)   { return getLocalPlane(wireType);} // temporarry
  PHCylPoint getCylLocalPoint()       { return cylLocalPoint;}
  PHCylPoint getCylGlobalPoint()      { return cylGlobalPoint;}
  PHFrame    getTrackFrame() {return trackFrame;}
  
  PHPoint   rotateAndTranslate(const PHPoint&);
  PHBoolean intersectUVwithXPlane(DchHitLine* hitLine, PHPoint& intersection);
  void resetAssociated(short plane);
  int numberOfHits(); 
  void print();
 
public:
  
  PHPointerList<DchHitLine>* closestX;    // limited to one hit per plane 
  PHPointerList<DchHitLine>* closestUV;   // distance to candidate in associatedChi2
  PHPointerList<DchHitLine>* X;    // all hits in road set as parameter in DchHitAssociator
  PHPointerList<DchHitLine>* UV;
  PHPointerList<DchHitLine>* uv1;  // all hits that intersect UV plane
  PHPointerList<DchHitLine>* uv2;
  PHPointerList<DchHitLine>* x1;   // all hits in +/- 1 cell
  PHPointerList<DchHitLine>* x2;
  PHPointerList<DchPc1Hit>*  pc1;  // PC1 hits close to UV-plane 

private:

  int      finalPc1Id;   
  PHPlane  localPlaneX;  
  PHPlane  globalPlaneX;
  
  PHPlane  localPlaneUV;  // track-plane defined by the X wires
  PHPlane  globalPlaneUV; // track-plane defined by the X wires
  
  PHPoint    globalPoint;
  PHVector   globalVector;
  PHPoint    localPoint; // local coordinates
  PHVector   localVector;
  
  PHCylPoint cylLocalPoint;
  PHCylPoint cylGlobalPoint;
  PHFrame    trackFrame;

  PHMatrix  rotation;
  PHVector  translation;
  
  float globalPhi;
  float localPhi;
  float localAlpha;
  float globalAlpha;

  float localZed;
  float localBeta;
  float globalZed;
  float globalBeta;

  short xHough;
  short uvHough;
  float localIntercept;
  float localSlope;
  short arm;
  short side;

  int trackId;
  int quality;
  DchTrackInfo* trackInfo;
  long  associatedHit[numberOfPlanes]; // the list of point id's as function of plane
  float associatedChi2[numberOfPlanes]; // this is the associated chi2 
  // distance of the point from the line corresponding to the candidate
  PHPoint associatedPoint[numberOfPlanes]; // this is the set of points
  // corresponding to each of the hits through which the candidate
  // passed.  For the x wires this is trivially the end point of the
  // wire and the z-position at which they crossed the wire (not
  // necessarily filled right now).  For
  // the UV wires, it's the xyz point where they cross the plane.
  
  int   numberOfFittedHits;
  float chi2OfFit;
  float distanceToAxis;

}; 

#endif /* __DCHTRACKCANDIDATE_H */ 
