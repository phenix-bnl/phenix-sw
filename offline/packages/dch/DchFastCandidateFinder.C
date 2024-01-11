#include "DchFastCandidateFinder.hh"
#include "DchHitLineLists.hh"
#include "PHDchHistogrammer.hh"
#include "dDchTracksExtWrapper.h"
#include "DchTrack.h"
#include "DchHitLineTable.hh"

#include <dPadCluster.h>
#include <PHDchGeometryObject.h>
#include <PHGeometry.h>

#include <TNtuple.h>

#include <gsl/gsl_fit.h>

#include <cmath>
#include <iostream>

using namespace std;
using namespace PHGeometry;

// ***************************************************************************
// this constructor takes the staf hit tables and creates the list of dchlines
// ***************************************************************************

DchFastCandidateFinder::DchFastCandidateFinder(PHDchGeometryObject *geo,
					       DchHitLineLists* list, 
					       TABLE_HEAD_ST *dDchHit_h,
					       DDCHHIT_ST *dDchHit,
					       DchHitLineTable* hitLineTable)
{
  dchTracksExt=0;

  HoughDimensionX = 0;
  HoughDimensionY = 0;

  histogrammer = 0;
  geometry = geo;
  hitList = list;
  candidates = 0;
  long numberOfHits = dDchHit_h->nok;
  int id,idmirror;
  short arm, side, plane, cell;
  float time, width, distance;
  DchHitLine *dchHitLine;
  PHPoint base;
  PHVector dir;
  PHCylPoint cylLocalPoint;

  cylinderNorthPoint[WEST] = geometry->getCenterWestArmOnNorthSide();
  cylinderNorthPoint[EAST] = geometry->getCenterEastArmOnNorthSide();
  cylinderCenter[WEST]     = geometry->getCenterWestArm();
  cylinderCenter[EAST]     = geometry->getCenterEastArm();
  cylinderAxis[WEST]       = geometry->getAxisWestArm();
  cylinderAxis[EAST]       = geometry->getAxisEastArm();

  // If the event is REALLY noisy than the number of hits in the DC
  // is HUGE (>2^15) and the table of hits runs out of space
  tooManyHits = False;
  if(numberOfHits>32768)
    {
      numberOfHits = 0;
      tooManyHits = True;
    }

  for (int  i =0; i < numberOfHits; i++){
      id       = dDchHit[i].id;
      idmirror = dDchHit[i].idmirror;
      cell     = dDchHit[i].cell;
      plane    = dDchHit[i].plane;
      arm      = dDchHit[i].arm;
      side     = dDchHit[i].side;
      time     = dDchHit[i].time1;
      width    = dDchHit[i].width;
      distance = dDchHit[i].distance;
      base.setX(dDchHit[i].xyz[0]);  // This is in global coordinates TKH 1-29-2002
      base.setY(dDchHit[i].xyz[1]);
      base.setZ(dDchHit[i].xyz[2]);
      dir.setX(dDchHit[i].vxyz[0]);  // This is in global coordinates TKH 1-29-2002
      dir.setY(dDchHit[i].vxyz[1]);
      dir.setZ(dDchHit[i].vxyz[2]);

      PHPoint lp = geometry->rotateAndTranslateInverse(arm,base);   
      cylLocalPoint = (PHCylPoint)(lp);   // this is in r,phi,Z (Z now preserved...TKH)

      dchHitLine = new DchHitLine(id,idmirror, arm, side, plane, cell, 
				  time, width, distance, base, dir, cylLocalPoint);
      hitList->append(dchHitLine);
      }
}


// ******************************************************************************
// Find out if number of Hits is too long for DDCHHIT_ST, in that case we want
// to send a ABORTEVENT to DchReco
// ******************************************************************************
PHBoolean DchFastCandidateFinder::hasTooManyHits()
{
  if(tooManyHits) return True;
  return False;
}

// ******************************************************************************
// Find out if number of Tracks is larger than dDchTrack table size, in that 
// case we want to send a ABORTEVENT to DchReco
// ******************************************************************************
PHBoolean DchFastCandidateFinder::hasTooManyTracks()
{
  if(tooManyTracks) return True;
  return False;
}


// ******************************************************************************
// Calculation of phi and sin(alpha) of the intersection of line between two hits
// and reference cylinder.
// Should be rechecked for X only case !
// ******************************************************************************
void 
DchFastCandidateFinder::phiAndAlpha(DchHitLine* line1, 
				    DchHitLine* line2, 
				    double& Alpha,
				    double& sinPhi)
{
  double r1, r2, phi1, phi2;
  double d, deltaPhi, sinBeta, sinAlpha, Gamma;
  PHPoint p1, p2;

  r1 = line1->getLocalRadius();
  r2 = line2->getLocalRadius();
  phi1 = line1->getLocalPhi();
  phi2 = line2->getLocalPhi();  
  p1 = line1->getBasepoint();
  p2 = line2->getBasepoint();

//  swap notations if r1>r2
  if (r1 > r2) {
    swap(r1, r2);
    swap(phi1, phi2);
  }

  deltaPhi = phi1 - phi2;
  d = distancePointToPoint(p1, p2);

//  sine theorem for alpha and beta
  sinBeta = sin(deltaPhi) * r2 / d;
  sinAlpha = sinBeta * r1 / pReferenceRadius;
 
  // SB 02/22/01   Correction for the case when ther is no intersection with ref. plane. 
  if (fabs(sinBeta)<1. && fabs(sinAlpha)<1.) 
    {
      Alpha = asin(sinAlpha);
      Gamma = (fabs(asin(sinBeta)) - fabs(Alpha));
    }
  else 
    {
      Gamma = 0.;
      Alpha = pMaxAlpha + 1.;  // moving the point outside the hough space boundaries  
    }

  if (phi1 > phi2)
    {
      sinPhi = phi1 - Gamma;
    }
  else
    {
      sinPhi = phi1 + Gamma;
    }
}

// ***********************************************************************
// Calculation of Z and beta for intersection of line, connecting 2 points 
// with reference cylinder
// ***********************************************************************

void DchFastCandidateFinder::betaAndZed(const PHPoint& p1, const PHPoint& p2, 
				   double& beta, double& zed)
{
  double m;
  // Calculating the inverse slope might be much more stable near
  // beta = pi/2
  m = (p2.getY() - p1.getY()) / (p2.getX() - p1.getX());

  // Correct sign error in zed
  zed = (p1.getY() * p2.getX() -  p1.getX() * p2.getY()) / 
    (p1.getY() - p2.getY());
  beta = atan(m);

  //  Beta must not change sign at 90 degrees
  if (beta < 0) 
    {
      beta = Pi + beta;
    }
}

// ******************************************************************

void DchFastCandidateFinder::resizeHoughArray(short x, short y)
{
  // one extra bin to either side to let the search algorithm look at
  // all surrounding bins of a candidate
  HoughArray.resize(-1,x+1,-1,y+1);

  HoughDimensionX = x;
  HoughDimensionY = y;

}

// ******************************************************************

void DchFastCandidateFinder::zeroHoughArray(short ix, short iy)
{
  // chp the array knows how large it is, this clear it!
  HoughArray = 0;
  SparseHoughLookup.clear();
}

PHBoolean DchFastCandidateFinder::surviveBetaAndZedCorrelation()
{
  // This function is used to identify the right peak in the UV Hough
  // Transformation!  It might use the Vertex constraint or the PC hit
  // locations!  For now, it is very flexible!

  return True;
}

short 
DchFastCandidateFinder::integralAndCogIfLocalMaximum(short ix, short iy, 
						     float& xcog, float& ycog)
{
  // Examines the eight bins surrounding (ix, iy) to decide if we're
  // looking at a local maximum.  If so, sums the contents of all nine
  // bins and determines a weighted average of their bin coordinates.
  // In case of peak splitting between 2 neighbouring bins, picks only one bin
  // returns position of peak centroid in 3x3 array aroun the local maximum

  int thisValue, integral;
  float weightedX, weightedY;
  
  thisValue = HoughArray[ix][iy];
  if (thisValue < houghThresholdOnCell) 
    {
      return 0;
    }

  if ((HoughArray[ix-1][iy-1] >= thisValue) ||
      (HoughArray[ix  ][iy-1] >= thisValue) ||
      (HoughArray[ix+1][iy-1] >= thisValue) ||
      (HoughArray[ix-1][iy  ] >= thisValue) ||
      (HoughArray[ix+1][iy  ] >  thisValue) ||
      (HoughArray[ix-1][iy+1] >  thisValue) ||
      (HoughArray[ix  ][iy+1] >  thisValue) ||
      (HoughArray[ix+1][iy+1] >  thisValue))
    {
      // OK, so this isn't a local maximum ... 
      return 0;
    }

  integral =  
    HoughArray[ix-1][iy+1] + HoughArray[ix  ][iy+1] + HoughArray[ix+1][iy+1] +
    HoughArray[ix-1][iy  ] +       thisValue        + HoughArray[ix+1][iy  ] +
    HoughArray[ix-1][iy-1] + HoughArray[ix  ][iy-1] + HoughArray[ix+1][iy-1];

  if (integral < houghThresholdOnMask) 
    {
      return 0; 
    }

  weightedX = 
    (ix - 1) * (HoughArray[ix-1][iy-1] +  
		HoughArray[ix-1][iy  ] +  
		HoughArray[ix-1][iy+1]) +
    ix * (HoughArray[ix  ][iy-1] +  
	  thisValue +
	  HoughArray[ix  ][iy+1]) +
    (ix + 1) * (HoughArray[ix+1][iy-1] +  
		HoughArray[ix+1][iy  ] +  
		HoughArray[ix+1][iy+1]);
  weightedY = 
    (iy - 1) * (HoughArray[ix-1][iy-1] +  
		HoughArray[ix  ][iy-1] +  
		HoughArray[ix+1][iy-1]) +
    iy  * (HoughArray[ix-1][iy  ] +  
	   thisValue +  
	   HoughArray[ix+1][iy  ]) +
    (iy + 1) * (HoughArray[ix-1][iy+1] +  
		HoughArray[ix  ][iy+1] +  
		HoughArray[ix+1][iy+1]);

  // Return coordinates of the weighted center of the group of bins
  // (0.5 is binning correction)
  xcog = weightedX / integral + 0.5;
  ycog = weightedY / integral + 0.5;

  return integral; 
}

// ************************************************************
// Main routine obtaining PC1-UV information for all candidates
// ************************************************************

PHBoolean DchFastCandidateFinder::getPc1UVCandidates()
{
  // track candidate
  DchTrackCandidate* mycandidate; 
  int lengthCandidates = candidates->length();
  PHPlane   planeUV;

  // Dch hits
  DchHitLine *hit;
  PHPointerList<DchHitLine> *uv1candi;
  PHPointerList<DchHitLine> *uv2candi;

  // PC1 hits
  DchPc1Hit* phit;
  PHPointerList<DchPc1Hit> *pc1candi;

  // 
  // dimensions of search array
  double beta,zed;
  int betaIndex, zedIndex;
  resizeHoughArray(pNumberOfBetaBins, pNumberOfZedBins); // resize if necessary
  houghThresholdOnCell = 2;
  houghThresholdOnMask = pHoughThresholdOnUVMaskWithPc1Hit;

  // loop over all candidates
  for (int c=lengthCandidates-1; c>=0; c--){
    //
    // get candidate information
    // 
    mycandidate = (*candidates)[c];

    if (mycandidate->getQuality()>3) continue;  // only if UV was not yet reconstructed

    uv1candi = mycandidate->uv1;    // get the pointer to the list
    uv2candi = mycandidate->uv2;    // get the pointer to the list
    pc1candi = mycandidate->pc1;    // get the pointer to the list

    if (pc1candi->length()<1) continue;        // only if Pc1 hit associated
    if ( (int) (uv2candi->length()+uv1candi->length() )< pHoughThresholdOnUVMaskWithPc1Hit) continue;
                                                // sufficient # of UV hits
    // fill feature space
    zeroHoughArray(HoughDimensionX, HoughDimensionY); 

    PHPoint inter;
    PHPoint pc1inter;
    // Pc1 - UV1 first
    for (unsigned int i=0 ; i< pc1candi->length(); i++){  
      phit = (*mycandidate->pc1)[i]; 
      pc1inter = mycandidate->rotateAndTranslate(*phit);
      phit->setLocalIntersectionPoint(pc1inter);
      for (unsigned int i2=0 ; i2< uv1candi->length(); i2++){   
	hit = (*mycandidate->uv1)[i2]; 
	int cid = hit->getAssociatedCandidate(); 
	inter = hit->getLocalIntersectionPoint();
	// if hit was already assigned to another candidate
        // recalculate intersection point with X plane in local system
	if (cid != c) {
	  hit->setAssociatedCandidate(c);
	  PHPoint transInter;
	  if (mycandidate->intersectUVwithXPlane(hit,transInter)) {
	    inter = mycandidate->rotateAndTranslate(transInter);
	    hit->setLocalIntersectionPoint(inter);
	    hit->setGlobalIntersectionPoint(transInter);
	  }
	}
	// for all PC1 hits calculate Beta and Zed with all UV1 hits
	// and if result lies within hough space boundaries - fills HoughArray
	betaAndZed(pc1inter,inter,beta,zed);
	betaIndex  = int((beta - pMinBeta)*pNumberOfBetaBins/(pMaxBeta - pMinBeta)); 
	zedIndex = int((zed - pMinZed)*pNumberOfZedBins/(pMaxZed - pMinZed)); 
	if (zed>pMinZed && zed<pMaxZed && beta<pMaxBeta && beta>pMinBeta) 
	  {
	    pair<int,int> Position(betaIndex,zedIndex);
	    SparseHoughLookup.insert(Position);
	    HoughArray[betaIndex][zedIndex]++;
	  }
      }    
      // Pc1 - UV2 
      for (unsigned int i=0 ; i< uv2candi->length(); i++){   
	hit = (*mycandidate->uv2)[i]; 
	int cid = hit->getAssociatedCandidate(); 
	inter = hit->getLocalIntersectionPoint();
	if (cid != c) {
	  hit->setAssociatedCandidate(c);
	  PHPoint transInter;
	  if (mycandidate->intersectUVwithXPlane(hit,transInter)) {
	    inter = mycandidate->rotateAndTranslate(transInter);
	    hit->setLocalIntersectionPoint(inter);
	    hit->setGlobalIntersectionPoint(transInter);
	  }
	}
	betaAndZed(pc1inter,inter,beta,zed);
	betaIndex  = int((beta - pMinBeta)*pNumberOfBetaBins/(pMaxBeta - pMinBeta)); 
	zedIndex = int((zed - pMinZed)*pNumberOfZedBins/(pMaxZed - pMinZed)); 
	if (zed>pMinZed && zed<pMaxZed && beta<pMaxBeta && beta>pMinBeta) {
	  HoughArray[betaIndex][zedIndex]++;
	}
      }
    } 
    // search for maximum

    // first for match with PC1 point

    //  if candidate have UV information - update quality 
    if (searchUVCandidates(mycandidate)) {
      int tmp = mycandidate->getQuality();
      mycandidate->setQuality(12|tmp);
    }    
  } // candidate loop

  return True;
}

// *****************************************************************
// Main routine obtaining vertex-PC1 information for all candidates
// *****************************************************************

PHBoolean DchFastCandidateFinder::getVertexUVCandidates()
{
  // This routine has been rewritten to use a purely projective
  // selection criterion for UV hits.  It runs in global coordinates!
  // TKH-- 11-30-2001

  // track candidate
  DchTrackCandidate* mycandidate; 
  int lengthCandidates = candidates->length();
  int pc1quality=0, UVquality=0;
  
  // Dch hits
  DchHitLine *hit;
  PHPointerList<DchHitLine> *uv1candi;
  PHPointerList<DchHitLine> *uv2candi;
  
  // PC1 hits
  DchPc1Hit* phit;
  PHPointerList<DchPc1Hit> *pc1candi;
  
  // Select out the necessary vertex information:
  float vz = vertex.getZ();
  
  //This is the ideal radius of the u11 wire.
  float u11r=214.85; 
  float alpha=0;
  float finalZ=0, finalR=250;

  // loop over all candidates
  for (int c=lengthCandidates-1; c>=0; c--){
    
    finalZ=0;
    finalR=250;
    mycandidate = (*candidates)[c];
    alpha = mycandidate->getLocalAlpha();
    if (mycandidate->getQuality()>3) continue;  // only if UV was not yet reconstructed
    
    uv1candi = mycandidate->uv1;    // get the pointer to the list
    uv2candi = mycandidate->uv2;    // get the pointer to the list
    pc1candi = mycandidate->pc1;    // get the pointer to the list
    pc1quality=0;
    UVquality=0;
    if (pc1candi->length() == 0) continue;     // No PC1 hit available...exit
    if (pc1candi->length() >  1) pc1quality=1; // OK quality:        PC1 there but not unique
    if (pc1candi->length() == 1) pc1quality=2; // Excellent quality: PC1 there and unique.
    if (pc1candi->length() >  0) {             // Fill the answer with the "first choice".
      phit = (*mycandidate->pc1)[0];           // If UV resolves this choice, then the values here
      mycandidate->setFinalPc1Id(phit->getId()); // will be overwritten...otherwise quality will
      float px = phit->getX();                   // indicate choice is bad.      TKH JJ 3-11-2002
      float py = phit->getY();
      float pz = phit->getZ();
      float pr = sqrt(px*px + py*py);
      finalZ=pz;
      finalR=pr;
    }
    
    // Search the pc1 hits for the one which is maximally confirmed...
    int maxNumberOfConfirms=0;
    for (unsigned int ipad=0 ; ipad< pc1candi->length(); ipad++){  
      phit = (*mycandidate->pc1)[ipad]; 
      float px = phit->getX();
      float py = phit->getY();
      float pz = phit->getZ();
      float pr = sqrt(px*px + py*py);
      
      int recentConfirms=0;
      
      //uv1
      for (unsigned int iuv1=0 ; iuv1< uv1candi->length(); iuv1++){   
	hit = (*mycandidate->uv1)[iuv1]; 
	int cid = hit->getAssociatedCandidate(); 
	// if UV hit assigned to a different candidate, recalculate intersection point
	if (cid != c) {
	  hit->setAssociatedCandidate(c);
	  PHPoint transInter;
	  if (mycandidate->intersectUVwithXPlane(hit,transInter)) {
	    PHPoint inter = mycandidate->rotateAndTranslate(transInter);
	    hit->setLocalIntersectionPoint(inter);
	    hit->setGlobalIntersectionPoint(transInter);
	  }
	}
	PHPoint inter = hit->getGlobalIntersectionPoint();
	float hix = inter.getX();
	float hiy = inter.getY();
	float hiz = inter.getZ();
	float hr  = sqrt(hix*hix + hiy*hiy);
	float prediction = (pz-vz)/pr * hr + vz;
	// Parameterized error due to residual bend: TKH
	// There is no a fundamental justification for the form.
	prediction = prediction +(0.25*pz*alpha*alpha*(pr-hr)/(pr-u11r));
	float distance = abs( hiz - prediction );

	if (distance<pMaxUVDistance/2.0) recentConfirms++; //should be a cut parameter...
      }
      
      //uv2
      for (unsigned int iuv2=0 ; iuv2< uv2candi->length(); iuv2++){   
	hit = (*mycandidate->uv2)[iuv2]; 
	int cid = hit->getAssociatedCandidate(); 
	// if UV hit assigned to a different candidate, recalculate intersection point
	if (cid != c) {
	  hit->setAssociatedCandidate(c);
	  PHPoint transInter;
	  if (mycandidate->intersectUVwithXPlane(hit,transInter)) {
	    PHPoint inter = mycandidate->rotateAndTranslate(transInter);
	    hit->setLocalIntersectionPoint(inter);
	    hit->setGlobalIntersectionPoint(transInter);
	  }
	}

	PHPoint inter = hit->getGlobalIntersectionPoint();
	float hix = inter.getX();
	float hiy = inter.getY();
	float hiz = inter.getZ();
	float hr  = sqrt(hix*hix + hiy*hiy);
	float prediction = (pz-vz)/pr * hr + vz;
	// Parameterized error due to residual bend: TKH
	// There is no a fundamental justification for the form.
	prediction = prediction +(0.25*pz*alpha*alpha*(pr-hr)/(pr-u11r));
	float distance = abs( hiz - prediction );

	if (distance<pMaxUVDistance/2.0) recentConfirms++; //should be a cut parameter...
      }
      
      if (recentConfirms>0 && recentConfirms>=maxNumberOfConfirms) {
	if (recentConfirms==maxNumberOfConfirms) UVquality = 1;  // OK quality UV there but not unique
	else                                     UVquality = 2;  // Excellent quality, UV there and unique.

	maxNumberOfConfirms=recentConfirms;
	mycandidate->setFinalPc1Id(phit->getId());
	finalZ=pz;
	finalR=pr;
      }
      
    }

    // update candidate with UV information if found
    if (pc1quality) {

      // I have modified this routine so that is simply calls the candidate
      // update function after determining new global and local points.
      
      // calculate beta and zed using vertex coordinates
      float argu = (finalZ-vz)/finalR - .25*finalZ*alpha*alpha/(finalR-u11r);
      float beta = atan(1.0/argu);
      if (beta<0) beta = Pi+beta;
      
      float refX = mycandidate->getGlobalPoint().getX();
      float refY = mycandidate->getGlobalPoint().getY();
      float refR = sqrt(refX*refX+refY*refY);
      float Zed = (finalZ-vz)/finalR*refR + vz;
      Zed = Zed + (0.25*finalZ*alpha*alpha*(finalR-refR)/(finalR-u11r));

      // recalculation of basepoint and direction of candidate
      PHPoint  newGlobalPoint =  mycandidate->getGlobalPoint();
      newGlobalPoint.setZ(Zed);

      PHVector newGlobalVector =  mycandidate->getGlobalVector();
      newGlobalVector.setZ(0.);
      newGlobalVector.normalize();
      newGlobalVector.setX(sin(beta)*newGlobalVector.getX());
      newGlobalVector.setY(sin(beta)*newGlobalVector.getY());
      newGlobalVector.setZ(cos(beta));    

      int arm = mycandidate->getArm();
      PHPoint newLocalPoint  = geometry->rotateAndTranslateInverse(arm,newGlobalPoint);
      PHPoint newLocalVector = geometry->rotateAndTranslateInverse(arm,newGlobalVector);

      mycandidate->update(newLocalPoint, newLocalVector, newGlobalPoint, newGlobalVector);

      // update quality
      if (pc1quality == 1)  mycandidate->setQuality(16|mycandidate->getQuality()); // PC1 found...
      if (pc1quality == 2)  mycandidate->setQuality(48|mycandidate->getQuality()); // PC1 found AND unique...
      if (UVquality  == 1)  mycandidate->setQuality( 4|mycandidate->getQuality()); // UV found...
      if (UVquality  == 2)  mycandidate->setQuality(12|mycandidate->getQuality()); // UV found AND unique...
      mycandidate->setUVHough(maxNumberOfConfirms);

    } 
  } // candidate loop

  return True;
}

PHBoolean DchFastCandidateFinder::getPC1Candidates(TABLE_HEAD_ST *dPc1_h,DPADCLUSTER_ST *dPc1)
{
  unsigned int arm,side;
  PHPoint pc1, localPc1;
  PHCylPoint cylPc1;
  PHVector localVector;     
  PHPoint  localPoint;
  PHVector globalVector;     
  PHPoint  globalPoint;
  DchTrackCandidate *temp_candidate;

  for (int i=0; i<dPc1_h->nok; i++){  // loop over all PC1 point and make DchTrackCandidates

    pc1.setX(dPc1[i].xyz[0]);           // Pc1 Point in global coordinates
    pc1.setY(dPc1[i].xyz[1]);
    pc1.setZ(dPc1[i].xyz[2]);

    if (pc1.getZ() < 0) {
      side = SOUTH;
    } else {
      side = NORTH;
    }
    if (pc1.getX() < 0) {
      arm = EAST;
    } else {
      arm = WEST;
    }

    globalPoint = pc1;  
    globalVector.setX(pc1.getX()-vertex.getX());  
    globalVector.setY(pc1.getY()-vertex.getY());
    globalVector.setZ(pc1.getZ()-vertex.getZ());

  /// temporary fix to calculate intersection with reference cylinder

  // calculates intersection with reference radius
  // input     arm
  // basepoint and direction of straight line trajectory 
  //           and one hit on track in global coordinates and one hit on track
  // output    basepoint and direction in local corrdinates
  //           basepoint in global coordinates is moved to reference radius

    PHPoint p1,p2;  
    PHCylinderSection *dcArm;
    PHCylinderSection *dcEast = geometry->getEastCylinder();
    PHCylinderSection *dcWest = geometry->getWestCylinder();

    if (arm == EAST) {
      dcArm = dcEast;
    }else{
      dcArm = dcWest; 
    }  	

    PHLine track(globalPoint,globalVector);
    short numberOfCrossings = intersectionLineCylinderSection(track,*dcArm,p1,p2);
    if (numberOfCrossings == 0 ) {
    } else if (numberOfCrossings == 1) {
       globalPoint = p1; 
    } else {
      double d1 = distancePointToPoint(pc1,p1);
      double d2 = distancePointToPoint(pc1,p2);
      if (d1 >= d2) {
      	globalPoint = p1;
      } else {
        globalPoint = p2;
      }
    }	
    localPoint  = geometry->rotateAndTranslateInverse(arm,globalPoint);   
    localVector = geometry->rotateAndTranslateInverse(arm,globalVector);

    // create candidate with local point and vector

    temp_candidate = new DchTrackCandidate(arm, side, localPoint, localVector);

    transformCandidateFromLocalToGlobal(temp_candidate);
    candidates->append(temp_candidate);

    // collect UV hits that intersect alpha-plane

    PHPoint inter;
    PHAngle candidatePhi, hitPhi;
    DchHitLine *hit;
    PHPointerList<DchHitLine> *uv1candi;
    PHPointerList<DchHitLine> *uv2candi;
    PHPointerList<DchHitLine>* list1;
    PHPointerList<DchHitLine>* list2;

    list1 = hitList->getList(UV1Wire,arm,side);
    list2 = hitList->getList(UV2Wire,arm,side);

    int lengthList1 = list1->length();
    int lengthList2 = list2->length();

    candidatePhi = temp_candidate->getLocalPhi();

    uv1candi = temp_candidate->uv1; // get the pointer to the list
    uv2candi = temp_candidate->uv2; // get the pointer to the list

    for(int h=0; h<lengthList1; h++) {
      hit = (*list1)[h];
      hitPhi = hit->getLocalPhi();

      if ( fabs(candidatePhi-hitPhi) > (pCellDifferenceCut+4)*0.07854 ) continue;   
      if (temp_candidate->intersectUVwithXPlane(hit,inter)) {
	// select z range depending on side (side = -1 no side selection)
	if (inter.getZ()<pZMin || inter.getZ()>pZMax) continue;
	if (side == NORTH && (inter.getZ() < -pZAvg)) continue; 
	if (side == SOUTH && (inter.getZ() >  pZAvg)) continue; 

	uv1candi->append(hit); // append the uv hit to the list of the candidate
       }
     }
    for(int h=0; h<lengthList2; h++) {
      hit = (*list2)[h];
      hitPhi = hit->getLocalPhi();

      if ( fabs(candidatePhi-hitPhi) > (pCellDifferenceCut+4)*0.07854 ) continue;   
      if (temp_candidate->intersectUVwithXPlane(hit,inter)) {
	// select z range depending on side (side = -1 no side selection)
	if (inter.getZ()<pZMin || inter.getZ()>pZMax) continue;
	if (side == NORTH && (inter.getZ() < -pZAvg)) continue; 
	if (side == SOUTH && (inter.getZ() >  pZAvg)) continue; 

	uv2candi->append(hit); // append the uv hit to the list of the candidate
      }
    }
  } 

  return True;

}

// ******************************************
// Main routine for X1 only candidate search
// ******************************************

PHBoolean DchFastCandidateFinder::getX1Candidates(short arm, short side)
{
  if (HoughDimensionX != pNumberOfAlphaBins || HoughDimensionY != pNumberOfPhiBins) {
    resizeHoughArray(pNumberOfAlphaBins, pNumberOfPhiBins); // resize if necessary
    houghThresholdOnCell = pHoughThresholdOnXCell;
    houghThresholdOnMask = pHoughThresholdOnXMask;
  }

  PHPointerList<DchHitLine>* list1;
  PHPointerList<DchHitLine>* list2;

  list1 = hitList->getList(X1Wire,arm,side);
  list2 = hitList->getList(X1Wire,arm,side);

  // Hough transform of X1 hits with X1 hits
  fillXHoughArray(list1, list2);

  // searching for the candidates
  searchXCandidates(arm,side);

  // updating local and global information for all the candidates found 
  DchTrackCandidate* mycandidate;
  int lengthCandidates = candidates->length();

  for (int i=0; i< lengthCandidates; i++) {
     mycandidate = (*candidates)[i];
     transformCandidateFromLocalToGlobal(mycandidate);
  }

  return True;
}

// ******************************************
// Main routine for X2 only candidate search
// ******************************************
PHBoolean DchFastCandidateFinder::getX2Candidates(short arm, short side)
{
  if (HoughDimensionX != pNumberOfAlphaBins || HoughDimensionY != pNumberOfPhiBins) {
    resizeHoughArray(pNumberOfAlphaBins, pNumberOfPhiBins); // resize if necessary
    houghThresholdOnCell = pHoughThresholdOnXCell;
    houghThresholdOnMask = pHoughThresholdOnXMask;
  }

  PHPointerList<DchHitLine>* list1;
  PHPointerList<DchHitLine>* list2;

  list1 = hitList->getList(X2Wire,arm,side);
  list2 = hitList->getList(X2Wire,arm,side);

  fillXHoughArray(list1, list2);
  searchXCandidates(arm,side);

  DchTrackCandidate* mycandidate;
  int lengthCandidates = candidates->length();

  for (int i=0; i< lengthCandidates; i++) {
     mycandidate = (*candidates)[i];
     transformCandidateFromLocalToGlobal(mycandidate);
  }

  return True;
}
// ******************************************
// Main routine for X1-X2 candidate search
// ******************************************

PHBoolean DchFastCandidateFinder::getXCandidates(short arm, short side)
{
  if (HoughDimensionX != pNumberOfAlphaBins || HoughDimensionY != pNumberOfPhiBins) {
    resizeHoughArray(pNumberOfAlphaBins, pNumberOfPhiBins); // resize if necessary
    houghThresholdOnCell = pHoughThresholdOnXCell;
    houghThresholdOnMask = pHoughThresholdOnXMask;
  }

  PHPointerList<DchHitLine>* list1;
  PHPointerList<DchHitLine>* list2;

  list1 = hitList->getList(X1Wire,arm,side);
  list2 = hitList->getList(X2Wire,arm,side);

  fillXHoughArray(list1, list2);
  searchXCandidates(arm,side);

  DchTrackCandidate* mycandidate;
  int lengthCandidates = candidates->length();

  for (int i=0; i< lengthCandidates; i++) {
     mycandidate = (*candidates)[i];
     transformCandidateFromLocalToGlobal(mycandidate);
  }

  return True;
}

// ******************************************
// Main routine for UV1-UV2 candidate search
// ******************************************

PHBoolean DchFastCandidateFinder::getUVCandidates(short arm, short side)
{
  if (HoughDimensionX != pNumberOfBetaBins || HoughDimensionY != pNumberOfZedBins) {
    resizeHoughArray(pNumberOfBetaBins, pNumberOfZedBins); // resize if necessary
  }
  houghThresholdOnCell = pHoughThresholdOnUVCell;
  houghThresholdOnMask = pHoughThresholdOnUVMask;

  PHPointerList<DchHitLine>* list1;
  PHPointerList<DchHitLine>* list2;

  list1 = hitList->getList(UV1Wire,arm,side);
  list2 = hitList->getList(UV2Wire,arm,side);

  fillUVHoughArray(arm, side, list1, list2);

  return True;
}

// ***********************************************
// Fills X hough array for 2 X hit lists
// ***********************************************

void DchFastCandidateFinder::fillXHoughArray(PHPointerList<DchHitLine>* list1, PHPointerList<DchHitLine>* list2)
{
   int i1,i2;
   int min2;
   short cell1, cell2;
   int length1 = list1->length();
   int length2 = list2->length();
   double alpha,phi;
   int alphaIndex;
   int phiIndex;
   unsigned int arm;
   DchHitLine* hit1;
   DchHitLine* hit2;

   zeroHoughArray(HoughDimensionX, HoughDimensionY); 

   for (i1 = 0; i1 < length1; i1++) {
     hit1 = (*list1)[i1];
     //  if hit was used before for example in X1-X2 trackfinding,
     //  do not use it again
     if (hit1->used) continue;
     cell1 = hit1->getCell();
     //  for X only case, get rid of hits permutations and hit with itself case. 
     if(list1 == list2) {
       min2 = i1+1;
     } else {
       min2 = 0;
     }
     for(i2 = min2; i2 < length2; i2++) {
       hit2 = (*list2)[i2];
       if (hit2->used) continue;
       cell2 = hit2->getCell();
       // if the hits are too far from each other, just continue, they can not be from one track
       if (abs(cell2- cell1) > pCellDifferenceCut) continue; 

       phiAndAlpha(hit1, hit2, alpha, phi);
       arm = hit2->getArm();
       if (arm == EAST) {
	 phi = Pi - phi;
       }

       alphaIndex  = int((alpha -pMinAlpha)* HoughDimensionX/(pMaxAlpha - pMinAlpha)); 
       phiIndex    = int((phi-pMinPhi)*HoughDimensionY/(pMaxPhi - pMinPhi)); 

       // BE A PLACE TO ELIMINATE IF STATEMENTS 
       if( (alphaIndex < HoughDimensionX  && alphaIndex > 0) && (phiIndex<= HoughDimensionY && phiIndex>= 0) ) 
	 {
	   pair<int,int> Position(alphaIndex,phiIndex);
	   SparseHoughLookup.insert(Position);
	   HoughArray[alphaIndex][phiIndex]++;
	 }

     } // loop second list
   } // loop first list

}

// *******************************************************
// Create candidates if hough peak is over the threshold 
// calculates and update main track parameters
// *******************************************************

PHBoolean DchFastCandidateFinder::searchXCandidates(short arm, short side)
{
  float phiBinSize = (pMaxPhi-pMinPhi)/float(HoughDimensionY);
  float alphaBinSize = (pMaxAlpha-pMinAlpha)/float(HoughDimensionX);
  float alphaCog;
  float phiCog;
  double phi,alpha;
  float phiValue,slopeValue,alphaValue;
  int totalHoughValue;
  DchTrackCandidate *temp_candidate;

  if (arm != 0 && arm != 1)
    {
      return False;
    }
  set<pair<int,int>,ltposition>::const_iterator HoughLooper;
  for (HoughLooper=SparseHoughLookup.begin(); HoughLooper!=SparseHoughLookup.end(); HoughLooper++)
    {
      int isp,ia;
      pair<int,int> Position = (*HoughLooper);
      ia = Position.first;
      isp = Position.second;

      // if we have local maximum and 3x3 neighbouring bins sum is over threshold
      totalHoughValue = integralAndCogIfLocalMaximum(ia,isp,alphaCog, phiCog);
      if (totalHoughValue>0) 
	{
	  // calculation and update track parameters
  	  phi    = (phiCog)*phiBinSize + pMinPhi;
 	  alpha  = (alphaCog)*alphaBinSize + pMinAlpha;
	  
	  if (arm == 0) //EAST 
	    {
	      alphaValue = alpha; 
	      phiValue =  Pi - phi;
	      slopeValue = tan(phiValue - alphaValue - Pi); 
	    } 
	  else
	    {
	      alphaValue = alpha; 
	      phiValue = phi;
	      slopeValue = tan(phiValue - alphaValue);
	    }
	  // and create new DchTrackCandidate
          temp_candidate = new DchTrackCandidate(arm, side, alphaValue, phiValue, slopeValue, pReferenceRadius);
          temp_candidate->setXHough(totalHoughValue);
	  
	  candidates->append(temp_candidate);
	  
	}
    }
  return True;
}

// *******************************************************************
// Search for UV1-UV2 hough information for the given track candidate  
// calculates and update main track parameters
// *******************************************************************

PHBoolean DchFastCandidateFinder::searchUVCandidates(DchTrackCandidate* candidate)
{
  float zedBinSize = (pMaxZed-pMinZed)/float(HoughDimensionY);
  float betaBinSize = (pMaxBeta-pMinBeta)/float(HoughDimensionX);
  float betaCog;
  float betaTarget;
  double betaValue;
  double zedValue;
  float zedCog;
  int totalHoughValue;
  int maxHoughValue = 0;
  float zedMax=0;
  float betaMax = 0;
  int candidateCount = 0;
  set<pair<int,int>,ltposition>::const_iterator HoughLooper;
  for (HoughLooper=SparseHoughLookup.begin(); HoughLooper!=SparseHoughLookup.end(); HoughLooper++)
    {
      int ized,ibeta;
      pair<int,int> Position = (*HoughLooper);
      ibeta = Position.first;
      ized = Position.second;

      // if there is a hough peak over threshold
      totalHoughValue = integralAndCogIfLocalMaximum(ibeta,ized, betaCog, zedCog);
      if (totalHoughValue>0) 
	{
	  zedValue = (zedCog)*zedBinSize + pMinZed;
	  betaValue  = (betaCog)*betaBinSize + pMinBeta;
	  betaTarget = Pi/2. - (zedValue-vertex.getZ())/pReferenceRadius; //
	  if (totalHoughValue > maxHoughValue) 
	    { // if integral bigger than the previous one
	      if (fabs(betaValue - betaTarget) < pDeltaBetaVertexCut ) 
		{ // if in the correlation band (points to the vertex)
		  maxHoughValue = totalHoughValue;
		  betaMax = betaValue;
		  zedMax = zedValue;
		  candidateCount++;
		}
	    }
	}
    }
  

  // if we found UV information - update candidate parameters
  if (candidateCount) {
    candidate->setLocalBeta(betaMax);
    candidate->setLocalZed(zedMax);
    candidate->setUVHough(maxHoughValue);

    // recalculation of basepoint and direction of candidate
    PHPoint ptmp  =  candidate->getLocalPoint();
    PHVector vtmp =  candidate->getLocalVector();

    if (candidate->getLocalBeta() != 0 && candidate->getLocalZed() != 0) {
      ptmp.setZ(candidate->getLocalZed());
      vtmp.setZ(vtmp.length()/tan(candidate->getLocalBeta()));    

      candidate->setLocalPoint(ptmp);
      candidate->setLocalVector(vtmp);
    }
    candidate->setGlobalPoint (geometry->rotateAndTranslate(candidate->getArm(),ptmp));
    candidate->setGlobalVector(geometry->rotateAndTranslate(candidate->getArm(),vtmp));
    return True;
  } else {
    return False;
  }
}

// ******************************************************************
// Main track information update routine.
// Calculate and update local and global parameters of the track and
// coordinate systems associated with track  
// ******************************************************************

PHBoolean DchFastCandidateFinder:: transformCandidateFromLocalToGlobal(DchTrackCandidate* candidate)
{
  // This complicated fuction is done here even if would fit better in
  // the Candidate itself because to transform to global coordinate
  // system it needs the geometry and one does not want to give the
  // DCh geometry to the candidate transform Points and Vector from
  // local (after X search) to global.
  //
  // I have "updated" the candidate's "update" routine so that it not only
  // updates the points but also the planes.  This means that we don't
  // have to concern ourselves with calculating the global planes here,
  // it is done correctly in the candidate itself.
  //                             TKH 1-29-2002

  int arm=candidate->getArm();

  PHPoint   localpoint    = candidate->getLocalPoint();
  PHVector  localvector   = candidate->getLocalVector();

  PHPoint   globalPoint   = geometry->rotateAndTranslate(arm,localpoint);
  PHVector  globalVector  = geometry->rotateAndTranslate(arm,localvector); // candidate direction (R')

  candidate->update(localpoint, localvector, globalPoint, globalVector);

  // calculate the Rotation-Matrix and the TRanslationVector of the Track
  // AD should point in +z direction!
  //   I must admit that I don't understand the lines of code below, but 
  // I have not attempted to change them.
  //                            TKH 1-29-2002
  PHPlane   planeUV       = candidate->getLocalPlane(UV1Wire);
  PHVector  uvVector  = (PHVector)geometry->rotateAndTranslate(arm,planeUV.getNormal());
  PHPlane gloUV(globalPoint,uvVector);
  PHVector  missingAxis = globalVector.cross(uvVector); // candidate Z axis
  PHPoint origin = gloUV.getOrigin();
  PHFrame trackFrame(origin,missingAxis,globalVector);
  PHFrame XY;
  PHMatrix rotation;
  PHVector translation;

  // transform the two frame in a rotation matrix and a translation vector
  frames2MatrixAndVector(XY, trackFrame, rotation, translation);
  candidate->setRotationAndTranslation(rotation,translation);
  candidate->setTrackFrame(trackFrame);

  return True;
}

// *********************
// Fills UV hough array
// *********************
void DchFastCandidateFinder::fillUVHoughArray(short armIn, short sideIn, 
                          PHPointerList<DchHitLine>* list1, PHPointerList<DchHitLine>* list2)
{
  // AD major modifications in this routine 
  // parameters limiting z-range of alpha-plane

  int arm,side;
  int lengthCandidates = candidates->length();
  PHPoint inter1,inter2;
  PHAngle candidatePhi;
  DchHitLine *hit1;
  DchHitLine *hit2;
  DchTrackCandidate* mycandidate;
  PHPointerList<DchHitLine> *uv1candi;
  PHPointerList<DchHitLine> *uv2candi;

  for (int i=0; i< lengthCandidates; i++) {
     mycandidate = (*candidates)[i];
     arm = mycandidate->getArm();
     side = mycandidate->getSide();
     if (mycandidate->getQuality()>3) continue;  // only if UV was not yet reconstructed

     candidatePhi =  mycandidate->getLocalPhi();
     // only candidates in requested arm-side (no side selection side = -1) 
     if (side != sideIn || arm != armIn) continue;

     zeroHoughArray(HoughDimensionX,HoughDimensionY);
     uv1candi = mycandidate->uv1; // get the pointer to the list
     uv2candi = mycandidate->uv2; // get the pointer to the list

     double beta,zed;
     int betaIndex, zedIndex;
     int totalUV1 = uv1candi->length();
     int totalUV2 = uv2candi->length();
     PHPoint p1;
     PHPoint p2;
     PHPoint transInter,inter;
     int cid;

     for (int k1=0; k1 < totalUV1; k1++) {
       hit1  =  (*uv1candi)[k1];
       p1    = hit1->getLocalIntersectionPoint(); //in local system 
       cid   = hit1->getAssociatedCandidate(); 
       inter = hit1->getLocalIntersectionPoint();
       // hit was assigned to other candidate - intercection should be recalculated
       if (cid != i) {
         hit1->setAssociatedCandidate(i);
         if (mycandidate->intersectUVwithXPlane(hit1,transInter)) {
	   inter = mycandidate->rotateAndTranslate(transInter);
	   hit1->setLocalIntersectionPoint(inter);
	   hit1->setGlobalIntersectionPoint(transInter);
	   p1 = inter;
         }
       }
       for (int k2=0; k2 <totalUV2; k2++) {
 	 hit2   =  (*uv2candi)[k2];
	 p2     = hit2->getLocalIntersectionPoint();
	 cid    = hit2->getAssociatedCandidate(); 
	 inter  = hit2->getLocalIntersectionPoint();
	 // hit was assigned to other candidate - intercection should be recalculated
	 if (cid != i) {
	   hit2->setAssociatedCandidate(i);
	   if (mycandidate->intersectUVwithXPlane(hit2,transInter)) {
	     inter = mycandidate->rotateAndTranslate(transInter);
	     hit2->setLocalIntersectionPoint(inter);
	     hit2->setGlobalIntersectionPoint(transInter);
	     p2 = inter;
	   }
	 }
	 betaAndZed(p1,p2,beta,zed);
	 betaIndex  = int((beta - pMinBeta)* HoughDimensionX/(pMaxBeta - pMinBeta)); 
	 zedIndex = int((zed - pMinZed)*HoughDimensionY/(pMaxZed - pMinZed)); 

	 // if within the hough limits - fill the array
	 // BE A PLACE TO ELIMINATE IF STATEMENTS 
	 if( (betaIndex <= HoughDimensionX  && betaIndex >= 0) &&  (zedIndex <= HoughDimensionY && zedIndex>= 0) ) 
	   {
	     pair<int,int> Position(betaIndex,zedIndex);
	     SparseHoughLookup.insert(Position);
	     HoughArray[betaIndex][zedIndex]++;
	   }
       }
     }
     if (totalUV1 && totalUV2) {
       // found some UV information - update the quality
       if (searchUVCandidates(mycandidate)){
	 int tmp = mycandidate->getQuality();
	 mycandidate->setQuality(4|tmp);
       }
     }
  }
}

// *******************************************
// Main candidate purging routine.
// if track has too few hits in X, UV or X+UV
// it is being DELETED on this stage
// *******************************************

PHBoolean DchFastCandidateFinder::purgeCandidateList()
{
  // check if some candidates are too near one to the other

  int i = 0;
  short numberOfPurgedCandidates = 0;

  int hitX,hitUV;
  DchTrackCandidate *mycandidate;

  for (i=candidates->length()-1;i>=0;i--) {
    mycandidate = (*candidates)[i];
    // number of hits within a road around the track in X and UV plane
    hitX  = (mycandidate->closestX)->length();
    hitUV = (mycandidate->closestUV)->length();
    // if some number lower then the threshold - remove the candidate
    if (hitX+hitUV < pMinimumNumberOfHits ||
	hitX < pMinimumNumberOfXHits || hitUV < pMinimumNumberOfUVHits) {
       mycandidate = candidates->removeAt(i); 
       delete mycandidate;
       numberOfPurgedCandidates++;
    }
  }

  return True;
}

void DchFastCandidateFinder::Flatten(int reset, TABLE_HEAD_ST *dDchTracks_h,
				 DDCHTRACKS_ST *dDchTracks,
				 TABLE_HEAD_ST *dDchHit_h,
				 DDCHHIT_ST *dDchHit, int suppressHits){ 
  // pointers to candidates and vice versa are setted here
  unsigned long i,j;
  if (reset) dDchTracks_h->nok=0;
  DchTrackCandidate *mycandidate;

  /*
   * If there are more candidates than the max length of
   * dchTracksExt table can hold, we abort the event.
   */

  tooManyTracks = False;
  if(candidates->length() > dchTracksExt->MaxRowCount())
    {
      tooManyTracks = True;
      return;
    }

  for (i=0;i<candidates->length();i++){
    mycandidate = (*candidates)[i];    
    long id = dDchTracks_h->nok;
    mycandidate->setTrackId(id);

    dDchTracks[id].trackid   = id;
    dDchTracks[id].arm       = mycandidate->getArm();
    dDchTracks[id].side      = mycandidate->getSide();
    dDchTracks[id].quality   = mycandidate->getQuality();

    PHPoint temp;    
    temp = mycandidate->getGlobalPoint();
    dDchTracks[id].point[0] = temp.getX();
    dDchTracks[id].point[1] = temp.getY();
    dDchTracks[id].point[2] = temp.getZ();

    temp = mycandidate->getGlobalVector();
    dDchTracks[id].direction[0] = temp.getX();
    dDchTracks[id].direction[1] = temp.getY();
    dDchTracks[id].direction[2] = temp.getZ();

    dDchTracks[id].phi   = mycandidate->getGlobalPhi();
    dDchTracks[id].alpha = mycandidate->getGlobalAlpha();

    dDchTracks[id].betaNoVertex =  mycandidate->getLocalBeta(); 
    dDchTracks[id].beta =  mycandidate->getLocalBeta(); 
    dDchTracks[id].zed  =  mycandidate->getLocalZed(); 

    if (dDchTracks[id].quality < 4) { // Z information not existing 
      dDchTracks[id].point[2] = -10000;
      dDchTracks[id].zed = -10000;
    }

    // put first 40 associated hits in tracktable 
    int plane=-1;
    DchHitLine* hit;
    for (j=0; j<numberOfPlanes; j++){
      dDchTracks[id].hits[j] = -1;
    }

    for (j=0; j<(mycandidate->closestX)->length(); j++){
      hit = (*(mycandidate->closestX))[j];
      plane = hit->getPlane();
      dDchTracks[id].hits[plane] = hit->getId();
    }
    for (j=0; j<(mycandidate->closestUV)->length(); j++){
      hit = (*(mycandidate->closestUV))[j];
      plane = hit->getPlane();
      dDchTracks[id].hits[plane] = hit->getId();
    }

    int relocated =0;
    if(dDchTracks[id].hits[39]>-1){
      for(j=38;j>31;j--){
	if(dDchTracks[id].hits[j]<0) {
	  dDchTracks[id].hits[j] = dDchTracks[id].hits[39];
	  relocated =1;
	  break;
	}
      }
      if(!relocated){
	for(j=19;j>11;j--){
	  if(dDchTracks[id].hits[j]<0) {
	    dDchTracks[id].hits[j] = dDchTracks[id].hits[39];
	    break;
	  }
	}
      }
    }
    dDchTracks[id].hits[39] = mycandidate->getFinalPc1Id();
    dDchTracks_h->nok++;
  }  
  // if suppress the Hit , set to zero the count 
  if (suppressHits ) {
    dDchHit_h->nok = 0; 
  }
}

void DchFastCandidateFinder::fillOutputTables(DchTrack *trackTable, DchHitLineTable* hitLineTable, int suppressHits)
{
  trackOutList.clearAndDestroy();

  unsigned long i, j;
  DchSnglTrackv1* trackOut;
  DchTrackCandidate *mycandidate;

  for (i = 0;i < candidates->length();i++)
    {
      mycandidate = (*candidates)[i];
      long id = trackOutList.length();
      mycandidate->setTrackId(id);

      trackOut = new DchSnglTrackv1();
      trackOutList.append(trackOut);

      trackOut->set_trackid(id);
      trackOut->set_arm(mycandidate->getArm());
      trackOut->set_side(mycandidate->getSide());
      trackOut->set_quality(mycandidate->getQuality());

      PHPoint temp;
      PHVector temp2;

      temp = mycandidate->getGlobalPoint();
      temp2 = mycandidate->getGlobalVector();

      if (mycandidate->getQuality() < 4)
        {
          temp.setZ( -10000);
        }
      trackOut->set_point(temp);
      trackOut->set_direction(temp2);

      trackOut->set_phi(mycandidate->getGlobalPhi());
      trackOut->set_alpha(mycandidate->getGlobalAlpha());

      trackOut->set_betaNoVertex(mycandidate->getLocalBeta());
      trackOut->set_beta(mycandidate->getLocalBeta());
      trackOut->set_zed(mycandidate->getLocalZed());
      if (mycandidate->getQuality() < 4)
        {
          trackOut->set_zed( -10000);
        }

      // put first 40 associated hits in tracktable
      int plane = -1;
      DchHitLine* hit;
      for (j = 0; j < numberOfPlanes; j++)
        {
          trackOut->set_hits(j, -1);
        }

      for (j = 0; j < (mycandidate->closestX)->length(); j++)
        {
          hit = (*(mycandidate->closestX))[j];
          plane = hit->getPlane();
          trackOut->set_hits(plane, hit->getId());
        }
      for (j = 0; j < (mycandidate->closestUV)->length(); j++)
        {
          hit = (*(mycandidate->closestUV))[j];
          plane = hit->getPlane();
          trackOut->set_hits(plane, hit->getId());
        }

      int relocated = 0;
      if (trackOut->get_hits(39) > -1)
        {
          for (j = 38;j > 31;j--)
            {
              if (trackOut->get_hits(j) < 0)
                {
                  trackOut->set_hits(j, trackOut->get_hits(39));
                  relocated = 1;
                  break;
                }
            }
          if (!relocated)
            {
              for (j = 19;j > 11;j--)
                {
                  if (trackOut->get_hits(j) < 0)
                    {
                      trackOut->set_hits(j, trackOut->get_hits(39));
                      break;
                    }
                }
            }
        }
      trackOut->set_hits(39, mycandidate->getFinalPc1Id());
    }

  fitDchTracksAlpha();

  if (trackTable)
    {
      trackTable->set_TClonesArraySize( trackOutList.length());
      trackTable->set_DchNTrack( trackOutList.length());

      for (i = 0; i < trackOutList.length(); i++)
	{
	  trackOut = trackOutList[i];
	  trackTable->AddTrack(trackOut, i);
	  delete trackOut;
	}

    }

  // if you want to suppress the hits then Clear the list

  if (suppressHits)
    {
      hitLineTable->Clear();
    }
}

void DchFastCandidateFinder::calculateClosestApproachToBeamAxis() 
{
  DchTrackCandidate *mycandidate;
  PHPoint  base;
  PHVector direction;
  PHLine   track, beam;

  beam.setBasepoint(vertex);  
  beam.setDirection(beamaxis);  

  for (unsigned int i=0; i<candidates->length();i++) {
    mycandidate = (*candidates)[i];
    base        = mycandidate->getGlobalPoint();
    direction   = mycandidate->getGlobalVector();
    track.setBasepoint(base);  
    track.setDirection(direction);

    float dist = distanceLineLine(track,beam);
    mycandidate->setClosestApproachToBeamAxis(dist);
  }
}

void DchFastCandidateFinder::setQuality(int q) 
{
  DchTrackCandidate *mycandidate;

  for (unsigned int i=0; i<candidates->length();i++) {
    mycandidate = (*candidates)[i];
    int tmp = mycandidate->getQuality();

    mycandidate->setQuality(tmp|q);
  }
}

void 
DchFastCandidateFinder::fitDchTracksAlpha()
{
  Int_t nxclosest;
  PHPoint point;
  PHCylPoint cylpoint;
  DchTrackCandidate *mycandidate;
  DchHitLine* hitline;
  unsigned int i;
  int k, j;
  int arm, plane;
  float dist1, dist2;
  int nx1 = 0, nx2 = 0;

  double w1[12];
  double w2[12];
  double x1r[12];
  double x2r[12];
  double y1r[12];
  double y2r[12];
  double r1r[12];
  double r2r[12];
  double p1r[12];
  double p2r[12];

  int multiplicity;

  multiplicity = candidates->length();

  if (dchTracksExt)
    {
      dchTracksExt->SetRowCount(multiplicity);
    }

  for (i = 0; i < candidates->length(); i++)
    {
      int odd1, even1;
      int odd2, even2;
      mycandidate = (*candidates)[i];
      arm = mycandidate->getArm();
      nx1 = nx2 = 0;
      dist1 = dist2 = 0;
      odd1 = even1 = odd2 = even2 = 0;
      nxclosest = mycandidate->closestX->length();
      for (j = 0; j < 12; j++)
        {
	  w1[j] = -100;
	  w2[j] = -100;
          p1r[j] = -100;
	  p2r[j] = -100;
	  x1r[j] = -100;
	  x2r[j] = -100;
	  y1r[j] = -100;
	  y2r[j] = -100;
	  r1r[j] = -100;
	  r2r[j] = -100;
        }
      // get the information needed for each hit
      for (k = 0; k < nxclosest; k++)
        {
          hitline = (*(mycandidate->closestX))[k];
          plane = hitline->getPlane();
          if (plane < 12)
            { // X1 hit
              point = hitline->getBasepoint();
              cylpoint = point;
              x1r[nx1] = point.getX();
              y1r[nx1] = point.getY();
              r1r[nx1] = cylpoint.getR();
              w1[nx1] = 1.;
              p1r[nx1] = plane;
              dist1 = dist1 + hitline->getDistance();
              nx1++;
              if (plane % 2)
                {
                  odd1++;
                }
              else
                {
                  even1++;
                }
            }
          else if (plane < 32 && plane >= 20)
            { //X2 hit
              point = hitline->getBasepoint();
              cylpoint = point;

              x2r[nx2] = point.getX();
              y2r[nx2] = point.getY();
              r2r[nx2] = cylpoint.getR();
              w2[nx2] = 1.;
              p2r[nx2] = plane - 20.;
              dist2 = dist2 + hitline->getDistance();
              nx2++;
              if (plane % 2)
                {
                  odd2++;
                }
              else
                {
                  even2++;
                }
            }
          else
            {
              continue;
            }
        }
      // fit the hits
      int check1;
      double slope1, intercept1, chi21;
      double sig_slope1, sig_intercept1, corr1;
      int check2;
      double slope2, intercept2, chi22;
      double sig_slope2, sig_intercept2, corr2;
      const size_t stride = 1;

      // fit the radius vs the plane number
      check1 = -1;
      check2 = -1;
      if (nx1 > 1)
        {
	  check1 = gsl_fit_wlinear(p1r,stride,w1,stride,r1r,stride,
				   nx1,&intercept1,&slope1,
				   &sig_intercept1,&corr1,&sig_slope1,
				   &chi21);
	}
      if (nx2 > 1)
        {
	  check2 = gsl_fit_wlinear(p2r,stride,w2,stride,r2r,stride,
				   nx2,&intercept2,&slope2,
				   &sig_intercept2,&corr2,&sig_slope2,
				   &chi22);
	}

      float rmean1 = 100, rmean2 = 100;
      if (check1 == 0)
        {
          rmean1 = intercept1 + slope1 * 5.5;  // mean radius of the cell
	  check1 = gsl_fit_wlinear(x1r,stride,w1,stride,y1r,stride,
				   nx1,&intercept1,&slope1,
				   &sig_intercept1,&corr1,&sig_slope1,
				   &chi21);
        }
      if (check2 == 0)
        {
          rmean2 = intercept2 + slope2 * 5.5;  // mean radius of the cell
	  check2 = gsl_fit_wlinear(x2r,stride,w2,stride,y2r,stride,
				   nx2,&intercept2,&slope2,
				   &sig_intercept2,&corr2,&sig_slope2,
				   &chi22);
        }
      float n, m;
      float X_intersection, Y_intersection;
      float X_intersection1, X_intersection2;
      float globalPhi1, tmp1, globalAlpha1;
      float globalPhi2, tmp2, globalAlpha2;

      // evaluate the intersection of the line with the mean radius
      if (check1 == 0)
        {
          n = intercept1;
          m = slope1;

          X_intersection1 = ( -2 * m * n + sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean1 * rmean1))) / (2 * (1 + m * m));
          X_intersection2 = ( -2 * m * n - sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean1 * rmean1))) / (2 * (1 + m * m));
          if (fabs(X_intersection1 - x1r[0]) < fabs(X_intersection2 - x1r[0]))
            {
              X_intersection = X_intersection1;
            }
          else
            {
              X_intersection = X_intersection2;
            }
          Y_intersection = m * X_intersection + n;

          globalPhi1 = atan2(Y_intersection, X_intersection);
          if (globalPhi1 < -Pi / 2.0)
            {
              globalPhi1 = globalPhi1 + 2 * Pi;
            }
          if (arm==1)
            {
	      tmp1 = atan2(slope1, 1);
	    }
          else
            {
	      tmp1 = atan2(-slope1, -1);
	    }
	  //cvs ci -m "alpha1 and alpha2 in east arm miscalculated, unrelated to reconstruction but messed up evaluation" offline/packages/dch/DchFastCandidateFinder.C
          if (tmp1 < -Pi / 2)
            {
              tmp1 = tmp1 + 2 * Pi;
            }
	  globalAlpha1 = globalPhi1 - tmp1;
	  if (globalAlpha1>(Pi/2.))
	    {
	      globalAlpha1=globalAlpha1-Pi;
	    }
        }
      else
        {
          globalAlpha1 = -100.;
          intercept1 = -100.;
          slope1 = -100.;
          chi21 = -1;
        }

      if (check2 == 0)
        {
          n = intercept2;
          m = slope2;
          X_intersection1 = ( -2 * m * n + sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean2 * rmean2))) / (2 * (1 + m * m));
          X_intersection2 = ( -2 * m * n - sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean2 * rmean2))) / (2 * (1 + m * m));
          if (fabs(X_intersection1 - x2r[0]) < fabs(X_intersection2 - x2r[0]))
            {
              X_intersection = X_intersection1;
            }
          else
            {
              X_intersection = X_intersection2;
            }
          Y_intersection = m * X_intersection + n;

          globalPhi2 = atan2(Y_intersection, X_intersection);
          if (globalPhi2 < -Pi / 2.0)
            {
              globalPhi2 = globalPhi2 + 2 * Pi;
            }
          if (arm)
            {
	      tmp2 = atan2(slope2, 1);
	    }
          else
            {
	      tmp2 = atan2(-slope2, -1);
	    }
          if (tmp2 < -Pi / 2)
            {
              tmp2 = tmp2 + 2 * Pi;
            }
          globalAlpha2 = globalPhi2 - tmp2;
	  if (globalAlpha2>(Pi/2.))
	    {
	      globalAlpha2=globalAlpha2-Pi;
	    }
        }
      else
        {
          globalAlpha2 = -100.;
          intercept2 = -100.;
          slope2 = -100.;
          chi22 = -1;
        }

      int status = 0;
      if (even1 == 0)
        {
          status |= 0x01;
        }
      else if (odd1 == 0)
        { // if only even
          status |= 0x02;
        }
      if (even2 == 0)
        { // if only odd
          status |= 0x04;
        }
      else if (odd2 == 0)
        { // if only even
          status |= 0x08;
        }
      DchSnglTrackv1* trackOut = trackOutList[i];
      if (trackOut)
        {
          trackOut->set_status(status);
          trackOut->set_alpha1(globalAlpha1);
          trackOut->set_alpha2(globalAlpha2);
          trackOut->set_chi21(chi21);
          trackOut->set_chi22(chi22);
          if (nx1)
            {
	      trackOut->set_dist1(dist1 / nx1);
	    }
          if (nx2)
            {
	      trackOut->set_dist2(dist2 / nx2);
	    }
        }
      if (dchTracksExt)
        {
          dchTracksExt->set_id(i, mycandidate->getTrackId());
          dchTracksExt->set_status(i, status);
          dchTracksExt->set_alpha1(i, globalAlpha1);
          dchTracksExt->set_alpha2(i, globalAlpha2);
          dchTracksExt->set_chi21(i, chi21);
          dchTracksExt->set_chi22(i, chi22);
          if (nx1)
            {
	      dchTracksExt->set_dist1(i, dist1 / nx1);
	    }
          if (nx2)
            {
	      dchTracksExt->set_dist2(i, dist2 / nx2);
	    }
          dchTracksExt->set_nx1hits(i, nx1);
          dchTracksExt->set_nx2hits(i, nx2);
        }
    } // loop over candidates
}

void 
DchFastCandidateFinder::fillAligmentNtuple() 
{
  Int_t nxclosest;
  float array[150];
  PHPoint point;
  PHCylPoint cylpoint;
  DchTrackCandidate *mycandidate;
  DchHitLine* hitline;

  int odd1,even1;
  int odd2,even2;
  int k,j;
  unsigned int i;
  int arm,side,plane;
  float dist1, dist2;
  float zvtx;
  float xpc1, ypc1, zpc1,rpc1,ppc1;
  int nx1, nx2;
  int sector;
  double t1[12];
  double c1[12];
  double wd1[12];
  double d1[12];
  double p1[12];

  double w1[12],w2[12];
  double x1r[12]; 
  double x2r[12];
  double y1r[12];
  double y2r[12];
  double r1r[12];
  double r2r[12];
  double p1r[12];
  double p2r[12];
  double phi1r[12];
  double phi2r[12];
  double t2[12];
  double c2[12];
  double wd2[12];
  double d2[12];
  double p2[12];

  int candidateid;
  int multiplicity;

  zvtx = vertex.getZ();
  multiplicity = candidates->length();

  for ( i=0; i<candidates->length();i++) {
    mycandidate = (*candidates)[i];
    candidateid = i;
    arm = mycandidate->getArm();
    side = mycandidate->getSide();
  
    if (mycandidate->pc1->length() == 1) { // only one choice 
      xpc1 = (*(mycandidate->pc1))[0]->getX();
      ypc1 = (*(mycandidate->pc1))[0]->getY();
      zpc1 = (*(mycandidate->pc1))[0]->getZ();
      sector = (*(mycandidate->pc1))[0]->getSector();
      rpc1 = sqrt(xpc1*xpc1 + ypc1*ypc1);

      if (xpc1 != 0) {   // good pc1 point 
	ppc1 = atan2(ypc1,xpc1);
      }else {
	continue;
      }
      nx1 = nx2 = 0;
      dist1 = dist2 =0;
      odd1 = even1 = odd2 = even2 = 0;
      nxclosest = mycandidate->closestX->length();
      for (j=0; j<12; j++) {
	w1[j] =-100;
        w2[j]=-100;
	p1r[j] = p2r[j] = x1r[j] = x2r[j] = y1r[j] = y2r[j] = r1r[j] = r2r[j] = phi1r[j] = phi2r[j] = -100;
	t1[j] = c1[j] = p1[j] = wd1[j] = d1[j] =t2[j] = c2[j] = p2[j] = wd2[j] = d2[j] = -100;
      }
      // get the information needed for each hit
      for (k=0; k < nxclosest; k++) {
	hitline = (*(mycandidate->closestX))[k];
	plane = hitline->getPlane();
	if (plane < 12) { // X1 hit
	  point = hitline->getBasepoint();
	  cylpoint = point;
	  t1[plane] = hitline->getTime();
	  c1[plane] = hitline->getCell();
	  wd1[plane] = hitline->getWidth();
	  d1[plane] = hitline->getDistance();
	  p1[plane] = atan2(point.getY(), point.getX());
	  x1r[nx1] = point.getX();
	  y1r[nx1] = point.getY();
	  r1r[nx1] = cylpoint.getR();
	  phi1r[nx1] = (float)cylpoint.getPhi();
	  w1[nx1] = 1.;
	  p1r[nx1] = plane;
	  dist1 = dist1 + hitline->getDistance();
	  nx1++;
	  if (plane%2) {
	    odd1++;
	  }else{
	    even1++;
	  }
	}else if (plane <32 && plane >=20){ //X2 hit 
	  point = hitline->getBasepoint();
	  cylpoint = point;
	  t2[plane-20] = hitline->getTime();
	  c2[plane-20] = hitline->getCell();
	  wd2[plane-20] = hitline->getWidth();
	  d2[plane-20] = hitline->getDistance();
	  p2[plane-20] = atan2(point.getY(), point.getX());

	  x2r[nx2] = point.getX();
	  y2r[nx2] = point.getY();
	  r2r[nx2] = cylpoint.getR();
	  phi2r[nx2] = (float)cylpoint.getPhi();
	  w2[nx2] = 1.;
	  p2r[nx2] = plane-20.;
	  dist2 = dist2 + hitline->getDistance();
	  nx2++;
	  if (plane%2) {
	    odd2++;
	  }else{
	    even2++;
	  }
	}else {
	  continue;
	}
      }

      // fit the hits 
      int check1;
      double slope1, intercept1, chi21;
      double sig_slope1, sig_intercept1,corr1;
      int check2;
      double slope2, intercept2, chi22;
      double sig_slope2, sig_intercept2,corr2;
      const size_t stride = 1;

      // fit the radius vs the plane number 
      check1 = gsl_fit_wlinear(p1r,stride,w1,stride,r1r,stride,
			       nx1,&intercept1,&slope1,
			       &sig_intercept1,&corr1,&sig_slope1,
			       &chi21);
      check2 = gsl_fit_wlinear(p2r,stride,w2,stride,r2r,stride,
			       nx2,&intercept2,&slope2,
			       &sig_intercept2,&corr2,&sig_slope2,
			       &chi22);

      float rmean1=100, rmean2=100;
      if (check1 == 0) 
	{
	  rmean1 = intercept1 + slope1*5.5;  // mean radius of the cell
	  check1 = gsl_fit_wlinear(x1r,stride,w1,stride,y1r,stride,
				   nx1,&intercept1,&slope1,
				   &sig_intercept1,&corr1,&sig_slope1,
				   &chi21);
	}
      
      if (check2 == 0)
	{
	  rmean2 = intercept2 + slope2*5.5;  // mean radius of the cell
	  check2 = gsl_fit_wlinear(x2r,stride,w2,stride,y2r,stride,
				   nx2,&intercept2,&slope2,
				   &sig_intercept2,&corr2,&sig_slope2,
				   &chi22);
	}
	
      float n,m;
      float X_intersection, Y_intersection;
      float X_intersection1, X_intersection2;
      float globalPhi1, tmp1, globalAlpha1;
      float globalPhi2, tmp2, globalAlpha2;
      
      // evaluate the intersection of the line with the mean radius
      if ( check1 == 0 ) 
	{
	  n = intercept1;
	  m = slope1;
      
	  X_intersection1 = (-2*m*n + sqrt(4*m*m*n*n - 4*(1+m*m)*(n*n -rmean1*rmean1)))/(2*(1+m*m));
	  X_intersection2 = (-2*m*n - sqrt(4*m*m*n*n - 4*(1+m*m)*(n*n -rmean1*rmean1)))/(2*(1+m*m));
	  
	  if (fabs(X_intersection1 - x1r[0]) < fabs(X_intersection2 -x1r[0])) {
	    X_intersection = X_intersection1;
	  } else {
	    X_intersection = X_intersection2;
	  }
	  
	  Y_intersection = m*X_intersection + n;
	  
	  PHPoint  point1(X_intersection,Y_intersection,0.); 
	  PHVector vector1(1.,slope1,0.);
	  
	  globalPhi1 = atan2(point1.getY(), point1.getX());
	  if (globalPhi1 < -Pi / 2.0){
	    globalPhi1 = globalPhi1 + 2 * Pi;
	  }
	  tmp1 = atan2(vector1.getY(), vector1.getX());
	  if (tmp1 < -Pi / 2){
	    tmp1 = tmp1 + 2 * Pi;
	  }

	  globalAlpha1 = globalPhi1 - tmp1;
	  
	}
      else
	{
	  globalPhi1 = -100.;
	  globalAlpha1 = -100.;
	  intercept1 = -100.;
	  slope1 = -100.;
	}
      
      if ( check2 == 0 ) 
	{    
	  n = intercept2;
	  m = slope2;
 
	  X_intersection1 = (-2*m*n + sqrt(4*m*m*n*n - 4*(1+m*m)*(n*n -rmean2*rmean2)))/(2*(1+m*m));
	  X_intersection2 = (-2*m*n - sqrt(4*m*m*n*n - 4*(1+m*m)*(n*n -rmean2*rmean2)))/(2*(1+m*m));
	  
	  if (fabs(X_intersection1 - x2r[0]) < fabs(X_intersection2 -x2r[0])) {
	    X_intersection = X_intersection1;
	  } else {
	    X_intersection = X_intersection2;
	  }
	  
	  Y_intersection = m*X_intersection + n;
	  
	  PHPoint  point2(X_intersection,Y_intersection,0.); 
	  PHVector vector2(1.,slope2,0.);
	        
	  globalPhi2 = atan2(point2.getY(), point2.getX());
	  if (globalPhi2 < -Pi / 2.0){
	    globalPhi2 = globalPhi2 + 2 * Pi;
	  }
	  tmp2 = atan2(vector2.getY(), vector2.getX());
	  if (tmp2 < -Pi / 2){
	    tmp2 = tmp2 + 2 * Pi;
	  }
	  globalAlpha2 = globalPhi2 - tmp2;
	}
      else
	{
	  globalPhi2 = -100.;
	  globalAlpha2 = -100.;
	  intercept2 = -100.;
	  slope2 = -100.;
	}
      
     // fill the ntuple
  
      int oddeven1 = 0;
      int oddeven2 = 0;

      if (even1 == 0) { // if only odd
	oddeven1 = -1;
      }else if (odd1 ==0) { // if only even
	oddeven1 = 1;
      }
      if (even2 == 0) { // if only odd
	oddeven2 = -1;
      }else if (odd2 ==0) { // if only even
	oddeven2 = 1;
      }

      if (histogrammer) {
	int m=0;
	array[m++] = zvtx;
	array[m++] = multiplicity;
	array[m++] = candidateid;
	array[m++] = sector;
	array[m++] = rpc1;
	array[m++] = ppc1;
	array[m++] = zpc1;
	array[m++] = arm;
	array[m++] = side;
	array[m++] = nx1;
	array[m++] = nx2;
	array[m++] = globalPhi1;
	array[m++] = globalPhi2;
	array[m++] = globalAlpha1;
	array[m++] = globalAlpha2;
	array[m++] = oddeven1;
	array[m++] = oddeven2;
	if (nx1) array[m++] = dist1/nx1; else array[m++] = -100.;
	if (nx2) array[m++] = dist2/nx2; else array[m++] = -100.;
	array[m++] = intercept1;
	array[m++] = intercept2;
	array[m++] = slope1;
	array[m++] = slope2;

	for (int l = 0; l<12; l++) {
	  array[m++] = t1[l];
	  array[m++] = c1[l];	
	  array[m++] = wd1[l];	
	  array[m++] = d1[l];
	  array[m++] = p1[l];
	}
	for (int l = 0; l<12; l++) {
	  array[m++] = t2[l];
	  array[m++] = c2[l];	
	  array[m++] = wd2[l];	
	  array[m++] = d2[l];
	  array[m++] = p2[l];
	}
	
	// fill the ntuple
	
	histogrammer->aligment->Fill(array);
      }
    }else {
      continue;
    }

  } // loop over candidates
}


