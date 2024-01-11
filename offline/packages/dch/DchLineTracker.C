#include "DchLineTracker.hh"
#include "DchLineTrackFit.hh"
#include <PHGeometry.h>
#include <PHDchGeometryObject.h>

#include <gsl/gsl_fit.h>

#include <iostream>
#include <vector>

using namespace std;
using namespace PHGeometry;

DchLineTracker::DchLineTracker(PHDchGeometryObject* geo):
  geometry(geo),
  hitList(NULL),
  candidates(NULL),
  verbose(0)
{}

DchLineTracker::DchLineTracker(PHDchGeometryObject* geo, DchHitLineLists* list, PHPointerList<DchTrackCandidate>* candidateList):
  geometry(geo),
  hitList(list),
  candidates(candidateList),
  verbose(0)
{} 

void DchLineTracker::fitLineToXHits ()
{
  // This routine has been updated so that although it will change the
  // "xy" trajectory ofthe track, it will preserve the Z location of
  // the track as well as the "beta" angle of the track.  TKH
  // 1-29-2002. This routine operates in the Local coordinate system,
  // however, on the way out of the routine, it calls the "update"
  // routine.  This is almost fine, but does not define the track
  // frame (is that used????).  To get the latter define, it is
  // necessary to call transformLocalToGlobal.  Since the routine is
  // essentially 2D, we expect that all hit points have been moved
  // along the hit line to the optimum Z.  We then unpack and fit ONLY
  // the X,Y.  After the fit is complete we restore the candidate to a
  // full 3D description.

  long j,k,h;
  float alpha,cosal;
  short arrayLength = 40;
  double xs[arrayLength];
  double ys[arrayLength];
  double weights[arrayLength];
  float d[arrayLength];
  int numberOfXHits;
  int flag;
  float res_sq = 0.016*0.016; // "Known" resolution squared.
  int check;
  double slope_new, intercept_new, chi2_new;
  double sig_slope_new, sig_intercept_new, corr_new;
  float w,ww;
  int numFitted ;
  float ct = 3.;  // tukeys constant 
  const size_t stride = 1;

  DchTrackCandidate *mycandidate;
  DchHitLine* hit = 0;
  PHLine initialTrack;
  PHVector zax(0.,0.,1.);
  PHPoint  tmppoint;
  PHVector tmpvector;
  
  // Loop over al candidates...
  for (unsigned int i=0;i<candidates->length();i++)  {
    mycandidate = (*candidates)[i];
    short arm = mycandidate->getArm();
    alpha  =  mycandidate->getLocalAlpha();
    cosal =  fabs(cos(alpha));
      
    // Initialize the weights...
    for (j=0;j<arrayLength;j++) {
      weights[j]=0.;
      xs[j]=0.;
      ys[j]=0.;
    }

    // Get all the X hitLines and "HitScalars" of the candidate... 
    numberOfXHits = mycandidate->closestX->length();
    if (numberOfXHits > arrayLength) {
      numberOfXHits = arrayLength;
      cout << " DchLineTracker: too many hits for fit " << endl;
    }
    vector<PHLine> hitLines(numberOfXHits);
    for (h=0; h<numberOfXHits; h++) {	  
      hit        = (*(mycandidate->closestX))[h];
      tmppoint   = (PHPoint)(hit->getLocalPoint());
      tmppoint.setZ(0.);
      hitLines[h].setBasepoint(tmppoint);
      hitLines[h].setDirection(zax);
      xs[h]      = hitLines[h].getBasepoint().getX();
      ys[h]      = hitLines[h].getBasepoint().getY();
      weights[h] = 1.;
    }
      
    // starting values loaded into the "track" PHLine...
    tmppoint = (PHPoint)(mycandidate->getLocalPoint());
    tmppoint.setZ(0.);
    initialTrack.setBasepoint(tmppoint);
    tmpvector = mycandidate->getLocalVector();
    tmpvector.setZ(0.);
    tmpvector.normalize();
    initialTrack.setDirection(tmpvector);
    PHLine track = initialTrack;

    if (numberOfXHits>4) {
      for (k=0; k<5; k++)  {  // iterate with robust weights
	numFitted = 0;
	w  = 0.;  // sum of weights from previous itteration
	ww = 0.;  // weighted sum of current distances
	
	// calculate sum of weights and new distances
	// store line copies of the hits for convenient looping.
	for (h=0; h<numberOfXHits; h++) { 	  
	  d[h]       = distanceLineLine(track,hitLines[h])/cosal;
	  ww        += weights[h]*d[h]*d[h];
	  w         += weights[h];
	}
	ww /= w;	   
	if (ww < res_sq) 
	  {
	    ww = res_sq;  // Don't let the robust fitter go nuts! TKH
			  // 1-29-2001.
	  }
	
	// calculate new weights	  
	for (h=0; h<numberOfXHits; h++) {
	  if ( d[h]*d[h] < ct*ct*ww ) {
	    double val = (d[h]*d[h])/(ct*ct);
	    double diff = 1.0 - val/ww;
	    weights[h] = diff*diff;
	    numFitted++;
	  } 
	  else 	{
	    weights[h]=0.0;
	  }
	}
	
	// fit
	if (numFitted > 1) {
	  check = gsl_fit_wlinear(xs,stride,weights,stride,ys,stride,
				   numberOfXHits,&intercept_new,&slope_new,
				   &sig_intercept_new,&corr_new,&sig_slope_new,
				   &chi2_new);
	  if (check == -1) {
	    break;
	  }
	  // update track parameters
	  PHPoint  localPoint(0.,intercept_new,0.); 
	  PHVector localVector(1.,slope_new,0.);
	  track.setBasepoint(localPoint);  
	  track.setDirection(localVector); 
	}
      }
      if (numFitted < 2) { 
	break;
      }
      

      // OK, it looks crazy, but its right.  Now that the fit has
      // converged, we have a line in the 2D space which represents part
      // of the full track.  The trouble is that the basepoint of this
      // line is along the X=Z=0 line.  Thus we have to first intersect
      // the line with the reference radius and to move that point.  We can then
      // displace the resulting point (and vec) along Z to restore full dimensionality to the 
      // track.  The trouble:  The reference radius is in GLOBAL coordinates!!!
      // So, we have to do all the following:
      //    1) Make a 2D line in local.
      //    2) Change the line to global.
      //    3) Intersect the global line with the reference radius.
      //    4) extract the intersection in local coords.
      //    5) move the local coord point (and vector) in Z.
      //    6) redo the intersection.
      //    7) exit with full 3D track
      //    8) get a beer, you've earned it!
      //
      // update candidate 
      PHPoint  firstHit = hit->getBasepoint();
      PHPoint  localPoint(0.,intercept_new,0.); 
      PHVector localVector(1., slope_new, 0.);
      if (initialTrack.getDirection().getX()<0) {
	localVector.setX(-1.);
	localVector.setY(-slope_new);
      }
      localVector.normalize();

      PHPoint globalPoint  = geometry->rotateAndTranslate(arm,localPoint);   
      PHVector globalVector = geometry->rotateAndTranslate(arm,localVector);
      flag = intersectionWithReferenceRadius(arm, globalPoint, globalVector, firstHit, localPoint, localVector);
      if (!flag && verbose>9)
	{
	  cout << PHWHERE << " intersectionWithReferenceRadius failed" << endl;
	}
      // OK...Now modify the returned localPoint and localVector...
      float localZed = mycandidate->getLocalPoint().getZ();
      localPoint.setZ(localZed);

      localVector.normalize();
      float THETA  = mycandidate->getLocalBeta();
      float cosPHI = localVector.getX();
      float sinPHI = localVector.getY();
      localVector.setX(sin(THETA)*cosPHI);
      localVector.setY(sin(THETA)*sinPHI);
      localVector.setZ(cos(THETA));
      globalPoint  = geometry->rotateAndTranslate(arm,localPoint);   
      globalVector = geometry->rotateAndTranslate(arm,localVector);
	  
      flag = intersectionWithReferenceRadius(arm, globalPoint, globalVector, firstHit, localPoint, localVector);

      if(flag)    {
	globalVector.normalize();
	localVector.normalize();
	mycandidate->update(localPoint, localVector, globalPoint, globalVector);
	mycandidate->setChi2OfFit(chi2_new);
	mycandidate->setNumberOfFittedHits(numFitted);
      }
      else    {
	mycandidate = candidates->removeAt(i);
	delete mycandidate;
      }
    }
  }
}

void DchLineTracker::fitLineToAllHits() 
{
  //
  // loops over candidate list:
  // o  uses candidate's current value of basepoint and direction as an initial
  //    guess
  // o  uses a locally generated hitLinesList of type PHLine using current candidate's
  //    associated closest X and UV hits
  // o  passes this information to DchLineTrackFit::DchLineTrackFit(guess, hitLinesList)
  // o  calls DchLineTrackFit::fitTrack()
  // o  calls DchLineTrackFit::getRedChi2() which is the reduced chi2 of the resulting fit
  // o  optional:  to see fitted results, call DchLineTrackFit::print()
  //

  short arm;
  DchTrackCandidate *mycandidate;

  PHPointerList<PHLine> *hitLinesList;
  hitLinesList = new PHPointerList<PHLine>;

  for (unsigned int i=0;i<candidates->length();i++){
    hitLinesList->clear();
    mycandidate = (*candidates)[i];
    arm = mycandidate->getArm();

    int numberOfXHits = mycandidate->closestX->length();
    int numberOfUVHits = mycandidate->closestUV->length();

    if (numberOfUVHits && numberOfXHits) {
      DchHitLine *xHit;
      PHLine *xHitLine;
      PHLine xline;
      for (int h=0; h<numberOfXHits; h++) {	  
	xHit = (*(mycandidate->closestX))[h];
	xline.setBasepoint(xHit->getBasepoint());
	xline.setDirection(xHit->getDirection());
	xHitLine = new PHLine(xline);
	if (!(hitLinesList->append(xHitLine)))
	  PHMessage("DchLineTracker::fitLineToAllHits",PHError,"cannot append!");
      }
      
      DchHitLine *uvHit;
      PHLine *uvHitLine;
      PHLine uvline;
      for (int h = 0; h<numberOfUVHits; h++) {
	uvHit = (*(mycandidate->closestUV))[h];
	uvline.setBasepoint(uvHit->getBasepoint());
	uvline.setDirection(uvHit->getDirection());
	uvHitLine= new PHLine(uvline);
	if (!(hitLinesList->append(uvHitLine)))
	  PHMessage("DchLineTracker::fitLineToAllHits",PHError,"cannot append!");
      }

      PHLine guess;
      guess.setBasepoint(mycandidate->getGlobalPoint());
      guess.setDirection(mycandidate->getGlobalVector());
      
      DchLineTrackFit trackFit(guess,hitLinesList);
      if (trackFit.fitTrack()) {
	PHLine track = trackFit.getTrack();

	// update candidate 

	// 1.  move track from global to local coordinates 
	// 2.  move basepoint to reference radius by intersecting track with reference cylinder and
	//   choosing the appropriate solution

	PHLine   *myHitLine   = (*hitLinesList)[0];
	PHPoint  firstHit     = myHitLine->getBasepoint();
	PHPoint  globalPoint  = track.getBasepoint(); 
	PHVector globalVector = track.getDirection();
	PHPoint  localPoint; 
	PHVector localVector;
	globalVector.normalize();

	intersectionWithReferenceRadius(arm, globalPoint, globalVector, firstHit, localPoint, localVector);

	// 3.  update candidate 

	mycandidate->update(localPoint, localVector, globalPoint, globalVector);
	mycandidate->setChi2OfFit(trackFit.getRedChi2());
	mycandidate->setNumberOfFittedHits(trackFit.getNumOfHits());

	PHVector ZAxis(0.,0.,1.);    // needs to be corrected for true geometry
	PHVector Norm = ZAxis.cross(mycandidate->getGlobalVector());
	PHPlane tmpPlane(mycandidate->getGlobalPoint(),Norm); 
	mycandidate->setGlobalPlane(UV1Wire,tmpPlane);

      } else {
	mycandidate->setNumberOfFittedHits(trackFit.getNumOfHits());
      }
    }
  }
  
  hitLinesList->clearAndDestroy();
  delete hitLinesList;
}

int DchLineTracker::intersectionWithReferenceRadius( short arm ,PHPoint& globalPoint, PHVector& globalVector, PHPoint& hit, PHPoint& localPoint, PHVector& localVector){
  int ret=1;
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

  if (arm == (int)EAST) {
    dcArm = dcEast;
  }else{
    dcArm = dcWest; 
  }  

  PHLine track(globalPoint,globalVector);
  short numberOfCrossings = intersectionLineCylinderSection(track,*dcArm,p1,p2);
  if (numberOfCrossings == 0 )
    {
      ret = 0;
    }
  else if (numberOfCrossings == 1)
    {
      if (distancePointToPoint(hit, p1) < 100.)
	globalPoint = p1; //direction check
      else
	{
	  ret = 0;
	}
    }
  else
    {
      double d1 = distancePointToPoint(hit, p1);
      double d2 = distancePointToPoint(hit, p2);
      if (d1 >= d2)
	{
	  globalPoint = p2;
	}
      else
      {
        globalPoint = p1;
      }
  }
  localPoint  = geometry->rotateAndTranslateInverse(arm,globalPoint);   
  localVector = geometry->rotateAndTranslateInverse(arm,globalVector);
  return ret;
}

void DchLineTracker::removeDuplicateCandidates()
{
  long i,k;
  DchTrackCandidate *candidate1;
  DchTrackCandidate *candidate2;
  float alpha1,phi1;
  float alpha2,phi2;
  int numberOfRemovedCandidates = 0;

  for (i=candidates->length()-1;i>0;i--){
    candidate1 = (*candidates)[i];
    alpha1     = candidate1->getLocalAlpha();
    phi1       = candidate1->getLocalPhi();
  
    for (k=i-1;k>=0;k--){
    candidate2 = (*candidates)[k];
    alpha2     = candidate2->getLocalAlpha();
    phi2       = candidate2->getLocalPhi();
       
    if (fabs(alpha1-alpha2) < pDeltaAlpha && fabs(phi1-phi2) < pDeltaPhi) {
      candidate1 = candidates->removeAt(i);
      delete candidate1;
      numberOfRemovedCandidates++;
      break;
    }
    }
  } 
  if (verbose>9){
  cout << "DchLineTracker::removeDuplicateCandidates():  " 
       << numberOfRemovedCandidates
       << " candidates removed " <<  endl;}
}
