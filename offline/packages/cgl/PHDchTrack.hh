//  Purpose: A drift chamber track model which publicly inherits from the  
//           generic PHTrack class.  Assumes tracks are curved due to      
//           magnetic field.                                               
//                                                                         
//  Created by: Jane Burward-Hoy and Steve Johnson                         
//  Purpose:  The DCH Track Model based on 4D (for momentum)and 3D         
//            (for theta0) polynomial interpolation of field               
//            integral values in a look-up "table" (read in by the         
//            PHDchTrackFitter constructor once instantiated).             
//            Needs as input:                                              
//                  o  associated hits (PHPointerList<PHLine>)             
//                  o  the track (DDCHTRACKS_ST),                          
//                  o  and the vertex (PHPoint).                           
//            Sets the following fitted variables:                         
//                  o  momentum (double)                                   
//                  o  phi0, theta0, beta, delta angles (double)           

#ifndef PHDCHTRACK_H
#define PHDCHTRACK_H

#include <phool.h>
#include <DchAnaPar.h>
#include <PHGeometry.h>
#include <PHTrack.h>
#include <cglDetectorGeo.hh>
#include <dDchTracksWrapper.h>

class PHDchTrackFitter;
class dDchTracks;

class PHDchTrack : public PHTrack 
{
public: 

  PHDchTrack(); 
  virtual ~PHDchTrack(){}

  PHDchTrack(long &index, 
	     dDchTracksWrapper* tracksWrapper,
	     const PHPointerList<PHLine> &DchHits,
	     const PHPoint &vtx,
	     const cglDetectorGeo &cglDetGeo);
  PHBoolean fitTrack(int &robustFlag, int &maxIterations);
  PHLine getFitTrack() const { return fitterTrack; }
  double getFitP() const  { return fitterP; }
  void setFitP(const double &p)  { fitterP = p; }
  double getFitPhi0() const  { return fitterPhi0; }
  void setFitPhi0(const double &phi0)  { fitterPhi0 = phi0; }
  double getFitAlpha() const {return fitterAlpha;}
  double getFitPhi() const {return fitterPhi;}
  double getFitTheta0() const  { return fitterTheta0; }
  void setFitTheta0(const double &theta0)  { fitterTheta0 = theta0; }
  double getFitBeta() const  { return fitterBeta;}
  double getFitDelta() const  { return fitterDelta; }
  double getChi2() const { return chisqr;}
  PHBoolean ifMethodGuess() const {return ifGuess;}
  PHBoolean getFitError() const {return fitError;}
  int getFitPErr() const {return errFitP;}
  PHBoolean ifTheta0Error() const { return theta0Error;}
  int getFitThet0Err() const {return errFitTheta0;}

public:
  PHVector predictMomentum();
  PHBoolean projectToVertex(PHLine &proj, PHPoint &error);
  PHBoolean projectToDch(PHLine&, PHPoint&);
  PHBoolean projectToPc1(PHLine&, PHPoint&);
  PHBoolean projectToPc2(PHLine&, PHPoint&);
  PHBoolean projectToPc3(PHLine&, PHPoint&);
  PHBoolean projectToTec(PHLine&, PHPoint&);
  PHBoolean projectToPbSc(PHLine&, PHPoint&);
  PHBoolean projectToPbGl(PHLine&, PHPoint&);
  PHBoolean projectToCrk(PHLine&, PHPoint&);
  PHBoolean projectToTof(PHLine&, PHPoint&);
  PHBoolean projectToAcc(PHLine&, PHPoint&);
  PHBoolean projectToTofw(PHLine&, PHPoint&);
  PHBoolean projectToSvx(short ilayer, PHLine&, PHPoint&);
  PHBoolean projectToHbd(PHLine&, PHPoint&);

  double pathLengthToTof();
  double pathLengthToCrk();
  double pathLengthToEmc();
  double pathLengthToTofw();

  double getPathLengthToTof() const { return tofPathLength;}
  double getPathLengthToCrk() const { return crkPathLength;}
  double getPathLengthToEmc() const { return emcPathLength;}
  double getPathLengthToTofw() const { return tofwPathLength;}

  int getPc1Sector() const { return pc1sector;}
  int getPc2Sector() const { return pc2sector;}
  int getPc3Sector() const { return pc3sector;}
  int getTecSector() const { return tecSector;}
  int getPbscSector() const { return pbscSector;}
  int getPbglSector() const { return pbglSector;}
  int getAccSector() const { return accSector;}
  int getTofwSector()const  { return tofwSector;}
  int getSvxLadder(const short ilayer) const { return svxladder[ilayer];}
  int getHbdSector() const { return hbdSector;}
  const cglDetectorGeo& getCglDetGeo() const {return *detGeo;}  

  // NEEDS TESTING/MORE WORK
  void calcPolyLine();
  PHBoolean projectToPlane(const PHPlane &plane, PHLine &proj);
  short projectToCylinder(const PHCylinder &cyl, PHLine &proj1, PHLine &proj2);
  short projectToSphere(const PHSphere &sphere, PHLine &proj1, PHLine &proj2);

  void printProjections() const;
  void printPolyLine() const;

  PHPoint getVertex() const    { return vertex;}
  short getArm() const   {return arm;} 
  PHLine getTrack() const  { return track;}  
  short getSide() const { return side;}
  double getTheta() const { return theta;}
  double getBeta()  const;               
  double getDelta() const {return (getBeta()-getTheta());}
  double getRefRadius() const {return refRadius;}
  short getCharge() const {return charge;}
  double getAlpha() const { return alpha; }
  double getMomentumGuess() const {return pGuess;}             
  double getTheta0Guess() const {return theta0Guess;}
  void setTrack(const PHLine &line) {track = line;}
  void setArm(const short a) {arm = a;} 
  void setVertex(PHPoint &vtx) { vertex = vtx;}
  void setCharge();
  void setAlpha(const double &a) { alpha = a; }
  void setMomentumGuess(const double &p) { pGuess = p; }
  void setTheta0Guess(const double &theta0) {theta0Guess = theta0;}
  void setTukeyConstant(const float &t) { tukeyConstant = t;}
  float getTukeyConstant() const { return tukeyConstant;}
  int getSuccessfulIterations() const { return successfulIterations;}
  long getNumberOfFittedHits() const { return numFitted;}
  long getNumOfHits() const { return numOfHits; }

private:
  void copyTrackInfo(const DDCHTRACKS_ST &);
  void copyHitInfo(const PHPointerList<PHLine> &);
  void setDefaultPolyOrders();
  void setPolyOrders(const int &);
  void setGuessP();
  PHBoolean calcFitTrack(PHLine &);
  double calcAlpha(const PHLine &) const;
  double calcPhi(const double &, const double &) const;
  double calcTheta(const double &, const double &) const;
  PHBoolean leastSqrsFitHits(const double &p,
			     const double &theta0,
			     double &fitP, 
			     double &fitPhi0); 
  PHBoolean robustFitHits(const double &p,
			  const double &phi0,
			  const double &theta0,
			  double &fitP, 
			  double &fitPhi0);
  PHBoolean fitTheta0(const double &p,
		      const double &theta0,
		      double &fitTheta0,
		      double &fitBeta,
		      double &fitDelta);
  PHBoolean fitTrackP(const int &robustFlag, 
		      const int &maxIterations,
		      const double &p, 
		      const double &theta0,
		      double &fitP,
		      double &fitPhi0);
  PHBoolean calcTrackPoint(const double &zvtx,
			   const double &p,
			   const double &phi0,
			   const double &r,
			   const double &theta,
			   const double &theta0,
			   PHPoint &predictPoint);
  PHBoolean calcTrackSection(const double &innerR,
			     const double &outerR,
			     const int &numPoints);

private:
  short side;
  PHPoint vertex;

  short charge;
  PHLine track, fitterTrack;
  long numOfHits,numFitted;
  PHLine trackHits[2*numberOfPlanes];
  double hitR[2*numberOfPlanes];
  double hitPhi[2*numberOfPlanes];
  double weights[2*numberOfPlanes];

  double refRadius;
  double pGuess,fitterP,theta0Guess,theta;   
  double alpha,fitterAlpha,fitterPhi,fitterPhi0;
  double fitterBeta,fitterTheta0, fitterDelta;
  PHBoolean fitError, ifGuess, theta0Error;
  double chisqr;
  int errFitP, errFitTheta0;
  int rOrderF, pOrderF, theta0OrderF, zOrderF;
  int rOrderDelta, pOrderDelta, theta0OrderDelta, zOrderDelta;
  int rOrderG, pOrderG, theta0OrderG, zOrderG;

  int successfulIterations;
  float tukeyConstant;

  PHDchTrackFitter *fitter;

  const cglDetectorGeo *detGeo;  

  int pc1sector, pc2sector, pc3sector;
  int tecSector;
  int tofSector;
  int accSector;
  int pbglSector, pbscSector;
  int tofwSector;
  int svxladder[SVXLAYERNUMBER];
  int hbdSector;

  double tofPathLength;
  double emcPathLength;
  double crkPathLength;
  double tofwPathLength;
}; 

#endif /* PHDCHTRACK_H */ 






