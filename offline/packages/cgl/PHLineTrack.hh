#ifndef PHLINETRACK_HH
#define PHLINETRACK_HH

// Created by: Julia Velkovska 
// Purpose: Line track model to be used for B=0 tracks 

#include <phool.h>
#include <PHLine.h>
#include <PHVector.h>
#include <PHTrack.h>

class PHSphere;
class PHPanel;
class cglDetectorGeo;
class dDchTracksWrapper;

class PHLineTrack : public PHTrack {
  
public:
  // constructors - line track has a line - so it will have all
  // constructors that a PHLine has
  PHLineTrack();
  PHLineTrack(const PHPoint &,const PHVector &);
  PHLineTrack(const PHPoint &, const PHPoint  &);
  PHLineTrack(const PHLine &);
  PHLineTrack(unsigned long &index, dDchTracksWrapper *, cglDetectorGeo &cglDetGeo);
  virtual ~PHLineTrack() {}
  
 public:
  
  // Methods that will return the coordinate and direction
  // vector of the track model at a given detector as a PHLine.  
  // The methods must also return the errors in the point prediction
  // as a PHPoint.  The errors will be used by the hit association to 
  // determine the window width.  The detector geometry is defined from 
  // the geometry database.  For example, PC1 consists of 8 PHPlanes in 
  // each arm.
  
  PHBoolean projectToVertex(PHLine&, PHPoint&);
  PHBoolean projectToDch(PHLine&, PHPoint&);
  PHBoolean projectToPc1(PHLine&, PHPoint&);
  PHBoolean projectToPc2(PHLine&, PHPoint&);
  PHBoolean projectToPc3(PHLine&, PHPoint&);
  PHBoolean projectToCrk(PHLine&, PHPoint&);
  PHBoolean projectToTec(PHLine&, PHPoint&);
  PHBoolean projectToTof(PHLine&, PHPoint&);
  PHBoolean projectToPbSc(PHLine&, PHPoint&);
  PHBoolean projectToPbGl(PHLine&, PHPoint&);
  PHBoolean projectToAcc(PHLine&, PHPoint&);
  PHBoolean projectToTofw(PHLine&, PHPoint&);
  PHBoolean projectToSvx(const short ilayer, PHLine&, PHPoint&);
  PHBoolean projectToHbd(PHLine&, PHPoint&);
  
  // Project the track to any arbitrary geometry.
  // No errors required here.
  
  PHBoolean projectToPlane(const PHPlane &plane, PHLine&);
  PHBoolean projectToPanel(const PHPanel &panel, PHLine&);
  short projectToCylinder(const PHCylinder &cyl, PHLine&, PHLine&);
  short projectToSphere(const PHSphere &sphere, PHLine&, PHLine&);
  
  // This method must be supplied, but its output is optional.
  // Return a null result if the momentum is not predicted.
  
  PHVector predictMomentum();
  
  // Return the path length for timing detectors
  
  double pathLengthToCrk();
  double pathLengthToTof();
  double pathLengthToEmc();
  double pathLengthToTofw();
  
  // This method must be supplied primarily for event display
  // purposes.  Detail must be provided inside the dch outer radius
  
  void calcPolyLine();
  
  // ----------------- in addition to polyline this track has a line
 private:
  PHLine itsLine; 
  cglDetectorGeo *refGeo;  // The detector geometry for all projections
  int verbosity;  

 public:
  PHLine getLine()const {return itsLine;}

  //  set the track from an arbitrary line or from the drift chamber track
  void setLine(PHLine &line){ itsLine=line;} 
  void setLine( int,dDchTracksWrapper * );

  // Set the reference arm
  void set_arm(short theArm){arm=theArm;}

  // get the reference arm
  short get_arm(){return arm;}

  // Set the reference detector geometry
  void set_refGeo(cglDetectorGeo * inGeo) {refGeo=inGeo;} 

  // Get the reference detector geometry
  cglDetectorGeo* get_refGeo() {return refGeo;}    

  void setVerbose(int v){verbosity=v;}
  int getVerbose(){return verbosity;}
}; 

#endif /* __PHLINETRACK_HH__ */





