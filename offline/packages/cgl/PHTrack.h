#ifndef __PHTRACK_H__
#define __PHTRACK_H__

// Description: Base class for PHENIX track models
// Details: This is a base class whose purpose is to define the
// functionality that must be provided by PHENIX track models.

#include <phool.h>
#include <PHVector.h>
#include <PHPolyLine.h>

class PHPlane;
class PHPoint;
class PHLine;
class PHCylinder;
class PHSphere;

#define SVXLAYERNUMBER 4

class PHTrack 
{ 
public:
  PHTrack();
  virtual ~PHTrack();
  
  // Methods that will return the coordinate and direction vector of
  // the track model at a given detector as a PHLine.  The methods
  // must also return the errors in the point prediction as a PHPoint.
  // The errors will be used by the hit association to determine the
  // window width.  The detector geometry is defined from the geometry
  // database.  For example, PC1 consists of 8 PHPlanes in each arm.
  virtual PHBoolean projectToVertex(PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToDch   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToPc1   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToPc2   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToPc3   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToCrk   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToTec   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToTof   (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToPbSc  (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToPbGl  (PHLine&, PHPoint&) = 0;
  virtual PHBoolean projectToSvx   (const short ilayer, PHLine&, PHPoint&) = 0;
  // Make these methods non-pure virtual - it's not the right thing to
  // do, but consider it a patch for the time being.  The right thing
  // is to replace all these detector specific projectToXXX methods
  // with one generic projectToDetector(const PHLine &, const detector
  // &, PHPoint &) method that accepts a detector object as an
  // argument.
  virtual PHBoolean projectToAcc   (PHLine &l, PHPoint &p) {return False;}
  virtual PHBoolean projectToTofw  (PHLine &l, PHPoint &p) {return False;}
  virtual PHBoolean projectToHbd  (PHLine &l, PHPoint &p) {return False;}

  // Project track to arbitrary geometry.  No errors required here.
  virtual PHBoolean projectToPlane(const PHPlane &plane, PHLine&);
  virtual short projectToCylinder(const PHCylinder &cyl, PHLine&, PHLine&);
  virtual short projectToSphere(const PHSphere &sphere, PHLine&, PHLine&);

  // This method must be supplied, but its output is optional.  Return
  // a null result if the momentum is not predicted.
  virtual PHVector predictMomentum() = 0;

  // Return the path length for timing detectors
  virtual double pathLengthToCrk() = 0;
  virtual double pathLengthToTof() = 0;
  virtual double pathLengthToEmc() = 0;
  virtual double pathLengthToTofw() = 0;
  // Same argument as above for making this late-comer non-pure virtual.

  // This method must be supplied primarily for event display
  // purposes.  Detail must be provided inside the dch outer radius
  virtual void calcPolyLine() = 0;

  // data member access methods
  PHPoint getPolyLinePoint(const int &i) {
    return *(polyLine.getPoint(i));
  }

  // to access projections and directions must loop over ifIntersect list
  PHPoint getProjectionPoint(const int &i) { 
    return *(projections.getPoint(i));
  }
  PHPoint getProjectionError(const int &i) { 
    return *(projErrors.getPoint(i));
  }
  PHVector getDirectionVector(const int &i) { 
    return *(directions[i]);
  }
  PHBoolean getIfIntersectFlag(const int &i) { 
    return *(ifIntersect[i]);
  }
  size_t getIfIntersectLength() { 
    return ifIntersect.length();
  }

  long getTrackIndex() const { return trackIndex; }
  void setTrackIndex(const long &ind) {trackIndex = ind; }

  short getArm() const { return arm; }
  void setArm(const short &iarm) {arm = iarm; }

  virtual void callProjections();

protected:
  PHPolyLine polyLine;     // Polyline describing the shape of the track
  long trackIndex;         // Pointer to the dCglTrack, if constructed
  short arm;               // Arm number

  PHPolyLine projections;               // List of calculated projections
  PHPolyLine projErrors;                // List of projection errors
  PHPointerList<PHVector> directions;   // List of vectors at projections
  PHPointerList<PHBoolean> ifIntersect; // Flags for intersection tests
}; 

#endif /* __PHTRACK_H__ */








