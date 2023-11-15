#ifndef PHCYLINDERSECTION_H
#define PHCYLINDERSECTION_H

// Class:  PHCylSection header
//
// Written by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose: A cylindrical segment that publicly inherits from
// PHCylinder class.
// 
// Details: A cylinder segment is defined not only by a center
// (PHPoint), a radius (double) and an unnormalized axis vector
// (PHVector, usually "z"), but is also bounded in phi by lower and
// upper phi angles (two PHAngles).
//
//          The total length of the cylindrical segment is 2 times the
//          length of the axis vector (as defined in PHCylinder).
//  
// 02/08/00 Added a frame as a data member to make transformations
// easier J.Velkovska

#include "PHCylinder.h"
#include "PHAngle.h"
#include "PHPoint.h"
#include "PHVector.h"
#include "PHFrame.h"

#include <iosfwd>

class PHCylinderSection : public PHCylinder {

 public:
  
  PHCylinderSection() {}  
  PHCylinderSection(const PHPoint&,double,const PHVector&);	
  virtual ~PHCylinderSection() {}

  void setPhiRange(const PHAngle &phiMin,const PHAngle &phiMax);
  
  const PHAngle& getPhiLower()  const;
  const PHAngle& getPhiUpper() const;
  PHFrame getFrame() const {return itsFrame;} 
  void setFrame(const PHFrame &);
  
  void print() const; 
  friend std::ostream& operator<<(std::ostream& ,const PHCylinderSection &);

  PHBoolean ifInsidePhiRange(const PHAngle&) const;
  
 private:
  PHAngleRange angleRange;
  PHFrame itsFrame;  
};

#endif /* PHCYLINDER_H */


