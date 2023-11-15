#ifndef PHSPHERESECTION_H
#define PHSPHERESECTION_H

// Class: PHSphereSection header
//
// Written by: Kenta Shigaki
//
// Purpose: A Sphere representation (Cartesian Coordinates).
// 
// Based on PHSphere header by Jane M. Burward-Hoy and Federica Messer
//
// Details: the sphere is defined by a center (PHPoint) and a radius
// (double)

#include "PHSphere.h"
#include "PHAngle.h"
#include "PHPoint.h"

#include <iosfwd>

class PHSphereSection : public PHSphere {

 public:

  PHSphereSection () {}
  PHSphereSection (const PHPoint&, const double radius);
  virtual ~PHSphereSection() {}
  
  void setPhiRange (const PHAngle &, const PHAngle &);
  void setThetaRange (const PHAngle &, const PHAngle &);

  const PHAngle& getPhiLower () const;
  const PHAngle& getPhiUpper () const;
  const PHAngle& getThetaLower () const;
  const PHAngle& getThetaUpper () const;

  void print () const;

  friend std::ostream& operator << (std::ostream &, const PHSphereSection&);

  PHBoolean ifInsidePhiRange (const PHAngle &) const;
  PHBoolean ifInsideThetaRange (const PHAngle &) const;
  PHBoolean ifInsideRange (const PHAngle &, const PHAngle &) const;

 private:

  PHAngleRange phiRange;
  PHAngleRange thetaRange;

};

#endif /* PHSPHERESECTION_H */
