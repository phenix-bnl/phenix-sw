// Class:  implementation of a sphere segment that inherits from PHSphere
//
// Written by:  Kenta Shigaki

#include "PHSphereSection.h"
#include <iostream>

using namespace std;

PHSphereSection::PHSphereSection (const PHPoint& point, const double r)
  : PHSphere (point, r)  {}


void 
PHSphereSection::print() const {
  PHSphere::print();
  cout << "Phi min: " << getPhiLower().degree()
       << "  Phi max: " << getPhiUpper().degree()
       << "  Theta min: " << getThetaLower().degree()
       << "  Theta max: " << getThetaUpper().degree()
       << endl;
}

ostream& operator<<(ostream& os, const PHSphereSection &Sphere) {
  PHSphere tmpSphe(Sphere.getCenter(),Sphere.getRadius());
  os << tmpSphe << endl;
  os << "Phi lower: " << Sphere.getPhiLower()
     << "  Phi upper: " << Sphere.getPhiUpper();
  return os;
}

void PHSphereSection::setPhiRange
(const PHAngle &phiMin, const PHAngle &phiMax) {
  phiRange.setAngleRange (phiMin, phiMax);
}
const PHAngle& PHSphereSection::getPhiLower () const {
  return phiRange.getPhiMin();
}
const PHAngle& PHSphereSection::getPhiUpper () const {
  return phiRange.getPhiMax();
}

void PHSphereSection::setThetaRange
(const PHAngle &thetaMin, const PHAngle &thetaMax) {
  thetaRange.setAngleRange (thetaMin, thetaMax);
}
const PHAngle& PHSphereSection::getThetaLower()  const {
  return thetaRange.getPhiMin();
}
const PHAngle& PHSphereSection::getThetaUpper() const {
  return thetaRange.getPhiMax();
}

PHBoolean PHSphereSection::ifInsidePhiRange (const PHAngle &angle) const {
  return phiRange.isInRange(angle);
}

PHBoolean PHSphereSection::ifInsideThetaRange (const PHAngle &angle) const {
  return thetaRange.isInRange(angle);
}

PHBoolean PHSphereSection::ifInsideRange
(const PHAngle &phi, const PHAngle &theta) const {
  return (ifInsidePhiRange (phi) && ifInsideThetaRange (theta));
}
