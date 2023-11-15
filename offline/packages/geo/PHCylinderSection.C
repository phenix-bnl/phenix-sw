// Class:  implementation of a cylinder segment that inherits from PHCylinder
//
// Written by:  Jane M. Burward-Hoy and Federica Messer

#include "PHCylinderSection.h"
#include <iostream>
using namespace std;

PHCylinderSection::PHCylinderSection(const PHPoint& point, double r,const PHVector& vector)
{
  center = point;
  axis = vector;
  radius = r;
  angleRange.setAngleRange(0.,0.);

  PHPoint origin=center;
  PHVector Zaxis=axis;
  PHVector null(0,0,0);
  PHVector Xaxis=null-Zaxis.orthogonal();
  PHVector Yaxis=Zaxis.cross(Xaxis);
  PHFrame cylFrame(origin,Xaxis,Yaxis,Zaxis);

  itsFrame=cylFrame; 

}

void PHCylinderSection::print() const
{
  PHCylinder::print();
  cout << "Phi lower: " << getPhiLower() << "  Phi upper: " << getPhiUpper() << endl;
  cout<<" CylinderSection frame "<<endl;
  itsFrame.print();
}

ostream& operator<<(ostream& os, const PHCylinderSection &cylinder)
{
  PHCylinder tmpCyl(cylinder.getCenter(),cylinder.getRadius(),cylinder.getAxis());
  os << tmpCyl << endl;
  os << "Phi lower: " << cylinder.getPhiLower() << "  Phi upper: " << cylinder.getPhiUpper();
  return os;
}

void PHCylinderSection::setPhiRange(const PHAngle &phiMin, const PHAngle &phiMax) {
  // phi range only makes sense if the frame is specified
   angleRange.setAngleRange(phiMin, phiMax);
}
  
const PHAngle& PHCylinderSection::getPhiLower()  const {
  return angleRange.getPhiMin();
}

const PHAngle& PHCylinderSection::getPhiUpper() const {
  return angleRange.getPhiMax();
}

PHBoolean PHCylinderSection::ifInsidePhiRange(const PHAngle &angle) const
{
  return angleRange.isInRange(angle);
}

void PHCylinderSection::setFrame(const PHFrame &f)

{ 
  itsFrame=f;
}
