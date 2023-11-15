// Class:  PHSphPoint.C 
//
// Created by:  Jane M. Burward-Hoy and Federica Messer

//INCLUDECHECKER: Removed this line: #include "PHSphPoint.h"
#include "PHGeometry.h"
#include <iostream>

using namespace std;

PHSphPoint::PHSphPoint()
{
  r     = 0;
  phi   = 0;
  theta = 0;
}

PHSphPoint::PHSphPoint(const double r0,  const double  phi0, const double theta0)
{
  r     = r0;
  phi   = phi0;
  theta = theta0;
}

PHSphPoint::PHSphPoint(const double r0, const PHAngle& phi0, const PHAngle& theta0)
{
   r = r0;
   phi = phi0;
   theta = theta0;
}

PHSphPoint::PHSphPoint(const PHPoint &rhs)
{
  PHGeometry::cartesianToSpherical(rhs,*this);

}

PHSphPoint::PHSphPoint(const PHCylPoint &rhs)
{
  PHGeometry::cylindricalToSpherical(rhs,*this);
}

void PHSphPoint::print() const
{
  cout << " ( " << r << ", " << phi << ", " << theta << " ) " << endl;
}

ostream& operator<< (ostream& os , PHSphPoint& point)
{
  os << " ( " << point.getR()<< ", " << point.getPhi() << ", " << point.getTheta() << " ) " ;
  return os;
}

double PHSphPoint::distanceToPoint(const PHSphPoint &sp) const
{
  PHPoint cart = *this;
  PHPoint cart2 = sp;
  return ( PHGeometry::distancePointToPoint(cart2,cart));
}

PHBoolean PHSphPoint::operator== (const PHSphPoint& p) const
{
  if (r == p.getR()&& phi == p.getPhi() && theta == p.getTheta()) {
    return True;
  }else {
    return False;
  }
}

PHSphPoint& PHSphPoint::operator= (const PHPoint &rhs)
{
  PHGeometry::cartesianToSpherical(rhs,*this);
  return *this;
}

PHSphPoint& PHSphPoint::operator= (const PHCylPoint &rhs)
{
  
  PHGeometry::cylindricalToSpherical(rhs,*this);
  return *this;
}

