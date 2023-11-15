// Class:  PHCylPoint.C 
// Created by:  Jane M. Burward-Hoy and Federica Messer

//INCLUDECHECKER: Removed this line: #include <cmath>
//INCLUDECHECKER: Removed this line: #include "PHPoint.h"
#include "PHGeometry.h"

#include <iostream>
using namespace std;

PHCylPoint::PHCylPoint()
{
  r  = 0;
  z  = 0;
  phi = 0;
}

PHCylPoint::PHCylPoint(const double r0, const double  phi0, const double z0)
{
  r     = r0;
  phi   = phi0;
  z     = z0;
}

PHCylPoint::PHCylPoint(const double r0, const PHAngle& phi0, const double z0)
{
   r   = r0;
   phi = phi0;
   z   = z0;
}

PHCylPoint::PHCylPoint(const PHPoint &rhs)
{
  PHGeometry::cartesianToCylindrical(rhs, *this);
}

PHCylPoint::PHCylPoint(const PHSphPoint &rhs)
{
  PHGeometry::sphericalToCylindrical(rhs, *this);
}

void 
PHCylPoint::print() const
{
  cout << " ( " << r << ", " << phi << ", " << z << " ) " << endl;
}

double 
PHCylPoint::distanceToPoint(const PHCylPoint &sp) const
{
  PHPoint cart = *this;
  PHPoint cart2 = sp;

  return ( PHGeometry::distancePointToPoint(cart2,cart));
}

PHBoolean 
PHCylPoint::operator== (const PHCylPoint& p) const
{
  if (r == p.getR() && phi == p.getPhi() && z == p.getZ()) 
    {
      return True;
    }

  return False;
}

PHCylPoint& 
PHCylPoint::operator= (const PHPoint &rhs)
{
  PHGeometry::cartesianToCylindrical(rhs, *this);

  return *this;
}

PHCylPoint& 
PHCylPoint::operator= (const PHSphPoint &rhs)
{
  PHGeometry::sphericalToCylindrical(rhs, *this);

  return *this;
}

ostream& 
operator<<(ostream& os, PHCylPoint &point)
{
  os << " ( " << point.getR() << ", " << point.getPhi() << ", " << point.getZ() << " ) " ;

  return os;
}
