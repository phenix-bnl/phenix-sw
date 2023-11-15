// Class:  PHCylinder 
//
// Written by:  Jane M. Burward-Hoy and Federica Messer

#include "PHCylinder.h"
#include <iostream>
using namespace std;

PHCylinder::PHCylinder()
{
  radius = 1;
  axis.setZ(1);
}

PHCylinder::PHCylinder(const PHPoint& point, double r, const PHVector& vector)
{
  radius = r;
  center = point;
  axis = vector;
}

void PHCylinder::print() const
{
  cout << "Radius : " << radius << " Center: " << center << " Axis: "  << axis << endl;      
}

ostream& operator<<(ostream& os, const PHCylinder &cylinder)
{
  os << "Radius : " << cylinder.getRadius() << " Center: " << cylinder.getCenter() << " Axis: " << cylinder.getAxis() << endl;
  return os;
}






