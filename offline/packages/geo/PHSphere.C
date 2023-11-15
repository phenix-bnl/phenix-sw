// Class:  PHSphere 
//
// Written by:  Jane M. Burward-Hoy and Federica Messer

#include "PHSphere.h"
#include <iostream>

using namespace std;

PHSphere::PHSphere()
{
  radius = 1;
}

PHSphere::PHSphere(const PHPoint& point, const double r)
{
  radius = r;
  center = point;
}


 void 
PHSphere::print() const
{
  cout << "Radius : " << radius << " Center: " << center << endl;      
}

ostream& 
operator<<(ostream& os, const PHSphere &cylinder)
{
  os << "Radius : " << cylinder.getRadius() << " Center: " << cylinder.getCenter() << endl;
  return os;
}



