// Class:  includes PHPlane header
// Created by:  Jane M. Burward-Hoy and Federica Messer

#include "PHPlane.h"
#include "PHLine.h"
#include <iostream>

using namespace std;

PHPlane::PHPlane(const PHPoint &p, const PHVector &v)
{
  origin = p;
  normal = v;
  normal.normalize();
 
}

PHPlane::PHPlane(const PHLine& line)
{
  origin = line.getBasepoint();
  normal = line.getDirection();
  normal.normalize();
}

// Constructor of a plane from 3 points:
// Original by J. Mitchell on 12/21/99
PHPlane::PHPlane(const PHPoint &p1, 
		 const PHPoint &p2, 
		 const PHPoint &p3)
{
  // Nice as the original tests in this routine might have seemed,
  // they just weren't sensible when you're dealing with floating
  // point values.  I think the right thing to do is test the
  // magnitude of vector1 and vector2 to be sure that each is bigger
  // than some (probably machine-dependent) minimum value.  Short of
  // that, the tests didn't really accomplish anything.  Since this
  // routine appears high on the list of CPU users, I've chosen to
  // discard them.

  PHVector vector1 = PHVector(p2 - p1);
  PHVector vector2 = PHVector(p3 - p1);

  origin = p1;
  normal = vector1.cross(vector2);
  normal.normalize();
}

PHPlane& 
PHPlane::operator=(const PHLine &rhs)
{
  origin = rhs.getBasepoint();
  normal = rhs.getDirection();
  normal.normalize();
  return *this;
}

void PHPlane::print() const
{
  cout << "PHPlane: Origin = " << origin 
       << "  Normal = " << normal << endl;
}

ostream& operator<<(ostream& os, const PHPlane& plane)
{
  os << " PHPlane: Origin = " << plane.getOrigin() 
     << "  Normal = " << plane.getNormal();
  return os;
}

void PHPlane::setNormal(const PHVector &v)
{
  normal = v;
  if ( (normal.length()!=1) && (normal.length()!=0.0) ) {
    normal.normalize();
  }
}

double PHPlane::getD() const
{
  PHVector v;
  v = origin;
  return (-normal.dot(v));
}


  


