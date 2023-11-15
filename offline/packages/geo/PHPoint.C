// Created by: Jane M. Burward-Hoy and Federica Messer

#include <iostream>

#include "phool.h"
#include "PHPoint.h"
#include "PHGeometry.h"

using namespace std;
using namespace PHGeometry;

PHPoint::PHPoint(const PHVector &p)
{
  dx = p.getX();
  dy = p.getY();
  dz = p.getZ();
}

PHPoint::PHPoint(const PHSphPoint &sph)
{
  sphericalToCartesian(sph,*this);
}

PHPoint::PHPoint(const PHCylPoint &cyl)
{
  cylindricalToCartesian(cyl,*this);
}

PHBoolean
PHPoint::operator== (const PHPoint& p) const
{
  if (dx == p.getX() && dy == p.getY() && dz == p.getZ())
    {
      return True;
    }

  return False;
}

PHPoint &
PHPoint::operator= (const PHVector &p)
{
  dx = p.getX();
  dy = p.getY();
  dz = p.getZ();

  return *this;
}

PHPoint &
PHPoint::operator= (const PHSphPoint &sph)
{
  sphericalToCartesian(sph,*this);

  return *this;
}

PHPoint &
PHPoint::operator= (const PHCylPoint &cyl)
{
  cylindricalToCartesian(cyl,*this);
  return *this;
}

ostream &
operator<<(ostream& os, const PHPoint &p)
{
  return os << "( " << p.getX()
	    << ", " << p.getY()
	    << ", " << p.getZ() << " ) ";
}

double
PHPoint::distanceToPoint(const PHPoint &p) const
{
  return distancePointToPoint(p,*this);
}

void
PHPoint::print() const
{
  cout << "( " << dx
       << ", " << dy
       << ", " << dz << "  ) " << endl;
}
