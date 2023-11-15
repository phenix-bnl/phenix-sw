#ifndef __PHCYLINDER_H__
#define __PHCYLINDER_H__

// Written by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose:  A Cylinder representation (Cartesian Coordinates)
// 
// Last update:  10/28/99
//
// Pioneered by  J. Mitchell
//
// Details: A cylinder is defined by a center (PHPoint), a radius (double) and an axis
//          vector (PHVector) that is not normalized.
//          The total length of the cylinder is 2 times the length of the axis vector.

#include "PHPoint.h"
#include "PHVector.h"

#include <iosfwd>

class PHCylinder
{
public:
 
  PHCylinder();
  PHCylinder(const PHPoint&, const double radius, const PHVector&);  
  virtual ~PHCylinder() {}
  
  void setCenter(const PHPoint& val) { center = val;}
  void setAxis(const PHVector& val)  { axis   = val;}
  void setRadius(const double val)   { radius = val;}
  
  double getRadius()  const { return radius;}
  PHPoint getCenter() const { return center;}
  PHVector getAxis()  const { return axis;}

  void print() const; 
  friend std::ostream& operator<<(std::ostream& ,const PHCylinder&);

  double length() const { 
    return 2.0 * axis.length(); 
  }

protected:
  double radius;
  PHVector axis;
  PHPoint center;
  
};

#endif /* __PHCYLINDER_H__ */
