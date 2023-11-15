#ifndef PHSPHERE_H
#define PHSPHERE_H

// Class:  PHSphere header
//
// Written by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose:  A Sphere representation (Cartesian Coordinates).
// 
// Pioneered by  J. Mitchell
//
// Details: the sphere is defined by a center (PHPoint) and a radius
// (double)

#include "PHPoint.h"

#include <iosfwd>

class PHSphere
{
public:
 
  PHSphere();
  PHSphere(const PHPoint&, const double radius);  
  virtual ~PHSphere() {}
  
  void setCenter(PHPoint& val) { center = val;}
  void setRadius(const double val)   { radius = val;}
  
  double  getRadius()  const  { return radius;}
  PHPoint getCenter() const { return center;}

  void print() const; 
  friend std::ostream& operator<<(std::ostream& ,const PHSphere&);

protected:
  double radius;
  PHPoint center;
  
};

#endif /* PHSPHERE_H */


