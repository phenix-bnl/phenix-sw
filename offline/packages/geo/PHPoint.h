#ifndef PHPOINT_H
#define PHPOINT_H

// Class:  PHPoint header
// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose: A Point in Cartesian Coordinates

#include <iosfwd>
#include "phool.h"

class PHCylPoint;
class PHSphPoint;
class PHVector;

class PHPoint
{
public:
  PHPoint() {
      dx = 0;
      dy = 0;
      dz = 0;
    }

  PHPoint(const double x, const double y, const double z) {
    dx = x;
    dy = y;
    dz = z;
  }

  PHPoint(const PHSphPoint &);
  PHPoint(const PHCylPoint &);
  PHPoint(const PHVector &);
  virtual ~PHPoint() {}
  

  PHPoint& operator= (const PHSphPoint &);
  PHPoint& operator= (const PHCylPoint &);
  PHPoint& operator= (const PHVector &); 

  PHBoolean operator== (const PHPoint&) const ;

  PHPoint operator- (const PHPoint &p) const {
    PHPoint newPoint(dx - p.getX(), dy - p.getY(), dz - p.getZ());
    return newPoint;
  }

  PHPoint operator* (const double &a) const {
    PHPoint newPoint(dx * a, dy * a, dz * a);
    return newPoint;
  }

  PHPoint operator+ (const PHPoint &p) const {
    PHPoint newPoint(dx + p.getX(), dy + p.getY(), dz + p.getZ());
    return newPoint;
  }

  double distanceToPoint(const PHPoint &) const;

  void print() const; 

  friend std::ostream& operator<<(std::ostream&, const PHPoint&);

  void setX(const double x) {dx = x;}
  void setY(const double y) {dy = y;}
  void setZ(const double z) {dz = z;}
  void setXYZ(const double x, const double y, const double z) { dx = x; dy = y; dz = z; }

  double getX() const {return dx;}
  double getY() const {return dy;}
  double getZ() const {return dz;}

protected:
  double dx,dy,dz;

};

#endif /* PHPOINT_H */


