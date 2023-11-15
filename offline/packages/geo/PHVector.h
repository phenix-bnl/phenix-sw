#ifndef __PHVECTOR_H__
#define __PHVECTOR_H__

// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose:  A vector in Cartesian Coordinates

#include <iosfwd>

#ifdef __CINT__
#include <math.h>
#else
#include <cmath>
#endif

#include <PHPoint.h>

class PHVector
{
public:
  PHVector(double t = 0.0) :
    dx(t),
    dy(t),
    dz(t) {}
  PHVector(double x, double y, double z) :
    dx(x),
    dy(y),
    dz(z) {}

  PHVector(const PHPoint& p) :
    dx(p.getX()),
    dy(p.getY()),
    dz(p.getZ()) {}
  virtual ~PHVector() {}

  PHVector& operator=(const PHPoint &p) {
    dx = p.getX();
    dy = p.getY();
    dz = p.getZ();
    return *this;
  }
  PHVector operator-() const { return PHVector(-dx, -dy, -dz); }
  PHVector operator-(const PHVector &p) const { return PHVector(dx-p.dx, dy-p.dy, dz-p.dz); }
  PHVector operator+(const PHVector &p) const { return PHVector(dx+p.dx, dy+p.dy, dz+p.dz); }
  PHVector operator*(const double &a) const { return PHVector(dx*a, dy*a, dz*a); }

  double dot(const PHVector& rhs) const {
    return
      getX() * rhs.getX() +
      getY() * rhs.getY() +
      getZ() * rhs.getZ();
  }
  double   angle(const PHVector &) const;
  PHVector cross(const PHVector &) const;
  PHVector orthogonal() const;

  double length() const {
    return sqrt(dx * dx + dy * dy + dz * dz);
  }

  double lengthSqr() const {
    return dx * dx + dy * dy + dz * dz;
  }

  // return normalized vector colinear to this one
  PHVector normalized() const
  {
    PHVector out( *this );
    out.normalize();
    return out;
  }

  void normalize() {
    // I inlined this method because it's near the top of the CPU
    // charts.  To do that I had to #include <cmath> to pick up sqrt.
    // And once you've done that you might as well inline length() and
    // lengthSqr().
    double l = sqrt(dx * dx + dy * dy + dz * dz);

    if (l == 0.0) return;

    dx = dx / l;
    dy = dy / l;
    dz = dz / l;
  }

  void print() const;

  friend std::ostream& operator<<(std::ostream& ,const PHVector&);

  void setX(double x) {dx = x;}
  void setY(double y) {dy = y;}
  void setZ(double z) {dz = z;}
  void setXYZ(double x, double y, double z) { dx = x; dy = y; dz = z; }

  double getX() const {return dx;}
  double getY() const {return dy;}
  double getZ() const {return dz;}

protected:

  double dx,dy,dz;

};

#endif /* __PHVECTOR_H__ */
