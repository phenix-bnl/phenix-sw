#ifndef PHPLANE_H
#define PHPLANE_H

// Class:  PHPlane header
// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose:  A plane representation  (Cartesian Coordinates).
// Description:
//           A plane is defined by a local coordinate origin point
//           and a unit vector NORMAL to the plane.  The equation
//           of a plane is Ax + By + Cz + D = 0, where A, B and C 
//           are the components of the unit vector normal to
//           the plane.

#include <iosfwd>
#include "PHPoint.h"
#include "PHVector.h"

class PHLine;

class PHPlane
{
public:
  PHPlane() {}
  PHPlane(const PHPoint &, const PHVector &);
  PHPlane(const PHPoint &, const PHPoint&, const PHPoint&);
  PHPlane(const PHLine &);
  PHPlane& operator=(const PHLine &);
  virtual ~PHPlane() {}

  void print() const;

  friend std::ostream& operator<<(std::ostream&, const PHPlane&);

  void     setOrigin(const PHPoint &p)  {origin = p;}
  void     setNormal(const PHVector &v);
  const PHPoint&  getOrigin()  const {return origin;}
  const PHVector& getNormal()  const {return normal;}

  double getD() const;

protected:
  PHPoint origin;
  PHVector normal;
};

#endif /*PHPLANE_H*/
