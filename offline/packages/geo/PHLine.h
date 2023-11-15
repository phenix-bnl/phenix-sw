#ifndef __PHLINE_H__
#define __PHLINE_H__

// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose:  A line representation (Cartesian Coordinates).
// Description : A line is defined by a basepoint (PHPoint) and a
// direction (PHVector).  If the direction is not normalized, the
// length of the vector constitutes the length of the line segment.

#include <iosfwd>

#include "PHPoint.h"
#include "PHVector.h"

class PHLine 
{
public:
  PHLine() {}
  PHLine(const PHPoint &, const PHVector &); 
  PHLine(const PHPoint &, const PHPoint  &); 
  virtual ~PHLine() {}

  double length() const {return direction.length();} 
  void normalize() {direction.normalize();}     

  void print() const;     

  friend std::ostream& operator<<(std::ostream&, PHLine &); 

  void setBasepoint(const PHPoint &p)  {basepoint = p;}     
  void setDirection(const PHVector &v) {direction = v;}

  const PHPoint&  getBasepoint() const { return basepoint; }
  const PHVector& getDirection() const { return direction; }

protected:
  PHPoint basepoint;
  PHVector direction;
};

#endif /* __PHLINE_H__ */
