#ifndef PHPANEL_H
#define PHPANEL_H

// Created by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose:  A pannel representation  (Cartesian Coordinates).
//      
// Description:
//           A panel is a limited plane with 4 edges.  It is defined
//           by 3 points (cartesian) in space.  The first point (p0)
//           is the reference point and the two other points (p1 and
//           p2) define the length of two edges. The last point (p3)
//           is calculated internally and it is the simmetric of p0 in
//           respect to the line connecting p1 and p2

#include "PHPoint.h"
#include "PHPlane.h"
#include "PHTriPanel.h"

#include <iosfwd>

class PHPanel: public PHPlane
{
public:
  PHPanel() {} 
  PHPanel(const PHPoint &, const PHPoint&, const PHPoint&);
  virtual ~PHPanel() {}

  void print() const;

  friend std::ostream& operator<<(std::ostream&, const PHPanel&);

  double surface() const;
  
  const PHPoint& getPoint(const int i) const ;
  const PHPoint& getCenter() const {return center;}

  const PHTriPanel& getPanel1() const { return panel1; }
  const PHTriPanel& getPanel2() const { return panel2; }

protected:
  PHPoint  p0;
  PHPoint  p1;
  PHPoint  p2;
  PHPoint  p3;

  PHPoint center;

  PHTriPanel panel1;
  PHTriPanel panel2;
};

#endif /*PHPANEL_H*/
