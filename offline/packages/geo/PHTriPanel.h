#ifndef PHTRIPANEL_H
#define PHTRIPANEL_H

// Class:  PHTriPanel header
// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose:  A panel representation  (Cartesian Coordinates).
// Description:
//           A panel is a limited plane with 3 edges.
//           It is defined by 3 points (cartesian) in space .  

#include "PHPoint.h"
#include "PHPlane.h"

#include <iosfwd>

class PHTriPanel: public PHPlane
{

 public:

  PHTriPanel() : angle12(0), angle12bis(0) {}
  PHTriPanel(const PHPoint &, const PHPoint&, const PHPoint&);
  virtual ~PHTriPanel() {}

  void print() const;
  friend std::ostream& operator<<(std::ostream&, const PHTriPanel&);

  double surface() const;
  
  const PHPoint&  getPoint(int i) const ;

  double getAngle12() const { return angle12; }
  double getAngle12bis() const { return angle12bis; }

  const PHVector& getVectorV1() const { return vectorV1; }
  const PHVector& getVectorV2() const { return vectorV2; }

  const PHVector& getVectorV1bis() const { return vectorV1bis; }
  const PHVector& getVectorV2bis() const { return vectorV2bis; }

 protected:
  
  PHPoint  p0;
  PHPoint  p1;
  PHPoint  p2;

  /*
    These quantity definitions are based on the 
    PHGeometry::intersectionLineTriPanel routines definition of them.
    See ctor's for more details...
  */

  double angle12;
  double angle12bis;

  PHVector vectorV1;
  PHVector vectorV2;
  PHVector vectorV1bis;
  PHVector vectorV2bis;

  void calcGeomAngles();
};

#endif /*PHTRIPANEL_H*/
