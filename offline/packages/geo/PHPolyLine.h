#ifndef __PHPOLYLINE_H__
#define __PHPOLYLINE_H__

// Class: PHPolyLine header
// Created by: Jane M. Burward-Hoy and Federica Messer
// Purpose: A polyline representation ( Cartesian Coordinates)
// Description: A polyline is defined as an array of (pointers to) points (PHPoint).
// Two consecutive points in the array describe a line segment (PHLine) 

#include "phool.h"
#include "PHPoint.h"
#include "PHPointerList.h"

#include <iosfwd>

class PHPolyLine
{
public:
  PHPolyLine() {}
 virtual ~PHPolyLine();
 void clear();
 void clearAndDestroy();

 size_t numberOfPoints() const { 
   return polyline.length(); 
 }

 double arcLength() const;
 double arcLength(const size_t, const size_t) const;

 PHPoint* getPoint(const size_t) const;
 PHPoint* removeLastPoint();
 PHPoint* removePointAt(size_t);
 void appendPoint(PHPoint* ptr);
 void insertPointAt(PHPoint* ptr,size_t);

 friend std::ostream& operator<< (std::ostream &, PHPolyLine &);
 void print() const;
 void print(size_t) const;

private:
 PHPointerList<PHPoint> polyline;

};

#endif /* __PHPOLYLINE_H__ */
