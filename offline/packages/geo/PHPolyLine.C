// Created by: Jane M. Burward-Hoy and Federica Messer Purpose: The
// constructor, destructor, copy constructor, and assignment functions
// are defined here for the PHPolyLine class.

#include <iostream>
//INCLUDECHECKER: Removed this line: #include "PHPoint.h"
//INCLUDECHECKER: Removed this line: #include "PHVector.h"
//INCLUDECHECKER: Removed this line: #include "PHPolyLine.h"
#include "PHGeometry.h"

using namespace std;
using namespace PHGeometry;

PHPolyLine::~PHPolyLine()
{
  polyline.clearAndDestroy();
}

void 
PHPolyLine::clear()
{
  polyline.clear();
}

void 
PHPolyLine::clearAndDestroy()
{
  polyline.clearAndDestroy();
}

ostream& 
operator<< (ostream &os, PHPolyLine &pline)
{
  os << pline;
  return os;
}

void 
PHPolyLine::print() const
{
  cout << polyline << endl;
}

void 
PHPolyLine::print(size_t index) const
{
  if (index < polyline.length())
    {
      cout << *polyline[index] << endl;
    }
  }

PHPoint* 
PHPolyLine::getPoint(const size_t index) const
{
  return polyline[index];
}

PHPoint* 
PHPolyLine::removeLastPoint()
{
  return polyline.removeLast();
}

PHPoint* 
PHPolyLine::removePointAt(size_t index)
{
  return polyline.removeAt(index);
}

void 
PHPolyLine::appendPoint(PHPoint* ptr)
{
  polyline.append(ptr);
}

void 
PHPolyLine::insertPointAt(PHPoint* ptr, size_t index)
{
  polyline.insertAt(ptr, index);
}

double 
PHPolyLine::arcLength() const
{
  const size_t last = numberOfPoints() - 1;
  return arcLength(0, last);
}

double 
PHPolyLine::arcLength(const size_t n1, const size_t n2) const
{
  double length = 0.0;
  size_t first, last;
  
  if (n1 < n2)
    {
      first = n1;
      last = n2;
    }
  else
    {
      first = n2;
      last = n1;
    }
  
  if (last < numberOfPoints())
    {
      PHPoint* previous = polyline[first];
      PHPoint* thisOne;
      for (size_t i = first + 1; i <= last; i++)
	{
	  thisOne = polyline[i];
	  length = length + distancePointToPoint(*previous, *thisOne);
	  previous = thisOne;
	}
    }
  else
    {
      PHMessage("PHPolyLine::arcLength", PHError, "index out of range");
    }
  
  return length;
}
