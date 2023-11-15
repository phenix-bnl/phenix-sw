// Includes: PHLine.h
// Created by: Jane M. Burward-Hoy and Federica Messer
// Purpose: The constructor, destructor, copy constructor, and
// assignment functions are defined here for the PHLine class.

#include <iostream>
#include "PHLine.h"

using namespace std;

PHLine::PHLine(const PHPoint &p, const PHVector &v)
{
  // Basepoint is the line reference point
  basepoint = p;  
  // Direction is the vector describing the direction of the line
  // itself.
  direction = v;  
}

PHLine::PHLine(const PHPoint &p1, const PHPoint &p2)
{
  // Basepoint is the line reference point
  basepoint = p1;        
  // Direction is the vector connecting the two points 
  direction = (PHVector)(p2 - p1); 
}

ostream&  operator<< (ostream &os, PHLine &line)
{
  os << line.getBasepoint() << " " << line.getDirection() << endl;
  return os;
}

void PHLine::print() const
{
  cout << basepoint << " " << direction << endl;
}
