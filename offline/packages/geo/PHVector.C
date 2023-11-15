// Created by:  Jane M. Burward-Hoy and Federica Messer

//  Note:  whenever setting member variables, use the appropriate set() fxn
//         This is critical for the successful use of the wrapper for this
//         class

//INCLUDECHECKER: Removed this line: #include "PHVector.h"
//INCLUDECHECKER: Removed this line: #include "PHPoint.h"
#include <iostream>
#include "PHVector.h"
#include "PHGeometry.h"
using namespace std;

PHVector
PHVector::cross(const PHVector &v) const
{
  return PHGeometry::cross(*this,v);
}

double
PHVector::angle(const PHVector& v) const
{
  return PHGeometry::angle(*this,v);
}

PHVector 
PHVector::orthogonal() const
{
 PHVector newVector;

 double x = dx < 0.0 ? -dx : dx;
 double y = dy < 0.0 ? -dy : dy;
 double z = dz < 0.0 ? -dz : dz;

 if (x < y) 
  {
    newVector =  x < z ? PHVector(0,dz,-dy) : PHVector(dy,-dx,0);
  }
 else
   {
     newVector =  y < z ? PHVector(-dz,0,dx) : PHVector(dy,-dx,0);
   }

 return newVector;        	 
}

ostream& 
operator<<(ostream& os, const PHVector &p)
{
  return os << "( " << p.getX() << ", " << p.getY() << ", " << p.getZ() << " ) ";
}

void 
PHVector::print() const
{
  cout << "( " << dx << ",  " << dy << ",  " << dz << "  ) " << endl;
}
