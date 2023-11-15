// Class:  includes PHTriPanel header
// Created by:  Jane M. Burward-Hoy and Federica Messer

#include <iostream>
//INCLUDECHECKER: Removed this line: #include <cmath>
//INCLUDECHECKER: Removed this line: #include "PHTriPanel.h"
#include "PHGeometry.h"

using namespace std;

PHTriPanel::PHTriPanel(const PHPoint &pp0, 
		       const PHPoint &pp1, 
		       const PHPoint &pp2)
  :PHPlane(pp0,pp1,pp2)
{
   p0 = pp0;
   p1 = pp1;
   p2 = pp2;

   /*
     These quantity definitions are based on the 
     PHGeometry::intersectionLineTriPanel routines definition of them.
   */

   vectorV1 = p1 - p0;
   vectorV2 = p2 - p0;
   
   angle12 = vectorV2.angle(vectorV1);
   
   vectorV1bis = p2 - p1;
   vectorV2bis = p0 - p1;
   
   angle12bis = vectorV2bis.angle(vectorV1bis);
}


void 
PHTriPanel::print() const
{
  cout << "PHTriPanel: P0 = " << p0 
       << "  P1 = " << p1 
       << "  P2 = " << p2 << endl;
}

ostream& 
operator<<(ostream& os, const PHTriPanel& panel)
{
  os << " PHTriPanel:  P0 = " << panel.getPoint(0) 
     << " P1 = " << panel.getPoint(1) 
     << " P2 = " << panel.getPoint(2);

  return os;
}

double 
PHTriPanel::surface() const
{
  PHLine line(p0,p1);
  double height = PHGeometry::distanceLinePoint(line,p2);
  double surface = (line.length())* height /2.;
  PHMessage("PHTriPanel::surface",PHWarning,"Need still test ");

  return surface;
}

const PHPoint& 
PHTriPanel::getPoint(int i) const 
{
  if (i == 0) 
    {
      return p0;
    } 
  else if (i == 1) 
    {
      return p1;
    }
  else if (i == 2) 
    {
      return p2;
    }

  // If these tests have failed, we'd really like to return a null
  // reference, but there is no such thing in C++.  So, print an error
  // message and return a valid reference to something.
  PHMessage("PHTriPanel::getPoint",PHError,"Index too big !! ");

  return p0;
}


  


