// Class:  includes PHPanel header
//
// Created by:  Jane M. Burward-Hoy and Federica Messer

#include <PHPanel.h>
#include <PHString.h> // for PHMessage

#include <cstdlib>
#include <iostream>

using namespace std;

PHPanel::PHPanel(const PHPoint &pp0, const PHPoint &pp1, const PHPoint &pp2)
            :PHPlane(pp0,pp1,pp2)
{
   p0 = pp0;
   p1 = pp1;
   p2 = pp2;

   p3 = p2 + p1 - p0;

   center.setX((double)(p0.getX() + p3.getX())/2.0);
   center.setY((double)(p0.getY() + p3.getY())/2.0);
   center.setZ((double)(p0.getZ() + p3.getZ())/2.0);

   PHTriPanel temp(p0,p1,p2);
   PHTriPanel temp2(p3,p1,p2);
   panel1 = temp;
   panel2 = temp2;
}

void 
PHPanel::print() const
{
  cout << "PHPanel: P0 = " << p0 
       << "  P1 = " << p1 
       << "  P2 = " << p2 
       <<  "  P3 = " << p3 
       << endl;
}

ostream& 
operator<<(ostream& os, const PHPanel& panel)
{
  os << " PHPanel:  P0 = " << panel.getPoint(0) 
     << " P1 = " << panel.getPoint(1) 
     << " P2 = " << panel.getPoint(2) 
     << " P3 = " << panel.getPoint(3);

  return os;
}

double 
PHPanel::surface() const
{
  PHVector v1=PHVector(p1)-PHVector(p0);
  PHVector v2=PHVector(p2)-PHVector(p0);
  PHVector a=v1.cross(v2);
  double surface=a.length();

  return surface;
}

const PHPoint& 
PHPanel::getPoint(const int i) const 
{
  if (i == 0) 
    {
      return p0;
    }
  else if (i ==1) 
    {
      return p1;
    }
  else if (i ==2) 
    {
      return p2;
    }
  else if (i ==3) 
    {
      return p3;
    }

  // If these tests have failed, we'd really like to return a null
  // reference, but there is no such thing in C++.  So, print an error
  // message and return a valid reference to something.
  PHMessage("PHPanel::getPoint",PHError,"Index too big !! ");
  exit(1);
}



