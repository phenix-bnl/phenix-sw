// Created by:  Jane M. Burward-Hoy and Federica Messer
// Purpose: a Reference frame class.
// Adapted from Jeff Mitchell's luxFrame.hh and luxFrame.cc

#include <cmath>
#include "PHFrame.h"
#include "PHString.h"
#include <iostream>
using namespace std;

PHFrame::PHFrame()
{
  u.setX(1.0);
  v.setY(1.0);
  w.setZ(1.0);
}

PHFrame::PHFrame(const PHPoint& p, const PHVector& v1, const PHVector& v2)
{
  // Create a Frame given an origin (PHPoint) and 2 axes (PHVector)

  PHVector u1, u2;
  u1 = v1;
  u2 = v2;
  u1.normalize();
  u2.normalize();

  origin = p;
  u = u1;
  v = u2;
  w = u.cross(v);

  PHVector null;
  if (!checkOrthogonal())
    {
      PHMessage("PHFrame::constructor", PHError,
                "Not orthogonal reference system");
      u = null;
      v = null;
      w = null;
    }
}

PHFrame::PHFrame(const PHPoint& p, const PHVector& v1,
                 const PHVector& v2, const PHVector& v3)
{
  // Create a Frame given an origin (PHPoint) and 3 axes (PHVector)

  PHVector u1, u2, u3;
  u1 = v1;
  u2 = v2;
  u3 = v3;
  u1.normalize();
  u2.normalize();
  u3.normalize();

  origin = p;
  u = u1;
  v = u2;
  w = u3;

  PHVector null;
  if (!checkOrthogonal())
    {
      PHMessage("PHFrame::constructor", PHError,
                "Not orthogonal reference system");
      u = null;
      v = null;
      w = null;
    }
}

void
PHFrame::print() const
  {
    cout << "Origin: " << origin << endl;
    cout << "Axes:  u =  " << u << ", v =  " << v << ", w =  " << w << endl;
  }

PHBoolean
PHFrame::checkOrthogonal() const
  {
    PHBoolean orthogonal = True;

    if (fabs(u.dot(v)) > 2.0e-9)
      {
        orthogonal = False;
      }
    if (fabs(u.dot(w)) > 2.0e-9)
      {
        orthogonal = False;
      }
    if (fabs(v.dot(w)) > 2.0e-9)
      {
        orthogonal = False;
      }

    return orthogonal;
  }


