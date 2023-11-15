#include "PHAngle.h"
#include "gsl/gsl_machine.h"
#include <cmath>

// This returns an angle in the PHENIX central arm primary domain of
// [-Pi/2, 3Pi/2].  This version works around a bug in glibc in which
// fmod returns -0 for negative arguments.
static 
inline double
angleMod(double x)
{
  double y = x + M_PI_2;

  return (y >= 0.0) ?
    fmod(y, 2.0 * M_PI) - M_PI_2 : 
    2.0 * M_PI - fmod(-y, 2.0 * M_PI) - M_PI_2;
}

//  Return angle in degrees [0, 360]
double 
PHAngle::degree() const
{
  double val = phi;
  if (val < 0)
    {
      val = 2.0 * M_PI + val;
    }

   return val * 180.0 / M_PI;
}

// Here follow the member functions for the operators ==, !=, =, +=, -=, *=, /=
PHAngle 
PHAngle::operator= (double val) 
{
  phi = RangeIt(val);
  return *this;
}

PHAngle 
PHAngle::operator+=(const PHAngle& val) 
{
  double addit= phi  + (val.getPhi());
  phi = RangeIt(addit);
  return *this;
}

PHAngle 
PHAngle::operator-=(const PHAngle& val) 
{
  double minusit= phi  - (val.getPhi());
  phi = RangeIt(minusit);
  return *this;
}

PHAngle 
PHAngle::operator*= (double val) 
{
  double multiplyit= phi  * (val);
  phi = RangeIt(multiplyit);
  return *this;
}

PHAngle 
PHAngle::operator/= (double val) 
{
  double divideit= phi  / (val);
  phi = RangeIt(divideit);
  return *this;
}

// The proper way to check for the approximate equality of two
// floating point numbers is to use Knuth's method - insert a call to
// gsl_fcmp in both of these routines ASAP.  Our current version of
// GSL doesn't have that call.

int
PHAngle::operator== (const PHAngle& a) const
{
   return (fabs(PHAngle(phi-a.getPhi())) < GSL_FLT_EPSILON);
}

int
PHAngle::operator== (const double a) const
{
   return (fabs(PHAngle(phi-a)) < GSL_FLT_EPSILON);
}

// The friends for operator +, -, *, /

PHAngle
operator+ (PHAngle angle, PHAngle val) 
{
   double res = angle.getPhi() + val.getPhi();
   return angleMod(res);
}

PHAngle
operator- (PHAngle angle, PHAngle val) 
{
  double res = angle.getPhi() - val.getPhi();
  return angleMod(res);
}

PHAngle
operator* (PHAngle angle, double val) 
{
  double res = angle.getPhi() * val;
  return angleMod(res);
}

PHAngle
operator/ (PHAngle angle, double val) 
{
  double res = angle.getPhi() / val;
  return angleMod(res);
}

PHAngle
average(PHAngle a, PHAngle b)
{
  return a + (b - a) / 2.0;
}

 PHAngleRange::PHAngleRange():
   phimin(NAN),
   phimax(NAN)
 {}

PHAngleRange::PHAngleRange (const PHAngle &phi1, const PHAngle &phi2)
{
  phimin.setPhi(phi1);
  phimax.setPhi(phi2);
}

void
PHAngleRange::setAngleRange(const PHAngle &phi1, const PHAngle &phi2)
{
  phimin.setPhi(phi1);
  phimax.setPhi(phi2);
}

PHBoolean
PHAngleRange::isInRange(const PHAngle &phi) const
{
  // need to be finished !!!
  double tmpPhiMin = phimin;
  double tmpPhiMax = phimax;
  double tmpPhi  = phi;
  if (phimax < phimin) 
    {
      tmpPhiMax = tmpPhiMax + 2*M_PI;
      if (tmpPhi < 0)
	{
	  tmpPhi = tmpPhi +2*M_PI;
	}
    }
  if (tmpPhi > tmpPhiMin && tmpPhi < tmpPhiMax) 
    {
      return True;
    }
  else
    {
      return False;
    }
}
