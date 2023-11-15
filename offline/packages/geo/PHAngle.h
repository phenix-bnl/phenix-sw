#ifndef __PHANGLE_H__
#define __PHANGLE_H__

//  Purpose: An angle class of period [-pi/2,3pi/2]
//
//  PHAngle keeps an angle within the range of [-pi/2, 3pi/2], units
//  are radian, hence operators ++ and -- do not make a lot of sense.
//  The reason for the unusual range is to make it easier to do angle
//  arithmetic within the acceptance of the central arms.
//
//  PHAngleRange is an 'anti-clockwise' angle range !!! 
//
//  Adapted by: Federica Messer 

#include "phool.h"
#include <cmath>

class PHAngle {
 public:
  PHAngle() : phi(0) { }
  PHAngle(double val) : phi(RangeIt(val)) { }

  double degree() const; // return angle in degrees [-90,270]
  operator double() const { return phi; }
  void setPhi(double val) { phi = val;}
  double getPhi() const { return phi; }
  
  // member functions for operators
  PHAngle operator= (double);           
  PHAngle operator+= (const PHAngle&);
  PHAngle operator-= (const PHAngle&);
  PHAngle operator*= (double);
  PHAngle operator/= (double);
  int    operator== (const PHAngle&) const;
  int    operator== (const double) const;
 
 protected: 
  // canonicalize the angle to [-M_PI_2, 3 M_PI_2].  This version
  // works around a bug in GNU glibc which return -0 for negative
  // arguments to fmod.
  double RangeIt(double x) {  
    if (x > 3 * M_PI_2 || x < -M_PI_2) 
      {
	double y = x + M_PI_2;
	return (y >= 0.0) ?
	  fmod(y, 2.0 * M_PI) - M_PI_2 : 
	  2.0 * M_PI - fmod(-y, 2.0 * M_PI) - M_PI_2;
      }
    else
      {
	return x;
      }
  } 

  double phi;
};

class PHAngleRange {
 public:
  PHAngleRange();
  PHAngleRange(const PHAngle &, const PHAngle &);
  virtual ~PHAngleRange() {}

  const PHAngle& getPhiMin() const { return phimin;}
  const PHAngle& getPhiMax() const { return phimax;}
  
  void setAngleRange(const PHAngle &, const PHAngle &);
  PHBoolean isInRange(const PHAngle &phi) const;

private:
  PHAngle phimin;
  PHAngle phimax;
};

#endif /* __PHANGLE_H__ */


