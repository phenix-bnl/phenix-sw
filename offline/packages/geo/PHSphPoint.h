#ifndef PHSPHPOINT_H
#define PHSPHPOINT_H

// Class:  PHSphPoint header
//
// Created by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose: A point in Spherical Coordinates

#include "PHAngle.h"

#include <iosfwd>

class PHPoint;
class PHCylPoint;

class PHSphPoint 
{
public:
  
  PHSphPoint();
  PHSphPoint(const double r0, const double  phi0, const double theta0 );
  PHSphPoint(const double r0, const PHAngle& phi0, const PHAngle& theta0);
  PHSphPoint(const PHPoint &); 
  PHSphPoint(const PHCylPoint &); 
 
  PHSphPoint& operator= (const PHPoint&);
  PHSphPoint& operator= (const PHCylPoint&);
  
  PHBoolean operator== (const PHSphPoint&) const ;

  double distanceToPoint(const PHSphPoint &) const;           //Distance between two points
  virtual ~PHSphPoint() {}

  void  print() const ;
  friend std::ostream& operator<<(std::ostream&, PHSphPoint &); 

  void  setR(double val)      { r = val;}
  void  setPhi(PHAngle val)   { phi = val;}
  void  setTheta(PHAngle val) { theta = val;}
  
  void  setPhi(double val)    { phi = (PHAngle) val;}
  void  setTheta(double val)  { theta = (PHAngle) val;} 

  double  getR()     const {return r;}
  PHAngle getPhi()   const {return phi;}
  PHAngle getTheta() const {return theta;}

protected:

  double r;
  PHAngle phi;
  PHAngle theta;

};

#endif  /* PHSPHPOINT_H */


