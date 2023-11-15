#ifndef PHCYLPOINT_H
#define PHCYLPOINT_H

// Class:  PHCylPoint header
//
// Created by:  Jane M. Burward-Hoy and Federica Messer
//
// Purpose: A Point in Cylindrical Coordinates

#include "PHAngle.h"

#include <iosfwd>

class PHPoint;
class PHSphPoint;

class PHCylPoint 
{
public:
  PHCylPoint();
  PHCylPoint(double r0,  double  phi0, double z0);
  PHCylPoint(double r0,  const PHAngle& phi0, double z0);
  PHCylPoint(const PHPoint &);
  PHCylPoint(const PHSphPoint &);

  PHCylPoint& operator= (const PHPoint&);
  PHCylPoint& operator= (const PHSphPoint&);
  
  PHBoolean operator== (const PHCylPoint&) const ;

  double distanceToPoint(const PHCylPoint &) const;            
  virtual ~PHCylPoint() {}
  
  void  print() const;
  friend std::ostream& operator<<(std::ostream&, PHCylPoint &); 

  void  setR(double val)      { r = val;}
  void  setPhi(PHAngle val)   { phi = val;}
  void  setPhi(double val)    { phi = (PHAngle) val;}
  void  setZ(double val)      { z = val;} 

  double  getR()   const   {return r;}
  PHAngle getPhi() const   {return phi;}
  double  getZ()   const   {return z;}

protected:

  double r;
  PHAngle phi;
  double z;

};

#endif  /* PHCYLPOINT_H */













