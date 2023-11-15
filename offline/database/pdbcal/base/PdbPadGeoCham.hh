//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Declaration of class PdbPadGeoCham
//
//  Purpose: Store info on chamber positions
//
//  Description:
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#ifndef __PDBPADGEOCHAM_HH__
#define __PDBPADGEOCHAM_HH__

#include "PHPoint.h"
#include "PdbCalChan.hh"


class PdbPadGeoCham : public PdbCalChan {
public:
  PdbPadGeoCham();
  virtual ~PdbPadGeoCham();

  virtual void print() const;

  virtual PHPoint getNorthUpPoint() const;
  virtual PHPoint getSouthUpPoint() const;
  virtual PHPoint getNorthDownPoint() const;
  virtual PHPoint getSouthDownPoint() const;

  virtual void setNorthUpPoint(PHPoint point);
  virtual void setSouthUpPoint(PHPoint point);
  virtual void setNorthDownPoint(PHPoint point);
  virtual void setSouthDownPoint(PHPoint point);

  virtual short getPC() const { return pc;}
  virtual short getArm() const { return arm;}
  virtual short getSector() const { return sector;}
  virtual short getSide() const { return side;} // only needed for PC3

  virtual void setPC(short si) { pc=si;}
  virtual void setArm(short si)  { arm=si;}
  virtual void setSector(short si)  { sector=si;}
  virtual void setSide(short si)  { side=si;} // only needed for PC3

private:
   void zero();
   
private:

  double northUpPointX;
  double northUpPointY;
  double northUpPointZ;

  double southUpPointX;
  double southUpPointY;
  double southUpPointZ;

  double northDownPointX;
  double northDownPointY;
  double northDownPointZ;

  double southDownPointX;
  double southDownPointY;
  double southDownPointZ;

  short pc;
  short arm;
  short side;
  short sector;

  ClassDef(PdbPadGeoCham,1);

};

#endif /* __PDBPADGEOCHAM_HH__ */
