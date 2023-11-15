//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbDchWire
//
//  Purpose: User defined storage class
//
//  Description:
//
//  Author: federica
//-----------------------------------------------------------------------------
#ifndef __PDBDCHWIRE_HH__
#define __PDBDCHWIRE_HH__

#include "PHLine.h"
#include "PdbCalChan.hh"

class PdbDchWire : public PdbCalChan {
public:
  PdbDchWire();
  PdbDchWire(const PHLine& nl, const PHLine& sl);
  PdbDchWire(const PHPoint& np, const PHPoint&  sp,const PHVector& nd, const PHVector& sd);
  //PdbDchWire(const PHPoint& np, const PHVector& nd,const PHPoint&  sp, const PHVector& sd);
  virtual ~PdbDchWire();
  
  //  PdbDchWire(const PdbDchWire &); 
  PdbDchWire & operator = (const PdbDchWire &w); 
  
  virtual void print() const;

  virtual PHPoint getNorthPoint() const;// { return northPoint;}
  virtual PHPoint getSouthPoint() const; //{ return southPoint;}
  virtual PHVector getNorthDrift() const; //{ return northDrift;}
  virtual PHVector getSouthDrift() const; //{ return southDrift;}

  virtual PHLine getNorthLine() const;
  virtual PHLine getSouthLine() const;

  virtual PHLine getWireLine() const;

  virtual void setNorthPoint(PHPoint point); //{ northPoint = point;}
  virtual void setSouthPoint(PHPoint point); //{ southPoint = point;}
  virtual void setNorthDrift(PHVector vector); //{ northDrift = vector;}
  virtual void setSouthDrift(PHVector vector); //{ southDrift = vector;}
private:
  double southPointX;
  double southPointY;
  double southPointZ;
 
  double northPointX;
  double northPointY;
  double northPointZ;

  double southDriftX;
  double southDriftY;
  double southDriftZ;

  double northDriftX;
  double northDriftY;
  double northDriftZ;
  //PHPoint southPoint;
  //PHPoint northPoint;
  //PHVector southDrift;
  //PHVector northDrift;

  ClassDef(PdbDchWire,1);

};

#endif /* __PDBDCHWIRE_HH__ */
