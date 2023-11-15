//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbDchWire
//
//  Author: federica
//-----------------------------------------------------------------------------
#include "PdbDchWire.hh"

#include <iostream>

using namespace std;

PdbDchWire::PdbDchWire()
{
}

PdbDchWire::PdbDchWire(const PHLine& nl, const PHLine& sl)
{
  northPointX = (nl.getBasepoint()).getX();
  northPointY = (nl.getBasepoint()).getY();
  northPointZ = (nl.getBasepoint()).getZ();

  northDriftX = (nl.getDirection()).getX();
  northDriftY = (nl.getDirection()).getY();
  northDriftZ = (nl.getDirection()).getZ();


  southPointX = (sl.getBasepoint()).getX();
  southPointY = (sl.getBasepoint()).getY();
  southPointZ = (sl.getBasepoint()).getZ();

  southDriftX = (sl.getDirection()).getX();
  southDriftY = (sl.getDirection()).getY();
  southDriftZ = (sl.getDirection()).getZ();

}

PdbDchWire::PdbDchWire(const PHPoint& np, const PHPoint&  sp, const PHVector& nd, const PHVector& sd)
{

  northPointX = np.getX();
  northPointY = np.getY();
  northPointZ = np.getZ();
  
  southPointX = sp.getX();
  southPointY = sp.getY();
  southPointZ = sp.getZ();
  
  northDriftX = nd.getX();
  northDriftY = nd.getY();
  northDriftZ = nd.getZ();
  
  southDriftX = sd.getX();
  southDriftY = sd.getY();
  southDriftZ = sd.getZ();
  
}

PdbDchWire& PdbDchWire::operator=(const  PdbDchWire &w)
{
  this->setSouthPoint(w.getSouthPoint());
  this->setNorthPoint(w.getNorthPoint());
  this->setSouthDrift(w.getSouthDrift());
  this->setNorthDrift(w.getNorthDrift());
  return *this;
}

PdbDchWire::~PdbDchWire()
{
}

PHPoint 
PdbDchWire::getNorthPoint() const{
  PHPoint northPoint;
  northPoint.setX(northPointX);
  northPoint.setY(northPointY);
  northPoint.setZ(northPointZ);
  return northPoint;
}

PHPoint 
PdbDchWire::getSouthPoint() const{
  PHPoint southPoint;
  southPoint.setX(southPointX);
  southPoint.setY(southPointY);
  southPoint.setZ(southPointZ);
  return southPoint;
}

PHVector 
PdbDchWire::getNorthDrift() const{
  PHVector northDrift;
  northDrift.setXYZ(northDriftX, northDriftY, northDriftZ);
  return northDrift;
}

PHVector 
PdbDchWire::getSouthDrift() const{
  PHVector southDrift;
  southDrift.setXYZ(southDriftX, southDriftY, southDriftZ);
  return southDrift;
}

void 
PdbDchWire::setNorthPoint(PHPoint point){
  northPointX = point.getX();
  northPointY = point.getY();
  northPointZ = point.getZ();
}

void 
PdbDchWire::setSouthPoint(PHPoint point){
  southPointX = point.getX();
  southPointY = point.getY();
  southPointZ = point.getZ();
}


void
PdbDchWire::setNorthDrift(PHVector vector){
  northDriftX = vector.getX();
  northDriftY = vector.getY();
  northDriftZ = vector.getZ();
}

void
PdbDchWire::setSouthDrift(PHVector vector){
  southDriftX = vector.getX();
  southDriftY = vector.getY();
  southDriftZ = vector.getZ();
}


void PdbDchWire::print() const
{
  cout << "NorthPoint: " << endl;
  cout << "(" << northPointX << ", " << northPointY << ", " << northPointZ << "))" << endl;
  cout << "SouthPoint: " << endl;
  cout << "(" << southPointX << ", " << southPointY << ", " << southPointZ << "))" << endl;
  cout << "NorthDrift: " << endl;
  cout << "(" << northDriftX << ", " << northDriftY << ", " << northDriftZ << "))" << endl;
  cout << "SouthDrift: " << endl;
  cout << "(" << southDriftX << ", " << southDriftY << ", " << southDriftZ << "))" << endl;
}


PHLine PdbDchWire::getNorthLine() const
{
  PHLine northLine;
  PHPoint northPoint;
  PHVector northDrift;
  northPoint.setX(northPointX);
  northPoint.setY(northPointY);
  northPoint.setZ(northPointZ);
  northDrift.setXYZ(northDriftX, northDriftY, northDriftZ);
  northLine.setBasepoint(northPoint);
  northLine.setDirection(northDrift);
  return northLine;
}
PHLine PdbDchWire::getSouthLine() const
{
  PHLine southLine;
  PHPoint southPoint;
  PHVector southDrift;
  southPoint.setX(southPointX);
  southPoint.setY(southPointY);
  southPoint.setZ(southPointZ);
  southDrift.setXYZ(southDriftX, southDriftY, southDriftZ);
  southLine.setBasepoint(southPoint);
  southLine.setDirection(southDrift);
  return southLine;
 
}
PHLine PdbDchWire::getWireLine() const
{
  PHLine wireLine;
  PHPoint northPoint;
  PHPoint southPoint;
  northPoint.setX(northPointX);
  northPoint.setY(northPointY);
  northPoint.setZ(northPointZ);
  southPoint.setX(southPointX);
  southPoint.setY(southPointY);
  southPoint.setZ(southPointZ);
  wireLine.setBasepoint(northPoint);
  wireLine.setDirection((PHVector)(southPoint - northPoint));
  return wireLine;
}
