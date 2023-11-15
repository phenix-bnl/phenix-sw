//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Implementation of class PdbPadGeoCham
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#include "PdbPadGeoCham.hh"

#include <iostream>

using namespace std;

PdbPadGeoCham::PdbPadGeoCham()
{
  zero();
}

void PdbPadGeoCham::zero()
{
  northUpPointX = 0.0;
  northUpPointY = 0.0;
  northUpPointZ = 0.0;
  southUpPointX = 0.0;
  southUpPointY = 0.0;
  southUpPointZ = 0.0;

  northDownPointX = 0.0;
  northDownPointY = 0.0;
  northDownPointZ = 0.0;
  southDownPointX = 0.0;
  southDownPointY = 0.0;
  southDownPointZ = 0.0;

  pc=0;
  arm=0;
  side=0;
  sector=0;
}

PdbPadGeoCham::~PdbPadGeoCham()
{
}


void PdbPadGeoCham::print() const
{
  cout << "NorthUpPoint: " << endl;
  cout << "(" << northUpPointX << ", " << northUpPointY << ", " << northUpPointZ << "))" << endl;
  cout << "SouthUpPoint: " << endl;
  cout << "(" << southUpPointX << ", " << southUpPointY << ", " << southUpPointZ << ")" << endl;
  cout << "NorthDownPoint: " << endl;
  cout << "(" << northDownPointX << ", " << northDownPointY << ", " << northDownPointZ << ")" << endl;
  cout << "SouthDownPoint: " << endl;
  cout << "(" << southDownPointX << ", " << southDownPointY << ", " << southDownPointZ << ")" << endl;

  cout << "PC: " << pc << endl;
  cout << "Arm: " << arm << endl;
  cout << "Side: " << side << endl;
  cout << "Sector: " << sector << endl;
}

PHPoint 
PdbPadGeoCham::getNorthUpPoint() const{
  PHPoint northUpPoint;
  northUpPoint.setX(northUpPointX);
  northUpPoint.setY(northUpPointY);
  northUpPoint.setZ(northUpPointZ);
  return northUpPoint;
}

PHPoint 
PdbPadGeoCham::getSouthUpPoint() const{
  PHPoint southUpPoint;
  southUpPoint.setX(southUpPointX);
  southUpPoint.setY(southUpPointY);
  southUpPoint.setZ(southUpPointZ);
  return southUpPoint;
}

PHPoint 
PdbPadGeoCham::getNorthDownPoint() const{
  PHPoint northDownPoint;
  northDownPoint.setX(northDownPointX);
  northDownPoint.setY(northDownPointY);
  northDownPoint.setZ(northDownPointZ);
  return northDownPoint;
}

PHPoint 
PdbPadGeoCham::getSouthDownPoint() const{
  PHPoint southDownPoint;
  southDownPoint.setX(southDownPointX);
  southDownPoint.setY(southDownPointY);
  southDownPoint.setZ(southDownPointZ);
  return southDownPoint;
}


void 
PdbPadGeoCham::setNorthUpPoint(PHPoint point){
  northUpPointX = point.getX();
  northUpPointY = point.getY();
  northUpPointZ = point.getZ();
}

void 
PdbPadGeoCham::setSouthUpPoint(PHPoint point){
  southUpPointX = point.getX();
  southUpPointY = point.getY();
  southUpPointZ = point.getZ();
}

void 
PdbPadGeoCham::setNorthDownPoint(PHPoint point){
  northDownPointX = point.getX();
  northDownPointY = point.getY();
  northDownPointZ = point.getZ();
}

void 
PdbPadGeoCham::setSouthDownPoint(PHPoint point){
  southDownPointX = point.getX();
  southDownPointY = point.getY();
  southDownPointZ = point.getZ();
}

