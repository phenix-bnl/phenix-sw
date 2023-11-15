//----------------------------------------------------------------------------
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Implementation of class PdbPadSimGeoParV1
//
//  Author: Indrani Ojha
//-----------------------------------------------------------------------------
#include "PdbPadSimGeoParV1.hh"

#include <iostream>

PdbPadSimGeoParV1::PdbPadSimGeoParV1()
{
  zero();
}
void PdbPadSimGeoParV1::zero()
{
  z0Gap=0.0;
  xOffset=0.0;
  zOffset=0.0;
  gasAtten=0.0;
  anodeSpacing=0.0;
  pixelLength=0.0;
  sideWidth=0.0;
  centerWidth=0.0;
  pixelSpacing=0.0;
  cellSpacing=0.0;
  nWiresPerSect=0;
  nPadsAcrossWire=0;
  nPadsAlongWire=0;
} 
PdbPadSimGeoParV1::~PdbPadSimGeoParV1()
{
}
PdbPadSimGeoParV1::PdbPadSimGeoParV1(const PdbPadSimGeoParV1 &rhs)
{
  gasAtten=rhs.gasAtten;         
  anodeSpacing=rhs.anodeSpacing;     
  pixelLength=rhs.pixelLength;      
  sideWidth=rhs.sideWidth;        
  centerWidth=rhs.centerWidth;      
  pixelSpacing=rhs.pixelSpacing;     
  cellSpacing=rhs.cellSpacing;
  nWiresPerSect=rhs.nWiresPerSect;    
  nPadsAcrossWire=rhs.nPadsAcrossWire;  
  nPadsAlongWire=rhs.nPadsAlongWire;   
  
  z0Gap=rhs.z0Gap;            
  xOffset=rhs.xOffset;
  zOffset=rhs.zOffset;
}
 
void PdbPadSimGeoParV1::print() const
{
  std::cout << "anodeSpacing: " << anodeSpacing << std::endl;
  std::cout << "pixelLength: " << pixelLength << std::endl;
  std::cout << "sidePixelWidth: " << sideWidth << std::endl;
  std::cout << "centerPixelWidth: " << centerWidth << std::endl;
  std::cout << "pixelSpacing: " << pixelSpacing << std::endl;
  std::cout << "cellSpacing: " << cellSpacing << std::endl;

  std::cout << "xOffset: " << xOffset << std::endl;
  std::cout << "zOffset: " << zOffset << std::endl;
}



