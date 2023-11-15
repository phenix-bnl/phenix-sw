//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Implementation of class PdbPadGeoPar
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#include "PdbPadGeoPar.hh"

#include <iostream>

PdbPadGeoPar::PdbPadGeoPar()
{
  zero();
}
void PdbPadGeoPar::zero()
{
  pc=0;

  gasAtten=0.0;
  anodeSpacing=0.0;
  pixelLength=0.0;
  sidePixelWidth=0.0;
  centerPixelWidth=0.0;
  pixelSpacing=0.0;
  cellSpacing=0.0;
  
  nActiveSectors=0;
  nSectPerArm=0;
  nWiresPerSect=0;
  nPadsAcrossWire=0;
  nPadsAlongWire=0;
  
  z0Gap=0.0;
  xOffset=0.0;
  zOffset=0.0;
  pcRadius=0.0;

  phiBottomEast=0.0;
  phiTopEast=0.0;
  phiBottomWest=0.0;
  phiTopWest=0.0;
}
PdbPadGeoPar::PdbPadGeoPar(const PdbPadGeoPar &rhs)
{
  pc=rhs.pc;
  
  gasAtten=rhs.gasAtten;         
  anodeSpacing=rhs.anodeSpacing;     
  pixelLength=rhs.pixelLength;      
  sidePixelWidth=rhs.sidePixelWidth;        
  centerPixelWidth=rhs.centerPixelWidth;      
  pixelSpacing=rhs.pixelSpacing;     
  cellSpacing=rhs.cellSpacing;      
  
  nActiveSectors=rhs.nActiveSectors;
  nSectPerArm=rhs.nSectPerArm;
  nWiresPerSect=rhs.nWiresPerSect;    
  nPadsAcrossWire=rhs.nPadsAcrossWire;  
  nPadsAlongWire=rhs.nPadsAlongWire;   
  
  z0Gap=rhs.z0Gap;            
  xOffset=rhs.xOffset;
  zOffset=rhs.zOffset;          
  pcRadius=rhs.pcRadius;      

  phiBottomEast=rhs.phiBottomEast;
  phiTopEast=rhs.phiTopEast;
  phiBottomWest=rhs.phiBottomWest;
  phiTopWest=rhs.phiTopWest;
}

PdbPadGeoPar::~PdbPadGeoPar()
{
}

void PdbPadGeoPar::print() const
{
  std::cout << "\nPC: " << pc << std::endl;

  std::cout << "\ngasAtten: " << gasAtten << std::endl;
  std::cout << "anodeSpacing: " << anodeSpacing << std::endl;
  std::cout << "pixelLength: " << pixelLength << std::endl;
  std::cout << "sidePixelWidth: " << sidePixelWidth << std::endl;
  std::cout << "centerPixelWidth: " << centerPixelWidth << std::endl;
  std::cout << "pixelSpacing: " << pixelSpacing << std::endl;
  std::cout << "cellSpacing: " << cellSpacing << std::endl;

  std::cout << "\nnActiveSectors: " << nActiveSectors << std::endl;
  std::cout << "nSectPerArm: " << nSectPerArm << std::endl;
  std::cout << "nWiresPerSect: " << nWiresPerSect << std::endl;
  std::cout << "nPadsAcrossWire: " << nPadsAcrossWire << std::endl;
  std::cout << "nPadsAlongWire: " << nPadsAlongWire << std::endl;

  std::cout << "\nz0Gap: " << z0Gap << std::endl;
  std::cout << "xOffset: " << xOffset << std::endl;
  std::cout << "zOffset: " << zOffset << std::endl;
  std::cout << "pcRadius: " << pcRadius << std::endl;

  std::cout << "\nphiBottomEast: " << phiBottomEast << std::endl;
  std::cout << "phiTopEast: " << phiTopEast << std::endl;
  std::cout << "phiBottomWest: " << phiBottomWest << std::endl;
  std::cout << "phiTopWest: " << phiTopWest << std::endl;
}
