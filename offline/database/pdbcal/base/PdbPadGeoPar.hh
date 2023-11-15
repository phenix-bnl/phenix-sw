//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Declaration of class PdbPadGeoPar
//
//  Purpose: Store info on chamber parameters previously in dPadGeom
//
//  Description:
//
//  Author: silvermy
//-----------------------------------------------------------------------------
#ifndef __PDBPADGEOPAR_HH__
#define __PDBPADGEOPAR_HH__

#include "PdbCalChan.hh"

class PdbPadGeoPar : public PdbCalChan {
public:
  PdbPadGeoPar();
  virtual ~PdbPadGeoPar();
  PdbPadGeoPar(const PdbPadGeoPar &);

  virtual void print() const;

  virtual short get_PC() const { return pc;}
  virtual void set_PC(short si) { pc=si;}

  virtual float get_gasAtten() const {return gasAtten; } 
  virtual void set_gasAtten(float f) { gasAtten = f; }   
  virtual float get_anodeSpacing() const {return anodeSpacing; } 
  virtual void set_anodeSpacing(float f) { anodeSpacing = f; }   
  virtual float get_pixelLength() const {return pixelLength; } 
  virtual void set_pixelLength(float f) { pixelLength = f; }   
  virtual float get_sidePixelWidth() const {return sidePixelWidth; } 
  virtual void set_sidePixelWidth(float f) { sidePixelWidth = f; }   
  virtual float get_centerPixelWidth() const {return centerPixelWidth; } 
  virtual void set_centerPixelWidth(float f) { centerPixelWidth = f; }   
  virtual float get_pixelSpacing() const {return pixelSpacing; } 
  virtual void set_pixelSpacing(float f) { pixelSpacing = f; }   
  virtual float get_cellSpacing() const {return cellSpacing; } 
  virtual void set_cellSpacing(float f) { cellSpacing = f; }   

  virtual short get_nActiveSectors() const { return nActiveSectors;}
  virtual void set_nActiveSectors(short si) { nActiveSectors=si;}
  virtual short get_nSectPerArm() const { return nSectPerArm;}
  virtual void set_nSectPerArm(short si) { nSectPerArm=si;}
  virtual short get_nWiresPerSect() const { return nWiresPerSect;}
  virtual void set_nWiresPerSect(short si) { nWiresPerSect=si;}

  virtual short get_nPadsAcrossWire() const { return nPadsAcrossWire;}
  virtual void set_nPadsAcrossWire(short si) { nPadsAcrossWire=si;}
  virtual short get_nPadsAlongWire() const { return nPadsAlongWire;}
  virtual void set_nPadsAlongWire(short si) { nPadsAlongWire=si;}

  virtual float get_z0Gap() const {return z0Gap; } 
  virtual void set_z0Gap(float f) { z0Gap = f; }   
  virtual float get_xOffset() const {return xOffset; } 
  virtual void set_xOffset(float f) { xOffset = f; }   
  virtual float get_zOffset() const {return zOffset; } 
  virtual void set_zOffset(float f) { zOffset = f; }   
  virtual float get_pcRadius() const {return pcRadius; } 
  virtual void set_pcRadius(float f) { pcRadius = f; }   

  virtual float get_phiBottomEast() const {return phiBottomEast; } 
  virtual void set_phiBottomEast(float f) { phiBottomEast = f; }   
  virtual float get_phiTopEast() const {return phiTopEast; } 
  virtual void set_phiTopEast(float f) { phiTopEast = f; }   
  virtual float get_phiBottomWest() const {return phiBottomWest; } 
  virtual void set_phiBottomWest(float f) { phiBottomWest = f; }   
  virtual float get_phiTopWest() const {return phiTopWest; } 
  virtual void set_phiTopWest(float f) { phiTopWest = f; }   

private:
   void zero();  

private:

  short pc; // 0,1,2 for PC1, PC3 and PC3 respectively..

  float gasAtten;
  float anodeSpacing; 
  float pixelLength;
  float sidePixelWidth;
  float centerPixelWidth;
  float pixelSpacing; // pixel space line width 
  float cellSpacing; 

  short nActiveSectors;
  short nSectPerArm;
  short nWiresPerSect;
  short nPadsAcrossWire;
  short nPadsAlongWire;

  // quantities kept for historical reasons (from dPadGeom). Should not be necessary to use.
  // By def. only approximately correct. Calc. real values using PdbPadGeoCham instead
  float z0Gap;
  float xOffset;
  float zOffset;
  float pcRadius;

  float phiBottomEast;
  float phiTopEast;
  float phiBottomWest;
  float phiTopWest;

  ClassDef(PdbPadGeoPar,1);
};

#endif /* __PDBPADGEOPAR_HH__ */
