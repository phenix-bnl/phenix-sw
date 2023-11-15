//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2000
//
//  Declaration of class PdbPadSimGeoParV1
//
//  Purpose: Store info of chamber parameters into simualtion database
//           previously was in padGeometry.txt file
//
//  Author: Indrani Ojha
//------------------------------------------------------------------------------
#ifndef __PDBPADSIMGEOPARV1_HH__
#define __PDBPADSIMGEOPARV1_HH__

#include "PdbCalChan.hh"

class PdbPadSimGeoParV1: public PdbCalChan {
public:
  PdbPadSimGeoParV1();
  virtual ~PdbPadSimGeoParV1();
  PdbPadSimGeoParV1(const PdbPadSimGeoParV1 &);
  
  virtual void print() const;
  void zero();
    virtual float get_z0Gap() const {return z0Gap; } 
    virtual void set_z0Gap(float f) { z0Gap = f; }   
    virtual float get_xOffset() const {return xOffset; } 
    virtual void set_xOffset(float f) { xOffset = f; }   
    virtual float get_zOffset() const {return zOffset; } 
    virtual void set_zOffset(float f) { zOffset = f; }  
    virtual float get_gasAtten() const {return gasAtten; } 
    virtual void set_gasAtten(float f) { gasAtten = f; }   
    virtual float get_anodeSpacing() const {return anodeSpacing; } 
    virtual void set_anodeSpacing(float f) { anodeSpacing = f; }   
    virtual float get_pixelLength() const {return pixelLength; } 
    virtual void set_pixelLength(float f) { pixelLength = f; }   
    virtual float get_sideWidth() const {return sideWidth; } 
    virtual void set_sideWidth(float f) { sideWidth = f; }   
    virtual float get_centerWidth() const {return centerWidth; } 
    virtual void set_centerWidth(float f) { centerWidth = f; }   
    virtual float get_pixelSpacing() const {return pixelSpacing; } 
    virtual void set_pixelSpacing(float f) { pixelSpacing = f; }   
    virtual float get_cellSpacing() const {return cellSpacing; } 
    virtual void set_cellSpacing(float f) { cellSpacing = f; }   
    virtual short get_nWiresPerSect() const { return nWiresPerSect;}
    virtual void set_nWiresPerSect(short si) { nWiresPerSect=si;}
    virtual short get_nPadsAcrossWire() const { return nPadsAcrossWire;}
    virtual void set_nPadsAcrossWire(short si) { nPadsAcrossWire=si;}
    virtual short get_nPadsAlongWire() const { return nPadsAlongWire;}
    virtual void set_nPadsAlongWire(short si) { nPadsAlongWire=si;}

private:
  float z0Gap;
  float xOffset;
  float zOffset;

  float gasAtten;
  float anodeSpacing; 
  float pixelLength;
  float sideWidth;    // side pixel width
  float centerWidth;  // center pixel width
  float pixelSpacing; // pixel space line width 
  float cellSpacing;
  short nWiresPerSect;
  short nPadsAcrossWire;
  short nPadsAlongWire;

  ClassDef(PdbPadSimGeoParV1,1);
};

#endif /* __PADSIMGEOPAR_HH__ */
