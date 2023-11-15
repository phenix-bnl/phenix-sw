//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMuiTubeGeo
//
//  Author: pope
//-----------------------------------------------------------------------------
#include "PdbMuiTubeGeo.hh"

#include <iostream>

PdbMuiTubeGeo::PdbMuiTubeGeo()
  : fXlo(0.0), fXhi(0.0), fYlo(0.0), fYhi(0.0),
    fArm(-1), fGap(-1), fPanel(-1),
    fOrient(-1), fLayer(-1), fTwoPack(-1)
{
}

PdbMuiTubeGeo::PdbMuiTubeGeo(const short& arm,   const short& gap,
			     const short& panel, const short& orient,
			     const short& layer, const short& twopack,
			     const float& xlo,   const float& xhi,
			     const float& ylo,   const float& yhi)
  : fXlo(xlo), fXhi(xhi), fYlo(ylo), fYhi(yhi),
    fArm(arm), fGap(gap), fPanel(panel),
    fOrient(orient), fLayer(layer), fTwoPack(twopack)
{
}

PdbMuiTubeGeo::PdbMuiTubeGeo(const PdbMuiTubeGeo& rhs)
  : fXlo(rhs.fXlo), fXhi(rhs.fXhi), fYlo(rhs.fYlo), fYhi(rhs.fYhi),
    fArm(rhs.fArm), fGap(rhs.fGap), fPanel(rhs.fPanel),
    fOrient(rhs.fOrient), fLayer(rhs.fLayer), fTwoPack(rhs.fTwoPack)
{
}

PdbMuiTubeGeo&
PdbMuiTubeGeo::operator=(const PdbMuiTubeGeo& rhs)
{
  if (this == &rhs) return *this;

  fArm     = rhs.fArm;
  fGap     = rhs.fGap;
  fPanel   = rhs.fPanel;
  fOrient  = rhs.fOrient;
  fLayer   = rhs.fLayer;
  fTwoPack = rhs.fTwoPack;
  fXlo     = rhs.fXlo;
  fYlo     = rhs.fYlo;
  fXhi     = rhs.fXhi;
  fYhi     = rhs.fYhi;

  return *this;
}


PdbMuiTubeGeo::~PdbMuiTubeGeo()
{
}

void PdbMuiTubeGeo::print() const
{
  std::cout << " A" << fArm << " G" << fGap << " P" << fPanel
       << " O" << fOrient << " L" << fLayer << " T" << fTwoPack << "\n";
  std::cout << "  Xlo = " << fXlo << "  Xhi = " << fXhi
       << "  Ylo = " << fYlo << "  Yhi = " << fYhi << "\n";
  std::cout << std::endl;
}

void
PdbMuiTubeGeo::SetChannel(const short& arm,   const short& gap,
			  const short& panel, const short& orient,
			  const short& layer, const short& twopack)
{
  fArm     = arm;
  fGap     = gap;
  fPanel   = panel;
  fOrient  = orient;
  fLayer   = layer;
  fTwoPack = twopack;
}


void
PdbMuiTubeGeo::SetEndPositionLo(const float& x, const float& y)
{
  fXlo = x;
  fYlo = y;
}

void
PdbMuiTubeGeo::SetEndPositionHi(const float& x, const float& y)
{
  fXhi = x;
  fYhi = y;
}


void
PdbMuiTubeGeo::Channel(short& arm,    short& gap,   short& panel,
		       short& orient, short& layer, short& twopack)
    const
{
  arm     = fArm;
  gap     = fGap;
  panel   = fPanel;
  orient  = fOrient;
  layer   = fLayer;
  twopack = fTwoPack;
}

void
PdbMuiTubeGeo::EndPositionLo(float& x, float& y) const
{
  x = fXlo;
  y = fYlo;
}

void
PdbMuiTubeGeo::EndPositionHi(float& x, float& y) const
{
  x = fXhi;
  y = fYhi;
}
