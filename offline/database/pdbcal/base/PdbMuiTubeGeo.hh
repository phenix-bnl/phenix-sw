//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbMuiTubeGeo
//
//  Purpose: MuID tube geometry object.
//
//  Description: Contains the survey data for one MuID tube, and the
//               numbers needed to calculate the tube position and
//               orientation from the survey.
//
//  Author: pope
//-----------------------------------------------------------------------------
#ifndef __PDBMUITUBEGEO_HH__
#define __PDBMUITUBEGEO_HH__

#include "PdbCalChan.hh"

class PdbMuiTubeGeo : public PdbCalChan
{
public:
  PdbMuiTubeGeo();
  PdbMuiTubeGeo(const short& arm,   const short& gap,
		const short& panel, const short& orient,
		const short& layer, const short& twopack,
		const float& xlo,   const float& xhi,
		const float& ylo,   const float& yhi);
  PdbMuiTubeGeo(const PdbMuiTubeGeo& rhs);
  PdbMuiTubeGeo& operator=(const PdbMuiTubeGeo& rhs);

  virtual ~PdbMuiTubeGeo();

  virtual void print() const;

  virtual void SetChannel(const short& arm,   const short& gap,
			  const short& panel, const short& orient,
			  const short& layer, const short& twopack);

  virtual void SetEndPositionLo(const float& x, const float& y);
  virtual void SetEndPositionHi(const float& x, const float& y);

  virtual void Channel(short& arm,    short& gap,   short& panel,
		       short& orient, short& layer, short& twopack) const;

  virtual void EndPositionLo(float& x, float& y) const;
  virtual void EndPositionHi(float& x, float& y) const;

private:
  float fXlo, fXhi, fYlo, fYhi;
  short fArm, fGap, fPanel, fOrient, fLayer, fTwoPack;

  ClassDef(PdbMuiTubeGeo,1);
};

#endif /* __PDBMUITUBEGEO_HH__ */
