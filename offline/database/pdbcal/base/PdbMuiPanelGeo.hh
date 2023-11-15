//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbMuiPanelGeo
//
//  Purpose: MuID panel geometry object.
//
//  Description: Contains the survey data for one MuID panel, and the
//               numbers needed to calculate the panel position and
//               orientation from the survey.
//
//  Author: pope
//-----------------------------------------------------------------------------
#ifndef __PDBMUIPANELGEO_HH__
#define __PDBMUIPANELGEO_HH__

#include "PdbCalChan.hh"

class PdbMuiPanelGeo : public PdbCalChan
{
public:
  PdbMuiPanelGeo();
  PdbMuiPanelGeo(const short& arm, const short& gap,
		 const short& panel,
		 const float& xTarget1Global,
		 const float& yTarget1Global,
		 const float& zTarget1Global,
		 const float& xTarget2Global,
		 const float& yTarget2Global,
		 const float& zTarget2Global,
		 const float& dxTarget1ToFiducial,
		 const float& dyTarget1ToFiducial,
		 const float& dxFiducialToCenter,
		 const float& dyFiducialToCenter,
		 const float& dzCenterToCloseHTubes,
		 const float& dzCenterToFarHTubes,
		 const float& dzCenterToCloseVTubes,
		 const float& dzCenterToFarVTubes);
  PdbMuiPanelGeo(const PdbMuiPanelGeo& rhs);
  PdbMuiPanelGeo& operator=(const PdbMuiPanelGeo& rhs);

  virtual ~PdbMuiPanelGeo();

  virtual void print() const;

  virtual void SetChannel(const short& arm,   const short& gap,
			  const short& panel);

  virtual void SetTargetPosition(const short& target,
				 const float& x,
				 const float& y,
				 const float& z);
  virtual void SetTarget1ToFiducial(const float& dx, const float& dy);
  virtual void SetFiducialToCenter(const float& dx, const float& dy);
  virtual void SetTubeDisplacement(const short& orient,
				   const float& dz0, const float& dz1);

  virtual void Channel(short& arm, short& gap, short& panel) const;

  virtual void TargetPosition(const short& target,
			      float& x, float& y, float& z) const;
  virtual void LocalTargetPos(const short& target,
			      float& x, float& y, float& z) const;
  virtual void Target1ToFiducial(float& dx, float& dy) const;
  virtual void FiducialToCenter(float& dx, float& dy) const;
  virtual void TubeDisplacement(const short& orient,
				float& dz0, float& dz1) const;

private:
  float fTarget1Pos[3], fTarget2Pos[3];
  float fDxTarget1Fiducial, fDyTarget1Fiducial;
  float fDxFiducialCenter,  fDyFiducialCenter;
  float fDzCenterHTubesL0,  fDzCenterHTubesL1;
  float fDzCenterVTubesL0,  fDzCenterVTubesL1;
  short fArm, fGap, fPanel;

  ClassDef(PdbMuiPanelGeo,1);
};

#endif /* __PDBMUIPANELGEO_HH__ */
