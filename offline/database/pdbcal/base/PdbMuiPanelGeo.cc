//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMuiPanelGeo
//
//  Author: pope
//-----------------------------------------------------------------------------
#include "PdbMuiPanelGeo.hh"

#include <cmath>
#include <iostream>

PdbMuiPanelGeo::PdbMuiPanelGeo()
  : fDxTarget1Fiducial(0.0),    fDyTarget1Fiducial(0.0),
    fDxFiducialCenter(0.0),     fDyFiducialCenter(0.0),
    fDzCenterHTubesL0(0.0),     fDzCenterHTubesL1(0.0),
    fDzCenterVTubesL0(0.0),     fDzCenterVTubesL1(0.0),
    fArm(-1), fGap(-1), fPanel(-1)
{
  fTarget1Pos[0] = 0.0;
  fTarget1Pos[1] = 0.0;
  fTarget1Pos[2] = 0.0;
  fTarget2Pos[0] = 0.0;
  fTarget2Pos[1] = 0.0;
  fTarget2Pos[2] = 0.0;
}

PdbMuiPanelGeo::PdbMuiPanelGeo(const short& arm, const short& gap,
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
			       const float& dzCenterToFarVTubes)
  : fDxTarget1Fiducial(dxTarget1ToFiducial),
    fDyTarget1Fiducial(dyTarget1ToFiducial),
    fDxFiducialCenter(dxFiducialToCenter),
    fDyFiducialCenter(dyFiducialToCenter),
    fDzCenterHTubesL0(dzCenterToCloseHTubes),
    fDzCenterHTubesL1(dzCenterToFarHTubes),
    fDzCenterVTubesL0(dzCenterToCloseVTubes),
    fDzCenterVTubesL1(dzCenterToFarVTubes),
    fArm(arm), fGap(gap), fPanel(panel)
{
  fTarget1Pos[0] = xTarget1Global;
  fTarget1Pos[1] = yTarget1Global;
  fTarget1Pos[2] = zTarget1Global;
  fTarget2Pos[0] = xTarget2Global;
  fTarget2Pos[1] = yTarget2Global;
  fTarget2Pos[2] = zTarget2Global;
}

PdbMuiPanelGeo::PdbMuiPanelGeo(const PdbMuiPanelGeo& rhs)
  : fDxTarget1Fiducial(rhs.fDxTarget1Fiducial),
    fDyTarget1Fiducial(rhs.fDyTarget1Fiducial),
    fDxFiducialCenter(rhs.fDxFiducialCenter),
    fDyFiducialCenter(rhs.fDyFiducialCenter),
    fDzCenterHTubesL0(rhs.fDzCenterHTubesL0),
    fDzCenterHTubesL1(rhs.fDzCenterHTubesL1),
    fDzCenterVTubesL0(rhs.fDzCenterVTubesL0),
    fDzCenterVTubesL1(rhs.fDzCenterVTubesL1),
    fArm(rhs.fArm), fGap(rhs.fGap), fPanel(rhs.fPanel)
{
  fTarget1Pos[0] = rhs.fTarget1Pos[0];
  fTarget2Pos[0] = rhs.fTarget2Pos[0];
  fTarget1Pos[1] = rhs.fTarget1Pos[1];
  fTarget2Pos[1] = rhs.fTarget2Pos[1];
  fTarget1Pos[2] = rhs.fTarget1Pos[2];
  fTarget2Pos[2] = rhs.fTarget2Pos[2];
}

PdbMuiPanelGeo&
PdbMuiPanelGeo::operator=(const PdbMuiPanelGeo& rhs)
{
  if (this == &rhs) return *this;  // avoid self-assignment!

  fTarget1Pos[0] = rhs.fTarget1Pos[0];
  fTarget2Pos[0] = rhs.fTarget2Pos[0];
  fTarget1Pos[1] = rhs.fTarget1Pos[1];
  fTarget2Pos[1] = rhs.fTarget2Pos[1];
  fTarget1Pos[2] = rhs.fTarget1Pos[2];
  fTarget2Pos[2] = rhs.fTarget2Pos[2];
  fDxTarget1Fiducial = rhs.fDxTarget1Fiducial;
  fDyTarget1Fiducial = rhs.fDyTarget1Fiducial;
  fDxFiducialCenter  = rhs.fDxFiducialCenter;
  fDyFiducialCenter  = rhs.fDyFiducialCenter;
  fDzCenterHTubesL0  = rhs.fDzCenterHTubesL0;
  fDzCenterHTubesL1  = rhs.fDzCenterHTubesL1;
  fDzCenterVTubesL0  = rhs.fDzCenterVTubesL0;
  fDzCenterVTubesL1  = rhs.fDzCenterVTubesL1;
  fArm   = rhs.fArm;
  fGap   = rhs.fGap;
  fPanel = rhs.fPanel;

  return *this;
}

PdbMuiPanelGeo::~PdbMuiPanelGeo()
{
}

void
PdbMuiPanelGeo::print() const
{
  std::cout << "A" << fArm << " G" << fGap << " P" << fPanel << "\n";
  std::cout << "T1:"
       << "  " << fTarget1Pos[0]
       << "  " << fTarget1Pos[1]
       << "  " << fTarget1Pos[2] << "  "
       << "T2:"
       << "  " << fTarget2Pos[0]
       << "  " << fTarget2Pos[1]
       << "  " << fTarget2Pos[2] << "\n";
  std::cout << "T1 to fiducial:"
       << "  dx = " << fDxTarget1Fiducial
       << "  dy = " << fDyTarget1Fiducial << "  "
       << "fiducial to center:"
       << "  dx = " << fDxFiducialCenter
       << "  dy = " << fDyFiducialCenter << "\n";
  std::cout << "dz center to H layer 0 = " << fDzCenterHTubesL0 << "  "
       << "dz center to H layer 1 = " << fDzCenterHTubesL1 << "\n";
  std::cout << "dz center to V layer 0 = " << fDzCenterVTubesL0 << "  "
       << "dz center to V layer 1 = " << fDzCenterVTubesL1 << "\n";
  std::cout << std::endl;
}

void
PdbMuiPanelGeo::SetChannel(const short& arm, const short& gap,
			   const short& panel)
{
  // TODO:  would like to validate arguments, but would need
  //        TMuiChannelId header, which will not have been installed
  //        until mui package is built!
  fArm   = arm;
  fGap   = gap;
  fPanel = panel;
}

void
PdbMuiPanelGeo::SetTargetPosition(const short& target,
				  const float& x,
				  const float& y,
				  const float& z)
{
  // expects target == 1 or target == 2 !
  if (target == 1) {
    fTarget1Pos[0] = x;
    fTarget1Pos[1] = y;
    fTarget1Pos[2] = z;
  } else {
    fTarget2Pos[0] = x;
    fTarget2Pos[1] = y;
    fTarget2Pos[2] = z;
  }
}


void
PdbMuiPanelGeo::SetTarget1ToFiducial(const float& dx, const float& dy)
{
  fDxTarget1Fiducial = dx;
  fDyTarget1Fiducial = dy;
}

void
PdbMuiPanelGeo::SetFiducialToCenter(const float& dx, const float& dy)
{
  fDxFiducialCenter = dx;
  fDyFiducialCenter = dy;
}

void
PdbMuiPanelGeo::SetTubeDisplacement(const short& orient,
				    const float& dz0,
				    const float& dz1)
{
  // TODO:  would like to use kHORIZ/kVERT constants, but would need
  //        MuiCommon header, which will not have been installed until
  //        mui package is built!
  if (orient == 0) {  // horizontal
    fDzCenterHTubesL0 = dz0;
    fDzCenterHTubesL1 = dz1;
  } else {            // vertical
    fDzCenterVTubesL0 = dz0;
    fDzCenterVTubesL1 = dz1;
  }
}

void
PdbMuiPanelGeo::Channel(short& arm, short& gap, short& panel) const
{
  arm   = fArm;
  gap   = fGap;
  panel = fPanel;
}

void
PdbMuiPanelGeo::TargetPosition(const short& target,
			       float& x, float& y, float& z) const
{
  // expects target == 1 or target == 2 !
  if (target == 1) {
    x = fTarget1Pos[0];
    y = fTarget1Pos[1];
    z = fTarget1Pos[2];
  } else {
    x = fTarget2Pos[0];
    y = fTarget2Pos[1];
    z = fTarget2Pos[2];
  }
}

void
PdbMuiPanelGeo::LocalTargetPos(const short& target,
			       float& x, float& y, float& z) const
{
  // expects target == 1 or target == 2 !
  x = - fDxFiducialCenter - fDxTarget1Fiducial;
  y = - fDyFiducialCenter - fDyTarget1Fiducial;
  z = 0.0;

  if (target == 2) {
    // Assume that only the X positions of the two targets differ.

    float dx = fTarget2Pos[0] - fTarget1Pos[0];
    float dy = fTarget2Pos[1] - fTarget1Pos[1];
    float dz = fTarget2Pos[2] - fTarget1Pos[2];
    float dr = sqrt(dx*dx + dy*dy + dz*dz);

    if (dx < 0.0) {
      x -= dr;
    } else {
      x += dr;
    }

  }

}

void
PdbMuiPanelGeo::Target1ToFiducial(float& dx, float& dy) const
{
  dx = fDxTarget1Fiducial;
  dy = fDyTarget1Fiducial;
}

void
PdbMuiPanelGeo::FiducialToCenter(float& dx, float& dy) const
{
  dx = fDxFiducialCenter;
  dy = fDyFiducialCenter;
}

void
PdbMuiPanelGeo::TubeDisplacement(const short& orient,
				 float& dz0, float& dz1) const
{
  // TODO:  would like to use kHORIZ/kVERT constants, but would need
  //        MuiCommon header, which will not have been installed until
  //        mui package is built!
  if (orient == 0) {  // horizontal
    dz0 = fDzCenterHTubesL0;
    dz1 = fDzCenterHTubesL1;
  } else {            // vertical
    dz0 = fDzCenterVTubesL0;
    dz1 = fDzCenterVTubesL1;
  }
}
