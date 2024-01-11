/*
 * TFvtxMPNode.cxx
 *
 *  Created on: Nov 6, 2012
 *      Author: jinhuang
 */
// $$Id: TFvtxMPNode.cxx,v 1.7 2015/09/09 01:50:17 jinhuang Exp $$
/*!
 * \file TFvtxMPNode.cxx
 * \brief
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $$Revision: 1.7 $$
 * \date $$Date: 2015/09/09 01:50:17 $$
 */

#include "TFvtxMPNode.h"

#include <MUTOO.h>
#include <FVTXOO.h>
#include <FvtxGeom.h>

#include <TClonesArray.h>
#include <TMath.h>
#include <TVectorD.h>
#include <TMatrixD.h>
#include <TRandom3.h>

#include <fstream>
#include <iostream>
#include <cassert>
#include <cmath>

using namespace std;

////////////////////////////////////////////////////////////////////
// TFvtxMPNode
////////////////////////////////////////////////////////////////////

ClassImp(TFvtxMPNode);

TFvtxMPNode::TFvtxMPNode()
{
  Clear();
}

void
TFvtxMPNode::Clear(Option_t * opt)
{

  TObject::Clear(opt);

  //! w_det
  meas = INVALID;

  //! sigma
  sigma = INVALID;

  //! dwdx
  dwdx = INVALID;

  //! dwdtx
  dwdtx = INVALID;

  //! dwdy
  dwdy = INVALID;

  //! dwdty
  dwdty = INVALID;

  p_det.SetXYZ(0, 0, INVALID);

  //! Kalman fit
  residu_kalman = INVALID;
  fit_kalman = INVALID;
  p_kalman.SetXYZ(0, 0, INVALID);
  momentum_kalman.SetXYZ(0, 0, INVALID);

  //! Internal fit
  residu = INVALID;
  fit = INVALID;
  p_fit.SetXYZ(0, 0, INVALID);
  v_fit.SetXYZ(0, 0, INVALID);

}

TFvtxMPNode::~TFvtxMPNode()
{
  // TODO Auto-generated destructor stub
}

void
TFvtxMPNode::Print(Option_t *option) const
{
  TObject::Print(option);

  if (!IsValid())
    cout << "\t" << "======== INVALID DATA NODE ! ========" << endl;

  cout << "\t" << "measurement = " << "\t" << meas << " +/- " << sigma << endl;

  cout << "\t" << "detected point = " << "\t"
      << Form("[%f %f %f]", p_det.X(), p_det.Y(), p_det.Z()) << endl;
  cout << "\t" << "Kalman fit = " << "\t" << fit_kalman << " + "
      << residu_kalman << endl;
  cout << "\t" << "Kalman fit point = " << "\t"
      << Form("[%f %f %f]", p_kalman.X(), p_kalman.Y(), p_kalman.Z()) << endl;
  cout << "\t" << "Kalman fit momentum = " << "\t"
      << Form("[%f %f %f] GeV/c", momentum_kalman.X(), momentum_kalman.Y(),
          momentum_kalman.Z()) << endl;
  cout << "\t" << "internal fit = " << "\t" << fit << " + " << residu << endl;
  cout << "\t" << "internal fit point = " << "\t"
      << Form("[%f %f %f]", p_fit.X(), p_fit.Y(), p_fit.Z()) << endl;
  cout << "\t" << "internal fit vector = " << "\t"
      << Form("[%f %f %f]", v_fit.X(), v_fit.Y(), v_fit.Z()) << endl;

  cout << "\t" << "local derivatives = " << "\t"
      << Form("[%f %f %f %f]", dwdx, dwdtx, dwdy, dwdty) << endl;

  //consistency checks
  if (p_fit.z() != INVALID)
    assert(p_det.z() == p_fit.z());
  if (v_fit.z() != INVALID)
    assert(v_fit.z() == 1);
  if (p_kalman.z() != INVALID)
    assert(p_det.z() == p_kalman.z());

  if (dwdx != INVALID)
    assert(fabs(dwdx*dwdx + dwdy*dwdy - 1)<1e-6 or (dwdx == 0 and dwdy==0));

  if (residu_kalman != INVALID)
    assert(fabs(meas - (residu_kalman + fit_kalman))<1e-6);
  if (fit != INVALID)
    if (fabs(meas - (residu + fit)) > 1e-6)
      cout << "TFvtxMPNode::Print - ERROR - "
          << "large difference for (meas - (residu + fit)) = "
          << (meas - (residu + fit)) << endl;

  return;
}

bool
TFvtxMPNode::IsValid(Option_t *option) const
{

  //consistency checks
  if (residu == INVALID || isnan(residu))
    return false;
  if (fit == INVALID || isnan(fit))
    return false;
  if (p_fit.z() == INVALID || isnan(p_fit.z()))
    return false;
  if (v_fit.z() == INVALID || isnan(v_fit.z()))
    return false;
  if (p_kalman.z() == INVALID || isnan(p_kalman.z()))
    return false;
  if (dwdx == INVALID || isnan(dwdx))
    return false;

  if (residu_kalman == INVALID || isnan(residu_kalman))
    return false;

  return true;
}

double
TFvtxMPNode::get_measurement(double x, double y) const
{
  assert(dwdx != INVALID);
  assert(dwdy != INVALID);
  return dwdx * x + dwdy * y;
}

double
TFvtxMPNode::get_measurement_angle(double x, double y) const
{
  assert(dwdtx != INVALID);
  assert(dwdty != INVALID);
  return (dwdtx * x + dwdty * y);
}

void
TFvtxMPNode::fill_slope_derivatives(const double z_det, const double z0_fit)
{
  assert(dwdx != INVALID);
  assert(dwdy != INVALID);

  dwdtx = (dwdx * (z_det - z0_fit));
  dwdty = (dwdy * (z_det - z0_fit));
}

void
TFvtxMPNode::fill_kalman_fit(const double x_fit, const double y_fit)
{
  const double z_det = p_det.z();

  assert(z_det != INVALID);
  assert(meas != INVALID);

  fit_kalman = get_measurement(x_fit, y_fit);
  residu_kalman = meas - fit_kalman;
  p_kalman.SetXYZ(x_fit, y_fit, z_det);
}

void
TFvtxMPNode::fill_kalman_fit_angle(const double x_fit, const double y_fit)
{
  const double z_det = p_det.z();

  assert(z_det != INVALID);
  assert(meas != INVALID);

  fit_kalman = get_measurement_angle(x_fit, y_fit);
  residu_kalman = meas - fit_kalman;
  p_kalman.SetXYZ(x_fit, y_fit, z_det);
}

//! wrt_z - derivatives for global geometry parameters
double
TFvtxMPNode::get_wrt_z() const
{

  assert(dwdx != INVALID);
  assert(dwdy != INVALID);

  assert(v_fit.Z() != INVALID);

  return (dwdx * (v_fit.X() / v_fit.Z()) + dwdy * (v_fit.Y() / v_fit.Z()));

}

//! wrt_phi - derivatives for global geometry parameters
double
TFvtxMPNode::get_wrt_phi() const
{
  assert(dwdx != INVALID);
  assert(dwdy != INVALID);
  assert(p_fit.Z() != INVALID);

  return (-dwdy * p_fit.X() + dwdx * p_fit.Y());
}

////////////////////////////////////////////////////////////////////
// TFvtxMPNode_FVTX
////////////////////////////////////////////////////////////////////

ClassImp(TFvtxMPNode_FVTX);

TFvtxMPNode_FVTX::TFvtxMPNode_FVTX()
{
  Clear();
}

TFvtxMPNode_FVTX::~TFvtxMPNode_FVTX()
{
  // TODO Auto-generated destructor stub
}

void
TFvtxMPNode_FVTX::Clear(Option_t * opt)
{

  TFvtxMPNode::Clear(opt);

  //! arm
  arm = INVALID;

  //! cage
  cage = INVALID;

  //! station
  station = INVALID;

  //! sector
  sector = INVALID;

  //! half
  column = INVALID;

  strip = INVALID;

  p_strip_begin.SetXYZ(0, 0, INVALID);

  p_strip_end.SetXYZ(0, 0, INVALID);

  half_angle = INVALID;

  hit_size = INVALID;

  q_total = INVALID;

  w_fit_1D = INVALID;

  residu_1D = INVALID;

//  wrt_z = INVALID;
//
//  wrt_phi = INVALID;

}

void
TFvtxMPNode_FVTX::Print(Option_t *option) const
{
  TFvtxMPNode::Print(option);

  cout << "\t" << "FVTX strip IDs = " << "\t"
      << Form("[%d %d %d %d %d %d]", arm, cage, station, sector, column, strip)
      << endl;

  cout << "\t" << "hit_size = " << "\t" << hit_size << endl;
  cout << "\t" << "q_total = " << "\t" << q_total << endl;
  cout << "\t" << "half_angle = " << "\t" << half_angle << endl;
  cout << "\t" << "strip_begin = " << "\t"
      << Form("[%f %f %f]", p_strip_begin.X(), p_strip_begin.Y(),
          p_strip_begin.Z()) << endl;
  cout << "\t" << "strip_end = " << "\t"
      << Form("[%f %f %f]", p_strip_end.X(), p_strip_end.Y(), p_strip_end.Z())
      << endl;

  cout << "\t" << "W-Z fit = " << "\t" << w_fit_1D << " + " << residu_1D
      << endl;
//  cout << "\t" << "global derivatives = " << "\t"
//      << Form("[%f %f]", wrt_z, wrt_phi) << endl;

//consistency checks
  if (p_strip_begin.z() != INVALID)
    assert(p_det.z() == p_strip_begin.z());
  if (p_strip_end.z() != INVALID)
    assert(p_det.z() == p_strip_end.z());

  return;
}

int
TFvtxMPNode_FVTX::get_w_sign() const
{
  assert(meas != INVALID);

  return MUTOO::SIGN(meas);
}

////////////////////////////////////////////////////////////////////
// TFvtxMPNode_VTX
////////////////////////////////////////////////////////////////////

ClassImp(TFvtxMPNode_VTX);

TFvtxMPNode_VTX::TFvtxMPNode_VTX()
{
  Clear();
}

TFvtxMPNode_VTX::~TFvtxMPNode_VTX()
{
  // TODO Auto-generated destructor stub
}

void
TFvtxMPNode_VTX::Clear(Option_t * opt)
{

  TFvtxMPNode::Clear(opt);

  switch_r_phi = true;
  layer = INVALID;
  ladder = INVALID;
  sensor = INVALID;
  svxSection = INVALID;

}

void
TFvtxMPNode_VTX::Print(Option_t *option) const
{
  TFvtxMPNode::Print(option);

  cout << "\t" << "VTX sensor IDs = " << "\t"
      << Form("[%d %d %d %d] - ", svxSection, layer, ladder, sensor)
      << (switch_r_phi ? "R Direction" : "phi Direction") << endl;

  return;
}

////////////////////////////////////////////////////////////////////
// TFvtxMPNode_MuTr
////////////////////////////////////////////////////////////////////

ClassImp(TFvtxMPNode_MuTr);

TFvtxMPNode_MuTr::TFvtxMPNode_MuTr()
{
  Clear();
}

TFvtxMPNode_MuTr::~TFvtxMPNode_MuTr()
{
  // TODO Auto-generated destructor stub
}

void
TFvtxMPNode_MuTr::Clear(Option_t * opt)
{

  TFvtxMPNode::Clear(opt);

  arm = INVALID;
  station = INVALID;
  octant = INVALID;
  half_octant = INVALID;
  gap = INVALID;
  cathode = INVALID;
  strip = INVALID;

  dwdtx_multi_scat = INVALID;
  dwdty_multi_scat = INVALID;

  p_strip_begin.SetXYZ(0, 0, INVALID);

  p_strip_end.SetXYZ(0, 0, INVALID);

//  wrt_z = INVALID;
//
//  wrt_phi = INVALID;
}

void
TFvtxMPNode_MuTr::Print(Option_t *option) const
{
  TFvtxMPNode::Print(option);

  cout << "\t" << "MuTr IDs = " << "\t"
      << Form("[%d %d %d %d %d %d %d] - ", arm, station, octant, half_octant, gap,
          cathode, strip) << endl;

  cout << "\t" << "dwdtx_multi_scat = " << "\t"<<dwdtx_multi_scat << endl;
  cout << "\t" << "dwdty_multi_scat = " << "\t"<<dwdty_multi_scat << endl;

  cout << "\t" << "strip_begin = " << "\t"
      << Form("[%f %f %f]", p_strip_begin.X(), p_strip_begin.Y(),
          p_strip_begin.Z()) << endl;
  cout << "\t" << "strip_end = " << "\t"
      << Form("[%f %f %f]", p_strip_end.X(), p_strip_end.Y(), p_strip_end.Z())
      << endl;
//  cout << "\t" << "global derivatives = " << "\t"
//      << Form("[%f %f]", wrt_z, wrt_phi) << endl;

//consistency checks
  if (p_strip_begin.z() != INVALID)
    assert(p_det.z() == p_strip_begin.z());
  if (p_strip_end.z() != INVALID)
    assert(p_det.z() == p_strip_end.z());

  return;
}

////////////////////////////////////////////////////////////////////
// TFvtxMPNode_MuID
////////////////////////////////////////////////////////////////////

ClassImp(TFvtxMPNode_MuID);

TFvtxMPNode_MuID::TFvtxMPNode_MuID()
{
  Clear();
}

TFvtxMPNode_MuID::~TFvtxMPNode_MuID()
{
  // TODO Auto-generated destructor stub
}

void
TFvtxMPNode_MuID::Clear(Option_t * opt)
{

  TFvtxMPNode::Clear(opt);

  arm = INVALID;
  plane = INVALID;
  panel = INVALID;
  orientation = INVALID;

  dwdtx_multi_scat = INVALID;
  dwdty_multi_scat = INVALID;

  p_strip_begin.SetXYZ(0, 0, INVALID);

  p_strip_end.SetXYZ(0, 0, INVALID);
}

void
TFvtxMPNode_MuID::Print(Option_t *option) const
{
  TFvtxMPNode::Print(option);

  cout << "\t" << "MuID IDs = " << "\t"
      << Form("[%d %d %d %d] - ", arm, plane, panel, orientation) << endl;

  cout << "\t" << "dwdtx_multi_scat = " << "\t"<<dwdtx_multi_scat << endl;
  cout << "\t" << "dwdty_multi_scat = " << "\t"<<dwdty_multi_scat << endl;

  cout << "\t" << "strip_begin = " << "\t"
      << Form("[%f %f %f]", p_strip_begin.X(), p_strip_begin.Y(),
          p_strip_begin.Z()) << endl;
  cout << "\t" << "strip_end = " << "\t"
      << Form("[%f %f %f]", p_strip_end.X(), p_strip_end.Y(), p_strip_end.Z())
      << endl;

  return;
}

////////////////////////////////////////////////////////////////////
// TFvtxMPNode_Constraint
////////////////////////////////////////////////////////////////////

ClassImp(TFvtxMPNode_Constraint);

TFvtxMPNode_Constraint::TFvtxMPNode_Constraint()
{
  Clear();
}

TFvtxMPNode_Constraint::~TFvtxMPNode_Constraint()
{
}

void
TFvtxMPNode_Constraint::Clear(Option_t * opt)
{

  TFvtxMPNode::Clear(opt);

  constraint_type = DUMMY;
}

void
TFvtxMPNode_Constraint::Print(Option_t *option) const
{
  TFvtxMPNode::Print(option);

  cout << "\t" << "constraint_type = " << "\t" << constraint_type << " ("
      << constraint_type_string(constraint_type) << ")" << endl;

  return;
}
