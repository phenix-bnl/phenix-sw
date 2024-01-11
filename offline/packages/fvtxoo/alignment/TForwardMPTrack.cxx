// $Id: TForwardMPTrack.cxx,v 1.2 2015/09/09 01:50:17 jinhuang Exp $                                                                                             

/*!
 * \file TForwardMPTrack.cpp
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.2 $
 * \date $Date: 2015/09/09 01:50:17 $
 */

#include "TForwardMPTrack.h"

#include <TFvtxMILLEPEDE.h>

#include <FVTXOO.h>
#include <FvtxGeom.h>

#include <phool.h>
#include <PHCylPoint.h>
#include <PHGeometry.h>

#include <TClonesArray.h>
#include <TMath.h>
#include <TVectorD.h>
#include <TMatrixD.h>
#include <TRandom3.h>

#include <fstream>
#include <iostream>
#include <cassert>
#include <cmath>
#include <algorithm>

using namespace std;

TForwardMPTrack::TForwardMPTrack()
{
  // TODO Auto-generated constructor stub

}

TForwardMPTrack::~TForwardMPTrack()
{
  // TODO Auto-generated destructor stub
}

//______________________________________________________
void
TForwardMPTrack::Reset()
{
  TFvtxMPTrack::Reset();

  trk_par_absorb_scat.set(0, 0, INVALID, INVALID, INVALID);

  mutr_disp_tangent_theta = INVALID;
  mutr_disp_tangent_phi = INVALID;
  mutr_disp_pt_z_ref = INVALID;
  mutr_disp_lateral_z_ref = INVALID;
}

//______________________________________________________
void
TForwardMPTrack::Print(Option_t *option) const
{
  TFvtxMPTrack::Print(option);

  cout << "FVTX-MuTr matching summary:" << endl;
  cout << "\t Matching plane = \t" << z_ref << " cm" << endl;
  cout << "\t" << "mutr_disp_lateral_vtx = " << "\t" << mutr_disp_lateral_vtx
      << endl;
  cout << "\t" << "mutr_disp_pt_vtx = " << "\t" << mutr_disp_pt_vtx << endl;
  cout << "\t" << "mutr_disp_tangent_theta = " << "\t"
      << mutr_disp_tangent_theta << endl;
  cout << "\t" << "mutr_disp_tangent_phi = " << "\t" << mutr_disp_tangent_phi
      << endl;
  cout << "\t" << "mutr_disp_lateral_z_ref = " << "\t"
      << mutr_disp_lateral_z_ref << endl;
  cout << "\t" << "mutr_disp_pt_z_ref = " << "\t" << mutr_disp_pt_z_ref << endl;

  cout << "\t" << "z_ref_point_kalman = " << "\t"
      << Form("[%f %f %f]", z_ref_point_kalman.x(), z_ref_point_kalman.y(),
          z_ref_point_kalman.z()) << endl;

  cout << "\t" << "Internal fit for absorber : " << "\t"
      << Form("[x y](z) = [%f %f] + [%f %f] * (z - %f)", trk_par_absorb_scat.x0,
          trk_par_absorb_scat.y0, trk_par_absorb_scat.tx,
          trk_par_absorb_scat.ty, trk_par_absorb_scat.z0) << endl;

  cout << "Ref plane Kalman Fit FVTX Side" << endl;
  trk_par_kalman_zref.print(cout);

  cout << "Ref plane Kalman Fit MuTr Side" << endl;
  trk_par_kalman_mutr_zref.print(cout);

  return;
}

//______________________________________________________
void
TForwardMPTrack::interanl_fit()
{

  t_vec_nodes_const vec_nodes;

  unsigned int n_nodes = get_n_nodes();

  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode * node = get_node(i);
      assert(node);

      vec_nodes.push_back(node);

    }

  interanl_fit(vec_nodes, trk_par, trk_par_absorb_scat);

  // calculate residuals
  for (unsigned int i = 0; i < vec_nodes.size(); i++)
    {

      TFvtxMPNode * node = const_cast<TFvtxMPNode *>(vec_nodes.at(i));
      assert(node);

      const bool is_after_absorber = TForwardMPTrack::is_after_absorber(
          node->p_det.z());

      bool is_mul_scattering_constraint = node->is_mul_scattering_constraint();

      double fit = 0;

      if (!is_mul_scattering_constraint)
        {
          fit += trk_par.get_measurement(node);
        }

      if (is_mul_scattering_constraint or is_after_absorber)
        {
          fit += trk_par_absorb_scat.get_measurement(node);
        }

      if (use_kalman_fit)
        fit += node->fit_kalman;

      // internal cross check
      node->fit = fit;
      node->residu = node->meas - node->fit;
    }

  // calculate projections
  for (unsigned int i = 0; i < vec_nodes.size(); i++)
    {

      TFvtxMPNode * node = const_cast<TFvtxMPNode *>(vec_nodes.at(i));
      assert(node);

      node->p_fit = internal_fit_proj(node->p_det.Z());
      node->v_fit = internal_fit_vec(node->p_det.Z());
      if (use_kalman_fit)
        {
          const TVector3 xy_kalman(node->p_kalman.x(), node->p_kalman.y(), 0);

          node->p_fit += xy_kalman;
          node->v_fit += TVector3(
              node->momentum_kalman.x() / node->momentum_kalman.z(),
              node->momentum_kalman.y() / node->momentum_kalman.z(), 0);
        }

      // internal cross check
      if (abs(
          node->fit - node->get_measurement(node->p_fit.x(), node->p_fit.y()))
          > 10e-6 and (!node->is_mul_scattering_constraint()))
        {
          cout
              << "TForwardMPTrack::interanl_fit - cross check on residual failed for following node. Expected fit = "
              << node->get_measurement(node->p_fit.x(), node->p_fit.y())
              << endl;
          node->Print();
          assert(0);
        }
//      node->residu = node->meas - node->fit;

//      cout << "TForwardMPTrack::interanl_fit() - INFO - " << node->p_det.Z()
//          << ": " << node->p_kalman.x() << " + "
//          << trk_par.get_proj(node->p_det.Z()).x() << " = " << node->p_fit.X()
//          << endl;

    }

//  const double phi = atan2(trk_par_kalman_zref.get_y(),
//       trk_par_kalman_zref.get_x());
//  const double angle = phi - M_PI / 2; // roughly estimated pt x z, i.e. strip direction

  n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

//      double residual = node->residu;

      switch (node->constraint_type)
        {
      case TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_PT:
        mutr_disp_pt_z_ref = node->residu;
        break;
      case TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_LATERAL:
        mutr_disp_lateral_z_ref = node->residu;
        break;
      case TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_PT:
        mutr_disp_tangent_theta = node->residu;
        break;
      case TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_LATERAL:
        mutr_disp_tangent_phi = node->residu;
        break;

      default:

        break;
        }

    }

}

//______________________________________________________
void
TForwardMPTrack::interanl_fit(const t_vec_nodes_const& vec_nodes,
    TFvtxTrkPar &trk_par_fit, TFvtxTrkPar &trk_par_scat_fit)
{

  gMatrixCheck = 1;

  static const int n_fit_v = 8; // in x , y, tx , ty

  const unsigned int n_data = vec_nodes.size();

  if (n_data <= 0)
    cout << "TForwardMPTrack::interanl_fit - Error - No data to fit" << endl;

  TMatrixD matY(n_data, 1);
  TMatrixD matX(n_data, n_fit_v);
  TMatrixD matOmega(n_data, n_data);
//  TMatrixD matBeta(n_fit_v, 1);// in x , y, tx , ty

  int n_MULT_SCAT_CONSTRAINT = 0;

  for (unsigned int i = 0; i < n_data; i++)
    {

      const TFvtxMPNode * node = vec_nodes[i];
      assert(node);
      const bool is_after_absorber = TForwardMPTrack::is_after_absorber(
          node->p_det.z());

      bool is_mul_scattering_constraint = node->is_mul_scattering_constraint();

      if (is_mul_scattering_constraint)
        n_MULT_SCAT_CONSTRAINT++;

      matY(i, 0) = use_kalman_fit ? node->residu_kalman : node->meas;

      if (!is_mul_scattering_constraint)
        {
          matX(i, 0) = node->dwdx;
          matX(i, 1) = node->dwdy;
          matX(i, 2) = node->dwdtx;
          matX(i, 3) = node->dwdty;
        }

      if (is_mul_scattering_constraint)
        {
          matX(i, 0 + 4) = node->dwdx;
          matX(i, 1 + 4) = node->dwdy;
          matX(i, 2 + 4) = node->dwdtx;
          matX(i, 3 + 4) = node->dwdty;
        }

      else if (node->get_node_type() == TFvtxMPNode::MuTr)
        {
          const TFvtxMPNode_MuTr * node_mutr =
              dynamic_cast<const TFvtxMPNode_MuTr *>(node);
          assert(node_mutr);

          assert(is_after_absorber);
          matX(i, 0 + 4) = node_mutr->dwdx; //dwdx;
          matX(i, 1 + 4) = node_mutr->dwdtx_multi_scat; //dwdtx;
          matX(i, 2 + 4) = node_mutr->dwdy; //dwdy;
          matX(i, 3 + 4) = node_mutr->dwdty_multi_scat; //dwdty;
        }

      else if (node->get_node_type() == TFvtxMPNode::MuID)
        {
          const TFvtxMPNode_MuID * node_muid =
              dynamic_cast<const TFvtxMPNode_MuID *>(node);
          assert(node_muid);

          assert(is_after_absorber);
          matX(i, 0 + 4) = node_muid->dwdx; //dwdx;
          matX(i, 1 + 4) = node_muid->dwdtx_multi_scat; //dwdtx;
          matX(i, 2 + 4) = node_muid->dwdy; //dwdy;
          matX(i, 3 + 4) = node_muid->dwdty_multi_scat; //dwdty;
        }

      matOmega(i, i) = node->sigma * node->sigma;

    }

  assert(n_MULT_SCAT_CONSTRAINT == 4);

  //Generalized least squares fit

  matOmega.Invert();
  TMatrixD matXT(matX);
  matXT.Transpose(matXT);

  TMatrixD mat = matXT * matOmega * matX;
  mat.InvertFast();
  const TMatrixD matBeta = mat * matXT * matOmega * matY;

  // ---------------------------------
  // Save results
  // ---------------------------------

  // save fit result
  trk_par_fit.set( //
      matBeta(0, 0), //      double x=0,
      matBeta(1, 0), //          double y=0,
      z0_fit, //          double z=0,
      matBeta(2, 0), //          double px=0,
      matBeta(3, 0) //          double py=0,
          );

  trk_par_scat_fit.set( //
      matBeta(0 + 4, 0), //      double x=0,
      matBeta(1 + 4, 0), //          double y=0,
      z0_fit, //          double z=0,
      matBeta(2 + 4, 0), //          double px=0,
      matBeta(3 + 4, 0) //          double py=0,
          );

}

double
TForwardMPTrack::get_mutr_disp_lateral_z_ref_kalman() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_LATERAL)
        {
          return node->residu_kalman;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_pt_z_ref_kalman() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_PT)
        {
          return node->residu_kalman;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_tangent_phi_kalman() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_PT)
        {
          return node->residu_kalman;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_tangent_theta_kalman() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_PT)
        {
          return node->residu_kalman;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_lateral_z_ref() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_LATERAL)
        {
          return node->residu;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_pt_z_ref() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DR_PT)
        {
          return node->residu;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_tangent_phi() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_PT)
        {
          return node->residu;
        }

    }

  return INVALID;
}

double
TForwardMPTrack::get_mutr_disp_tangent_theta() const
{
  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::Constraint);
  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type
          == TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_DTHETA_PT)
        {
          return node->residu;
        }

    }

  return INVALID;
}

