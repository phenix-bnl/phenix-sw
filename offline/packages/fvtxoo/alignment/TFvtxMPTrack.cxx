/*
 * TFvtxMPTrack.cxx
 *
 *  Created on: Nov 6, 2012
 *      Author: jinhuang
 */
// $$Id: TFvtxMPTrack.cxx,v 1.10 2015/09/09 01:50:17 jinhuang Exp $$
/*!
 * \file TFvtxMPTrack.cxx
 * \brief
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $$Revision: 1.10 $$
 * \date $$Date: 2015/09/09 01:50:17 $$
 */

#include "TFvtxMPTrack.h"

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
//using namespace MILLEPEDE;

ClassImp(TFvtxMPTrack);

//______________________________________________________
TFvtxMPTrack::TFvtxMPTrack() :
    trk_par_wz_fit(), trk_par()
{
  nNodes_FVTX = 0;
  Nodes_FVTX = new TClonesArray("TFvtxMPNode_FVTX", MAX_N_NODES);

  nNodes_VTX = 0;
  Nodes_VTX = new TClonesArray("TFvtxMPNode_VTX", MAX_N_NODES);

  nNodes_MuTr = 0;
  Nodes_MuTr = new TClonesArray("TFvtxMPNode_MuTr", MAX_N_NODES);

  nNodes_MuID = 0;
  Nodes_MuID = new TClonesArray("TFvtxMPNode_MuID", MAX_N_NODES);

  nNodes_Constraint = 0;
  Nodes_Constraint = new TClonesArray("TFvtxMPNode_Constraint", MAX_N_NODES);

  Reset();
}

//______________________________________________________
TFvtxMPTrack::~TFvtxMPTrack()
{

  Nodes_FVTX->Delete();
  delete Nodes_FVTX;

  Nodes_MuTr->Delete();
  delete Nodes_MuTr;

  Nodes_MuID->Delete();
  delete Nodes_MuID;

  Nodes_VTX->Delete();
  delete Nodes_VTX;

  Nodes_Constraint->Delete();
  delete Nodes_Constraint;

}

//______________________________________________________
void
TFvtxMPTrack::Clear(Option_t *opt)
{
  TObject::Clear(opt);

  Reset();
}

//______________________________________________________
void
TFvtxMPTrack::Reset()
{

  run_num = INVALID;
  event_num = INVALID;
  track_index = INVALID;

  vtx_point.SetXYZ(0, 0, INVALID);
  vtxzp = INVALID;
  bbcz = INVALID;

  reject_event = true;
  reject_track = abs(INVALID);
  use_kalman_fit = false;
  chi_square_mutr = INVALID;
  ndf_mutr = INVALID;
  chi_square_fvtx = INVALID;
  ndf_fvtx = INVALID;
  z0reco_fvtx = INVALID;
  phi_avg = INVALID;
  phi_acpt_cent = INVALID;
  phi_acpt_width = INVALID;
  disp_lateral_vtx = INVALID;
  disp_pt_vtx = INVALID;
  z0reco_no_VTX_DCA = INVALID;

  trk_par_wz_fit.set(INVALID, INVALID);
  trk_par.set(0, 0, INVALID, INVALID, INVALID);
  z0_fit = 0;

  mutr_disp_lateral_vtx = INVALID;
  mutr_disp_pt_vtx = INVALID;
  vtx_point_kalman.SetXYZ(0, 0, INVALID);

  z_ref = INVALID;
  z_ref_point_kalman.SetXYZ(0, 0, INVALID);

//  p_kalman.SetXYZ(0, 0, INVALID);
  new (&trk_par_kalman) TMutTrkPar();
  trk_par_kalman.set_z(INVALID);
  trk_par_kalman.set_pz(INVALID);

  new (&trk_par_kalman_mutr) TMutTrkPar();
  trk_par_kalman_mutr.set_z(INVALID);
  trk_par_kalman_mutr.set_pz(INVALID);

  new (&trk_par_kalman_zref) TMutTrkPar();
  trk_par_kalman_zref.set_z(INVALID);
  trk_par_kalman_zref.set_pz(INVALID);

  new (&trk_par_kalman_mutr_zref) TMutTrkPar();
  trk_par_kalman_mutr_zref.set_z(INVALID);
  trk_par_kalman_mutr_zref.set_pz(INVALID);

  muid_lastgap = 0;
  dg0 = INVALID;
  ddg0 = INVALID;

  const Option_t * opt = "C";

  nNodes_FVTX = 0;
  Nodes_FVTX->Clear(opt);
  nNodes_MuTr = 0;
  Nodes_MuTr->Clear(opt);
  nNodes_MuID = 0;
  Nodes_MuID->Clear(opt);
  nNodes_VTX = 0;
  Nodes_VTX->Clear(opt);
  nNodes_Constraint = 0;
  Nodes_Constraint->Clear(opt);

}

//______________________________________________________
void
TFvtxMPTrack::Print(Option_t *option) const
{
  TObject::Print(option);

  if (!IsValid())
    cout << "\t" << "======== INVALID TRACK ! ========" << endl;

  cout << Form("Run %d event %d track %d", run_num, event_num, track_index)
      << endl;

  cout << "\t" << "vertex = " << "\t"
      << Form("[%f %f %f]", vtx_point.X(), vtx_point.Y(), vtx_point.Z())
      << endl;
  cout << "\t" << "momentum = " << "\t"
      << Form("[%f %f %f]", trk_par_kalman.get_px(), trk_par_kalman.get_py(),
          trk_par_kalman.get_pz()) << endl;

  cout << "\t" << "reject_track = " << "\t" << reject_track << endl;
  cout << "\t" << "use_kalman_fit = " << "\t" << use_kalman_fit << endl;

  cout << "\t" << "chi_square_fvtx = " << "\t" << chi_square_fvtx << endl;
  cout << "\t" << "ndf_fvtx = " << "\t" << ndf_fvtx << endl;
  cout << "\t" << "z0reco_fvtx = " << "\t" << z0reco_fvtx << endl;
  cout << "\t" << "phi_avg = " << "\t" << phi_avg << endl;
  cout << "\t" << "phi_acpt_cent = " << "\t" << phi_acpt_cent << endl;
  cout << "\t" << "phi_acpt_width = " << "\t" << phi_acpt_width << endl;
  cout << "\t" << "disp_lateral_vtx = " << "\t" << disp_lateral_vtx << endl;
  cout << "\t" << "disp_pt_vtx = " << "\t" << disp_pt_vtx << endl;

  cout << "\t" << "W-Z fit : " << "\t"
      << Form("W(z) = %f + %f * z", trk_par_wz_fit.w, trk_par_wz_fit.tw)
      << endl;
  cout << "\t" << "Internal fit : " << "\t"
      << Form("[x y](z) = [%f %f] + [%f %f] * (z - %f)", trk_par.x0, trk_par.y0,
          trk_par.tx, trk_par.ty, trk_par.z0) << endl;
  cout << "\t" << "internal fit z0 = " << "\t" << z0_fit << endl;

  cout << Form("Have %d FVTX hits:", nNodes_FVTX) << endl;
  if (nNodes_FVTX)
    Nodes_FVTX->Print();

  cout << Form("Have %d VTX hits:", nNodes_VTX) << endl;
  if (nNodes_VTX)
    Nodes_VTX->Print();

  cout << Form("Have %d MuTr hits:", nNodes_MuTr) << endl;
  if (nNodes_MuTr)
    {

      cout << "\t" << "chi_square_mutr = " << "\t" << chi_square_mutr << endl;
      cout << "\t" << "ndf_mutr = " << "\t" << ndf_mutr << endl;

      cout << "\t" << "momentum MuTr = " << "\t"
          << Form("[%f %f %f] @ z = %f cm", trk_par_kalman_mutr.get_px(),
              trk_par_kalman_mutr.get_py(), trk_par_kalman_mutr.get_pz(),
              trk_par_kalman_mutr.get_z()) << endl;
      cout << "Last MuID hit gap :";
      cout << "\t" << "muid_lastgap = " << muid_lastgap << endl;
      cout << "\t" << "dg0 = " << dg0 << endl;
      cout << "\t" << "ddg0 = " << ddg0 << endl;

      Nodes_MuTr->Print();

    }

  cout << Form("Have %d MuID hits:", nNodes_MuID) << endl;
  if (nNodes_MuID)
    {

      cout << "Last MuID hit gap :";
      cout << "\t" << "muid_lastgap = " << muid_lastgap << endl;
      cout << "\t" << "dg0 = " << dg0 << endl;
      cout << "\t" << "ddg0 = " << ddg0 << endl;

      Nodes_MuID->Print();

    }

  cout << Form("Have %d Constraints:", nNodes_Constraint) << endl;
  if (nNodes_Constraint)
    Nodes_Constraint->Print();

  //consistency checks
  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {

      TFvtxMPNode_FVTX * node = dynamic_cast<TFvtxMPNode_FVTX *> ((*Nodes_FVTX)[i]);
      assert(node);

      if (trk_par_kalman.get_pz() != TFvtxMPNode::INVALID)
        assert(
            (node->p_det.z() - vtx_point.z() )* trk_par_kalman.get_pz() > 0);
      // z>0 -> pz >0
//      assert( (node->arm -.5)* trk_par_kalman.get_pz() > 0  ); // arm 1 -> pz >0

    }
  for (unsigned int i = 0; i < nNodes_MuTr; i++)
    {

      TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *> ((*Nodes_MuTr)[i]);
      assert(node);

      if (trk_par_kalman.get_pz() != TFvtxMPNode::INVALID)
        assert( node->p_det.z() * trk_par_kalman.get_pz() > 0);

      // z>0 -> pz >0
      if (trk_par_kalman.get_pz() != TFvtxMPNode::INVALID)
        assert( (node->arm -.5)* trk_par_kalman.get_pz() > 0);
      // arm 1 -> pz >0

    }
  for (unsigned int i = 0; i < nNodes_MuID; i++)
    {

      TFvtxMPNode_MuID * node = dynamic_cast<TFvtxMPNode_MuID *> ((*Nodes_MuID)[i]);
      assert(node);

      if (trk_par_kalman.get_pz() != TFvtxMPNode::INVALID)
        assert( node->p_det.z() * trk_par_kalman.get_pz() > 0);

      // z>0 -> pz >0
      if (trk_par_kalman.get_pz() != TFvtxMPNode::INVALID)
        assert( (node->arm -.5)* trk_par_kalman.get_pz() > 0);
      // arm 1 -> pz >0

    }

  return;
}

//______________________________________________________
bool
TFvtxMPTrack::IsValid(Option_t *option) const
{

  const unsigned int n_nodes = get_n_nodes();

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      const TFvtxMPNode * node = get_node(i);
      assert(node);

      if (!node->IsValid())
        return false;
    }

  return true;
}

//______________________________________________________
void
TFvtxMPTrack::interanl_fit(const t_vec_nodes_const& vec_nodes,
    TFvtxTrkPar &trk_par_fit)
{

  gMatrixCheck = 1;

  static const int n_fit_v = 4; // in x , y, tx , ty

  const unsigned int n_data = vec_nodes.size();

  if (n_data == 0)
    cout << "TFvtxMPTrack::interanl_fit - Error - No data to fit" << endl;

  TMatrixD matY(n_data, 1);
  TMatrixD matX(n_data, n_fit_v);
  TMatrixD matOmega(n_data, n_data);
//  TMatrixD matBeta(n_fit_v, 1);// in x , y, tx , ty

  for (unsigned int i = 0; i < n_data; i++)
    {

      const TFvtxMPNode * node = vec_nodes[i];
      assert(node);

      matY(i, 0) = use_kalman_fit ? node->residu_kalman : node->meas;

      matX(i, 0) = node->dwdx;
      matX(i, 1) = node->dwdy;
      matX(i, 2) = node->dwdtx;
      matX(i, 3) = node->dwdty;

      matOmega(i, i) = node->sigma * node->sigma;

    }

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

}

//______________________________________________________
void
TFvtxMPTrack::interanl_fit()
{

  t_vec_nodes_const vec_nodes;

  const unsigned int n_nodes = get_n_nodes();

  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode * node = get_node(i);
      assert(node);

      if (node->get_node_type() == TFvtxMPNode::MuTr)
        continue;
//      static bool once = true;
//      if (once)
//        {
//          once = false;
//          cout <<"TFvtxMPTrack::interanl_fit - use all nodes in the fitting"<<endl;
//        }

      vec_nodes.push_back(node);

    }

  interanl_fit(vec_nodes, trk_par);

  // calculate projections

  for (unsigned int i = 0; i < vec_nodes.size(); i++)
    {

      TFvtxMPNode * node = const_cast<TFvtxMPNode *>(vec_nodes.at(i));
      assert(node);

      node->p_fit = trk_par.get_proj(node->p_det.Z());
      node->v_fit.SetXYZ(trk_par.get_tx(), trk_par.get_ty(), 1);
      if (use_kalman_fit)
        {
          const TVector3 xy_kalman(node->p_kalman.x(), node->p_kalman.y(), 0);

          node->p_fit += xy_kalman;
          node->v_fit += TVector3(
              node->momentum_kalman.x() / node->momentum_kalman.z(),
              node->momentum_kalman.y() / node->momentum_kalman.z(), 0);
        }

      node->fit = node->get_measurement(node->p_fit.x(), node->p_fit.y());
      node->residu = node->meas - node->fit;

//      cout << "TFvtxMPTrack::interanl_fit() - INFO - " << node->p_det.Z()
//          << ": " << node->p_kalman.x() << " + "
//          << trk_par.get_proj(node->p_det.Z()).x() << " = " << node->p_fit.X()
//          << endl;

    }

}

//______________________________________________________
void
TFvtxMPTrack::interanl_fit_no_VTX_DCA_Constraint()
{

  t_vec_nodes_const vec_nodes;

  const unsigned int n_nodes = get_n_nodes();

  for (unsigned int i = 0; i < n_nodes; i++)
    {

      const TFvtxMPNode * node = get_node(i);
      assert(node);

      if (node->get_node_type() == TFvtxMPNode::Constraint)
        {
          const TFvtxMPNode_Constraint * node_con =
              dynamic_cast<const TFvtxMPNode_Constraint *>(node);
          assert(node_con);

                if (node->get_node_type() == TFvtxMPNode::MuTr)
                  continue;
//                static bool once = true;
//                if (once)
//                  {
//                    once = false;
//                    cout <<"TFvtxMPTrack::interanl_fit - use all nodes in the fitting"<<endl;
//                  }

          if (node_con->constraint_type
              == TFvtxMPNode_Constraint::DCA_CONTRAINT_VTX)
            {
              // ignore DCA constraint at VTX vertex
              continue;
            }

        }

      vec_nodes.push_back(node);
    }

  TFvtxTrkPar trk_par_tmp;
  interanl_fit(vec_nodes, trk_par_tmp);

  const double phi = phi_acpt_cent; // roughly estimated azimuthal direction of the track
  const double angle = phi - M_PI / 2; // roughly estimated pt x z, i.e. strip direction
//  const double vtx_x = vtx_point.x();
//  const double vtx_y = vtx_point.y();
  const double vtx_z = vtx_point.z();

  TVector3 fit = trk_par_tmp.get_proj(vtx_z);
  if (use_kalman_fit)
    {
      if (vtx_point_kalman.z() == INVALID)
        cout
            << "TFvtxMPTrack::interanl_fit_no_VTX_DCA_Constraint - Error - invalid Kalman fit"
            << endl;

      const TVector3 xy_kalman(vtx_point_kalman.x(), vtx_point_kalman.y(), 0);
      fit += xy_kalman;
    }

  const TVector3 disp = vtx_point - fit;

  disp_lateral_vtx = disp.x() * cos(angle) + disp.y() * sin(angle);
  disp_pt_vtx = disp.x() * cos(phi) + disp.y() * sin(phi);

  // FVTX vertex location with track laterally constraint in the phi acceptance window
  const double pt_over_pz = (trk_par_tmp.tx * cos(phi)
      + trk_par_tmp.ty * sin(phi));
  z0reco_no_VTX_DCA = vtx_z - disp_pt_vtx / pt_over_pz;
}

//______________________________________________________
double
TFvtxMPTrack::interanl_fit_FVTX_self_proj(const int station)
{
  if (!fvtx_one_hit_per_station(0, 0))
    return 0;

  t_vec_nodes_const vec_nodes;

  TFvtxMPNode_FVTX * node_target = NULL;

  unsigned int n_nodes = get_n_nodes(TFvtxMPNode::FVTX);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      TFvtxMPNode_FVTX * node = dynamic_cast<TFvtxMPNode_FVTX *>(get_node(
          TFvtxMPNode::FVTX, i));
      assert(node);

      if (node->station == station)
        {
          assert(node_target == NULL);
          // not filled before

          node_target = node;
        }
      else
        vec_nodes.push_back(node);
    }

  // four station track
  assert(node_target);
  assert(vec_nodes.size() == 3);

  // add phi constraint
  n_nodes = get_n_nodes(TFvtxMPNode::Constraint);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type == TFvtxMPNode_Constraint::LAT_CONTRAINT_STAION3 //
      or//
          node->constraint_type == TFvtxMPNode_Constraint::LAT_CONTRAINT_VTX)
        vec_nodes.push_back(node);
    }

  // there are two more constraint on phi window
  assert(vec_nodes.size() == 5);

  // doing the fit
  TFvtxTrkPar trk_par_tmp;
  interanl_fit(vec_nodes, trk_par_tmp);

  // calculate residual
  TVector3 fit = trk_par_tmp.get_proj(node_target->p_det.Z());

  return node_target->meas - node_target->get_measurement(fit.X(), fit.Y());
}

////______________________________________________________
//void
//TFvtxMPTrack::interanl_fit_MuTr_Alone()
//{
//
//  t_vec_nodes_const vec_nodes;
//
//  const unsigned int n_nodes = get_n_nodes(TFvtxMPNode::MuTr);
//
//  if (n_nodes < 6)
//    {
//      cout
//          << "TFvtxMPTrack::interanl_fit_MuTr_Alone - Error - too few MuTr hits = "
//          << n_nodes << endl;
//      return;
//    }
//
//  for (unsigned int i = 0; i < n_nodes; i++)
//    {
//
//      const TFvtxMPNode * node = get_node(TFvtxMPNode::MuTr, i);
//      assert(node);
//
//      vec_nodes.push_back(node);
//    }
//
//  TFvtxTrkPar trk_par_tmp;
//  interanl_fit(vec_nodes, trk_par_tmp);
//
////  const double phi = atan2(trk_par_kalman_mutr_zref.get_py(),
////      trk_par_kalman_mutr_zref.get_px()); // roughly estimated azimuthal direction of the track
//  const double phi = atan2(trk_par_kalman_zref.get_y(),
//      trk_par_kalman_zref.get_x()); // roughly estimated azimuthal direction of the track
//  const double angle = phi - M_PI / 2; // roughly estimated pt x z, i.e. strip direction
////  const double vtx_x = vtx_point.x();
////  const double vtx_y = vtx_point.y();
////  const double vtx_z = vtx_point.z();
//
//  // calculate projections
//  for (unsigned int i = 0; i < vec_nodes.size(); i++)
//    {
//      TFvtxMPNode * node = const_cast<TFvtxMPNode *>(vec_nodes.at(i));
//
//      assert(node);
//
//      node->p_fit = trk_par_tmp.get_proj(node->p_det.Z());
//      node->v_fit.SetXYZ(trk_par_tmp.get_tx(), trk_par_tmp.get_ty(), 1);
//      if (use_kalman_fit)
//        {
//          const TVector3 xy_kalman(node->p_kalman.x(), node->p_kalman.y(), 0);
//
//          node->p_fit += xy_kalman;
//          node->v_fit += TVector3(
//              node->momentum_kalman.x() / node->momentum_kalman.z(),
//              node->momentum_kalman.y() / node->momentum_kalman.z(), 0);
//        }
//
////      cout << "TFvtxMPTrack::interanl_fit_MuTr_Alone() - INFO - " << node->p_det.Z()
////          << ": " << node->p_kalman.x() << " + "
////          << trk_par.get_proj(node->p_det.Z()).x() << " = " << node->p_fit.X()
////          << endl;
//
//      node->fit = node->get_measurement(node->p_fit.x(), node->p_fit.y());
//      node->residu = node->meas - node->fit;
//
//    }
//
////  // vertex projection
////    {
////      TVector3 fit = trk_par_tmp.get_proj(vtx_z);
////      if (use_kalman_fit)
////        {
////          assert(vtx_point_kalman.z()!=INVALID);
////          const TVector3 xy_kalman(vtx_point_kalman.x(), vtx_point_kalman.y(),
////              0);
////          fit += xy_kalman;
////        }
////      const TVector3 disp = vtx_point - fit;
////
////      mutr_disp_lateral_vtx = disp.x() * cos(angle) + disp.y() * sin(angle);
////      mutr_disp_pt_vtx = disp.x() * cos(phi) + disp.y() * sin(phi);
////    }
//
//  // z_ref projection
//    {
//      TVector3 fit_mutr = trk_par_tmp.get_proj(z_ref);
//      TVector3 fit_global = trk_par.get_proj(z_ref);
//
//      if (use_kalman_fit)
//        {
//          assert(trk_par_kalman_mutr_zref.get_z()==z_ref);
//          assert(trk_par_kalman_zref.get_z()==z_ref);
//
//          fit_mutr += TVector3(trk_par_kalman_mutr_zref.get_x(),
//              trk_par_kalman_mutr_zref.get_y(), 0);
//          fit_global += TVector3(trk_par_kalman_zref.get_x(),
//              trk_par_kalman_zref.get_y(), 0);
//        }
//
//      TVector3 disp = fit_global - fit_mutr;
//
//      mutr_disp_lateral_z_ref = disp.x() * cos(angle) + disp.y() * sin(angle);
//      mutr_disp_pt_z_ref = disp.x() * cos(phi) + disp.y() * sin(phi);
//
//    }
//
//  // angle diff
//    {
//      assert(trk_par_kalman.get_pz()!=INVALID);
//
//      TVector3 fit_mutr = TVector3(trk_par_tmp.get_tx(), trk_par_tmp.get_ty(),
//          1);
//      TVector3 fit_global = TVector3(trk_par.get_tx(), trk_par.get_ty(), 1);
//
//      if (use_kalman_fit)
//        {
//          assert(trk_par_kalman_mutr_zref.get_z()==z_ref);
//          assert(trk_par_kalman_zref.get_z()==z_ref);
//
//          fit_mutr += TVector3(
//              trk_par_kalman_mutr_zref.get_px()
//                  / trk_par_kalman_mutr_zref.get_pz(),
//              trk_par_kalman_mutr_zref.get_py()
//                  / trk_par_kalman_mutr_zref.get_pz(), 0);
//
//          fit_global += TVector3(
//              trk_par_kalman_zref.get_px() / trk_par_kalman_zref.get_pz(),
//              trk_par_kalman_zref.get_py() / trk_par_kalman_zref.get_pz(), 0);
//        }
//      TVector3 disp = fit_global - fit_mutr;
//
//      mutr_disp_tangent_theta = disp.x() * cos(phi) + disp.y() * sin(phi);
//      mutr_disp_tangent_phi = disp.x() * cos(angle) + disp.y() * sin(angle);
//
//    }
//}

//______________________________________________________
double
TFvtxMPTrack::residual_interanl_fit_MuTr_self_proj(
//! Arm [0,1]
    short arm,
    //! Station [0,2]
    short station,
    //! Octant [0,7]
    short octant,
    //! Half octant [0,1]
    short half_octant,
    //! Gap [0,2]
    short gap,
    //! Index [0,1023]
    short cathode)
{

  if (!can_interanl_fit_MuTr_self_proj(
  //! Arm [0,1]
      arm,
      //! Station [0,2]
      station,
      //! Octant [0,7]
      octant,
      //! Half octant [0,1]
      half_octant,
      //! Gap [0,2]
      gap,
      //! Index [0,1023]
      cathode))
    return -99999;


  t_vec_nodes_const vec_nodes;
  TFvtxMPNode_MuTr * node_target = NULL;
  unsigned int n_nodes = get_n_nodes(TFvtxMPNode::MuTr);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *>(get_node(
          TFvtxMPNode::MuTr, i));
      assert(node);

      if (node->arm == arm)
        if (node->station == station)
          if (node->octant == octant)
            if (node->half_octant == half_octant)
              if (node->gap == gap)
                if (node->cathode == cathode)
                  {
                    node_target = node;
                    continue;
                  }

      vec_nodes.push_back(node);

    }

  // four station track
  assert(node_target);

  // add phi constraint
  n_nodes = get_n_nodes(TFvtxMPNode::Constraint);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      const TFvtxMPNode_Constraint * node =
          dynamic_cast<const TFvtxMPNode_Constraint *>(get_node(
              TFvtxMPNode::Constraint, i));
      assert(node);

      if (node->constraint_type == TFvtxMPNode_Constraint::LAT_CONTRAINT_STAION3 //
      or//
          node->constraint_type == TFvtxMPNode_Constraint::LAT_CONTRAINT_VTX)
        vec_nodes.push_back(node);
    }


  // doing the fit
  TFvtxTrkPar trk_par_tmp;
  interanl_fit(vec_nodes, trk_par_tmp);

  // calculate residual
  TVector3 fit = trk_par_tmp.get_proj(node_target->p_det.Z());

  return node_target->meas - node_target->get_measurement(fit.X(), fit.Y());
}

//______________________________________________________
bool
TFvtxMPTrack::can_interanl_fit_MuTr_self_proj(
//! Arm [0,1]
    short arm,
    //! Station [0,2]
    short station,
    //! Octant [0,7]
    short octant,
    //! Half octant [0,1]
    short half_octant,
    //! Gap [0,2]
    short gap,
    //! Index [0,1023]
    short cathode)
{

  unsigned int n_nodes = get_n_nodes(TFvtxMPNode::MuTr);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *>(get_node(
          TFvtxMPNode::MuTr, i));
      assert(node);

      if (node->arm == arm)
        if (node->station == station)
          if (node->octant == octant)
            if (node->half_octant == half_octant)
              if (node->gap == gap)
                if (node->cathode == cathode)
                  {
                    return true;
                  }
    }

  return false;
}

//______________________________________________________
int
TFvtxMPTrack::MuTr_strip_hit(
//! Arm [0,1]
    short arm,
    //! Station [0,2]
    short station,
    //! Octant [0,7]
    short octant,
    //! Half octant [0,1]
    short half_octant,
    //! Gap [0,2]
    short gap,
    //! Index [0,1023]
    short cathode)
{

  unsigned int n_nodes = get_n_nodes(TFvtxMPNode::MuTr);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *>(get_node(
          TFvtxMPNode::MuTr, i));
      assert(node);

      if (node->arm == arm)
        if (node->station == station)
          if (node->octant == octant)
            if (node->half_octant == half_octant)
              if (node->gap == gap)
                if (node->cathode == cathode)
                  {
                    return node->strip;
                  }
    }

  return -99999;
}

//______________________________________________________
 double
TFvtxMPTrack::MuTr_p_fit_v_proj(
//! Arm [0,1]
    short arm,
    //! Station [0,2]
    short station,
    //! Octant [0,7]
    short octant,
    //! Half octant [0,1]
    short half_octant,
    //! Gap [0,2]
    short gap,
    //! Index [0,1023]
    short cathode)
{

  unsigned int n_nodes = get_n_nodes(TFvtxMPNode::MuTr);

  for (unsigned int i = 0; i < n_nodes; i++)
    {
      TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *>(get_node(
          TFvtxMPNode::MuTr, i));
      assert(node);

      if (node->arm == arm)
        if (node->station == station)
          if (node->octant == octant)
            if (node->half_octant == half_octant)
              if (node->gap == gap)
                if (node->cathode == cathode)
                  {
                    return node -> get_p_fit_v_proj();
                  }
    }

  return -99999;
}

 //______________________________________________________
  double
 TFvtxMPTrack::MuTr_p_fit_w_proj(
 //! Arm [0,1]
     short arm,
     //! Station [0,2]
     short station,
     //! Octant [0,7]
     short octant,
     //! Half octant [0,1]
     short half_octant,
     //! Gap [0,2]
     short gap,
     //! Index [0,1023]
     short cathode)
 {

   unsigned int n_nodes = get_n_nodes(TFvtxMPNode::MuTr);

   for (unsigned int i = 0; i < n_nodes; i++)
     {
       TFvtxMPNode_MuTr * node = dynamic_cast<TFvtxMPNode_MuTr *>(get_node(
           TFvtxMPNode::MuTr, i));
       assert(node);

       if (node->arm == arm)
         if (node->station == station)
           if (node->octant == octant)
             if (node->half_octant == half_octant)
               if (node->gap == gap)
                 if (node->cathode == cathode)
                   {
                     return node -> get_p_fit_w_proj();
                   }
     }

   return -99999;
 }

//______________________________________________________
TFvtxMPNode *
TFvtxMPTrack::add_node(TFvtxMPNode::node_type type)
{

  switch (type)
    {

  case TFvtxMPNode::FVTX:

    nNodes_FVTX++;

    assert(Nodes_FVTX);
    new ((*Nodes_FVTX)[nNodes_FVTX - 1]) TFvtxMPNode_FVTX();

    return (TFvtxMPNode *) ((*Nodes_FVTX)[nNodes_FVTX - 1]);
    break;

  case TFvtxMPNode::VTX:

    nNodes_VTX++;

    assert(Nodes_VTX);
    new ((*Nodes_VTX)[nNodes_VTX - 1]) TFvtxMPNode_VTX();

    return (TFvtxMPNode *) ((*Nodes_VTX)[nNodes_VTX - 1]);
    break;

  case TFvtxMPNode::MuTr:

    nNodes_MuTr++;

    assert(Nodes_MuTr);
    new ((*Nodes_MuTr)[nNodes_MuTr - 1]) TFvtxMPNode_MuTr();

    return (TFvtxMPNode *) ((*Nodes_MuTr)[nNodes_MuTr - 1]);
    break;

  case TFvtxMPNode::MuID:

    nNodes_MuID++;

    assert(Nodes_MuID);
    new ((*Nodes_MuID)[nNodes_MuID - 1]) TFvtxMPNode_MuID();

    return (TFvtxMPNode *) ((*Nodes_MuID)[nNodes_MuID - 1]);
    break;

  case TFvtxMPNode::Constraint:

    nNodes_Constraint++;

    assert(Nodes_Constraint);
    new ((*Nodes_Constraint)[nNodes_Constraint - 1]) TFvtxMPNode_Constraint();

    return (TFvtxMPNode *) ((*Nodes_Constraint)[nNodes_Constraint - 1]);
    break;

  default:

    cout << "TFvtxMPTrack::add_node - Error - Unknown node type " << type
        << endl;

    return 0;
    break;

    }

  return 0;
}

//______________________________________________________
void
TFvtxMPTrack::remove_last_node_fvtx()
{
  if (nNodes_FVTX > 0)
    {
      nNodes_FVTX--;
      Nodes_FVTX->RemoveAt(nNodes_FVTX);
    }
}

//______________________________________________________
void
TFvtxMPTrack::remove_nodes(TFvtxMPNode::node_type type)
{

  const Option_t * opt = "C";

  switch (type)
    {

  case TFvtxMPNode::FVTX:

    nNodes_FVTX = 0;
    Nodes_FVTX->Clear(opt);

    break;

  case TFvtxMPNode::VTX:

    nNodes_VTX = 0;
    Nodes_VTX->Clear(opt);

    break;

  case TFvtxMPNode::MuTr:

    nNodes_MuTr = 0;
    Nodes_MuTr->Clear(opt);

    break;

  case TFvtxMPNode::MuID:

    nNodes_MuID = 0;
    Nodes_MuID->Clear(opt);

    break;

  case TFvtxMPNode::Constraint:

    nNodes_Constraint = 0;
    Nodes_Constraint->Clear(opt);

    break;

  default:

    cout << "TFvtxMPTrack::add_node - Error - Unknown node type " << type
        << endl;
    break;
    }

  return;
}

//______________________________________________________
unsigned int
TFvtxMPTrack::get_n_nodes(TFvtxMPNode::node_type type) const
{

  switch (type)
    {

  case TFvtxMPNode::FVTX:

    return nNodes_FVTX;
    break;

  case TFvtxMPNode::VTX:

    return nNodes_VTX;
    break;

  case TFvtxMPNode::MuTr:

    return nNodes_MuTr;
    break;

  case TFvtxMPNode::MuID:

    return nNodes_MuID;
    break;


  case TFvtxMPNode::Constraint:

    return nNodes_Constraint;
    break;

  case TFvtxMPNode::ANY_NODE_TYPE:

    return nNodes_FVTX + nNodes_VTX + nNodes_MuTr + nNodes_MuID + nNodes_Constraint;
    break;

  default:

    cout << "TFvtxMPTrack::get_n_nodes - Error - Unknown node type " << type
        << endl;

    return 0;
    break;

    }

  return 0;

}

//______________________________________________________
TFvtxMPNode *
TFvtxMPTrack::get_node(TFvtxMPNode::node_type type, unsigned int index)
{

  switch (type)
    {

  case TFvtxMPNode::FVTX:

    if (index >= nNodes_FVTX)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_FVTX)[index]);
    break;

  case TFvtxMPNode::VTX:

    if (index >= nNodes_VTX)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_VTX)[index]);
    break;

  case TFvtxMPNode::MuTr:

    if (index >= nNodes_MuTr)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_MuTr)[index]);
    break;

  case TFvtxMPNode::MuID:

    if (index >= nNodes_MuID)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_MuID)[index]);
    break;

  case TFvtxMPNode::Constraint:

    if (index >= nNodes_Constraint)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_Constraint)[index]);
    break;

  case TFvtxMPNode::ANY_NODE_TYPE:

    if (index < nNodes_FVTX)
      return (TFvtxMPNode *) ((*Nodes_FVTX)[index]);
    index -= nNodes_FVTX;
    if (index < nNodes_VTX)
      return (TFvtxMPNode *) ((*Nodes_VTX)[index]);
    index -= nNodes_VTX;
    if (index < nNodes_MuTr)
      return (TFvtxMPNode *) ((*Nodes_MuTr)[index]);
    index -= nNodes_MuTr;
    if (index < nNodes_MuID)
      return (TFvtxMPNode *) ((*Nodes_MuID)[index]);
    index -= nNodes_MuID;
    if (index < nNodes_Constraint)
      return (TFvtxMPNode *) ((*Nodes_Constraint)[index]);
    return NULL;

    break;

  default:

    cout << "TFvtxMPTrack::get_nodes - Error - Unknown node type " << type
        << endl;

    return 0;
    break;

    }

  return 0;

}

//______________________________________________________
const TFvtxMPNode *
TFvtxMPTrack::get_node(TFvtxMPNode::node_type type, unsigned int index) const
{

  switch (type)
    {

  case TFvtxMPNode::FVTX:

    if (index >= nNodes_FVTX)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_FVTX)[index]);
    break;

  case TFvtxMPNode::VTX:

    if (index >= nNodes_VTX)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_VTX)[index]);
    break;

  case TFvtxMPNode::MuTr:

    if (index >= nNodes_MuTr)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_MuTr)[index]);
    break;


  case TFvtxMPNode::MuID:

    if (index >= nNodes_MuID)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_MuID)[index]);
    break;

  case TFvtxMPNode::Constraint:

    if (index >= nNodes_Constraint)
      return NULL;

    return (TFvtxMPNode *) ((*Nodes_Constraint)[index]);
    break;

  case TFvtxMPNode::ANY_NODE_TYPE:

    if (index < nNodes_FVTX)
      return (TFvtxMPNode *) ((*Nodes_FVTX)[index]);
    index -= nNodes_FVTX;
    if (index < nNodes_VTX)
      return (TFvtxMPNode *) ((*Nodes_VTX)[index]);
    index -= nNodes_VTX;
    if (index < nNodes_MuTr)
      return (TFvtxMPNode *) ((*Nodes_MuTr)[index]);
    index -= nNodes_MuTr;
    if (index < nNodes_MuID)
      return (TFvtxMPNode *) ((*Nodes_MuID)[index]);
    index -= nNodes_MuID;
    if (index < nNodes_Constraint)
      return (TFvtxMPNode *) ((*Nodes_Constraint)[index]);
    return NULL;

    break;

  default:

    cout << "TFvtxMPTrack::get_nodes - Error - Unknown node type " << type
        << endl;

    return 0;
    break;

    }

  return 0;

}

//______________________________________________________
void
TFvtxMPTrack::process_fvtx_track(void)
{
  StraightLineFitInWZ();

  calculate_average_half_angle();
  calculate_phi_aceptance();

  // interanl_fit() will later be called externally
//  interanl_fit();
}

//______________________________________________________
void
TFvtxMPTrack::process_mutr_only_track(void)
{
  phi_avg = atan2(trk_par_kalman_mutr.get_py(),trk_par_kalman_mutr.get_px());
  phi_acpt_cent = phi_avg;
  phi_acpt_width = 0.001;
}

//______________________________________________________
void
TFvtxMPTrack::calculate_average_half_angle()
{

  if (nNodes_FVTX == 0)
    {
      cout
          << "TFvtxMPTrack::calculate_average_half_angle() - WARNING - no FVTX hits"
          << endl;
      return;
    }

  double first_angle = 0;
  double sum_angle = 0;

  unsigned int n_coord = 0;
  for (n_coord = 0; n_coord < nNodes_FVTX; n_coord++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[n_coord]);
      assert(node);

      double angle = node->half_angle;

      if (n_coord == 0)
        first_angle = angle;
      else
        {
          // avoid roll-over to pi-s

          angle = (angle - first_angle < -M_PI_2) ? angle + M_PI : angle;
          angle = (angle - first_angle > M_PI_2) ? angle - M_PI : angle;

        }

      sum_angle += angle;

    }

  phi_avg = sum_angle / n_coord;
}

//______________________________________________________
void
TFvtxMPTrack::calculate_phi_aceptance(int verbosity, bool include_vtx)
{
  double max_phi_begin = -1000;
  double min_phi_end = 1000;
//
  const double phi_ref = atan2(trk_par_kalman.get_py(),
      trk_par_kalman.get_px());

  if (verbosity >= 2)
    cout << "TFvtxMPTrack::get_phi_aceptance - " << "init with  phi_ref = "
        << phi_ref << endl;
//
  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      double phi_begin = atan2(node->p_strip_begin.Y() - vtx_point.Y(),
          node->p_strip_begin.X() - vtx_point.X());

      double phi_end = atan2(node->p_strip_end.Y() - vtx_point.Y(),
          node->p_strip_end.X() - vtx_point.X());

      if (phi_begin - phi_ref > M_PI)
        phi_begin -= M_PI * 2;
      if (phi_begin - phi_ref < -M_PI)
        phi_begin += M_PI * 2;

      if (phi_end - phi_ref > M_PI)
        phi_end -= M_PI * 2;
      if (phi_end - phi_ref < -M_PI)
        phi_end += M_PI * 2;

      if (phi_begin > phi_end)
        swap(phi_begin, phi_end);
      if (!(phi_begin < phi_end))
        {
          cout << "TFvtxMPTrack::calculate_phi_aceptance - Error - " //
              << "phi_begin = " << phi_begin << ", " //
              << "phi_end = " << phi_end << ", " //
              << " and phi_begin >= phi_end. Force quit!" << endl;
          cout << "vtx_point = " << endl;
          vtx_point.Print();
          cout << "p_strip_begin = " << endl;
          node->p_strip_begin.Print();
          cout << "p_strip_end = " << endl;
          node->p_strip_end.Print();
        }
      assert(phi_begin < phi_end);

      if (phi_begin > max_phi_begin)
        max_phi_begin = phi_begin;
      if (phi_end < min_phi_end)
        min_phi_end = phi_end;
//
      if (verbosity >= 2)
        cout << "TFvtxMPTrack::get_phi_aceptance - FVTX - " << " phi_begin = "
            << phi_begin << " phi_end = " << phi_end << " max_phi_begin = "
            << max_phi_begin << " min_phi_end = " << min_phi_end << endl;

    }

  if (include_vtx && nNodes_VTX)
    for (unsigned int i = 0; i < nNodes_VTX; i++)
      {
        const TFvtxMPNode_VTX * node =
            dynamic_cast<const TFvtxMPNode_VTX *>((*Nodes_VTX)[i]);
        assert(node);

        double phi_begin = atan2(node->p_det.Y() - vtx_point.Y(),
            node->p_det.X() - vtx_point.X());

        if (phi_begin - phi_ref > M_PI)
          phi_begin -= M_PI * 2;
        if (phi_begin - phi_ref < -M_PI)
          phi_begin += M_PI * 2;

        double phi_end = phi_begin;

        if (phi_begin > max_phi_begin)
          max_phi_begin = phi_begin;
        if (phi_end < min_phi_end)
          min_phi_end = phi_end;
//
        if (verbosity >= 2)
          cout << "TFvtxMPTrack::get_phi_aceptance - VTX - " << " phi_begin = "
              << phi_begin << " phi_end = " << phi_end << " max_phi_begin = "
              << max_phi_begin << " min_phi_end = " << min_phi_end << endl;

      }

//
  const double phi_cent = (max_phi_begin + min_phi_end) / 2;

  phi_acpt_cent = fmod(phi_cent + M_PI * 2, M_PI * 2);
  phi_acpt_width = (min_phi_end - max_phi_begin);
}

//______________________________________________________
ClassImp(TFvtxMPTrack::TFvtx1DTrkPar);

//______________________________________________________
double
TFvtxMPTrack::TFvtx1DTrkPar::get_w_abs_fit_cor(const double z,
    const short unsigned int arm, const short unsigned int station,
    const short unsigned int sector) const
{

  static const int maxID = 32;
  static const double FitDiff[maxID + 1] =
    { 0.000570527, // +/- 3.82074e-05 @ ID = 0
        5.51803e-05, // +/- 3.9993e-05 @ ID = 1
        0.000652051, // +/- 3.91615e-05 @ ID = 2
        0.000119814, // +/- 3.50895e-05 @ ID = 3
        7.04547e-05, // +/- 5.0701e-05 @ ID = 4
        -0.000753536, // +/- 5.09923e-05 @ ID = 5
        0.000206395, // +/- 5.17021e-05 @ ID = 6
        -0.000686008, // +/- 4.16331e-05 @ ID = 7
        -0.000649864, // +/- 4.57189e-05 @ ID = 8
        -0.000696014, // +/- 4.26946e-05 @ ID = 9
        -0.000267473, // +/- 4.59793e-05 @ ID = 10
        -0.000656026, // +/- 3.85681e-05 @ ID = 11
        0.000542618, // +/- 3.48016e-05 @ ID = 12
        0.000660535, // +/- 3.82748e-05 @ ID = 13
        0.000537111, // +/- 3.53711e-05 @ ID = 14
        0.000314063, // +/- 3.0832e-05 @ ID = 15
        0.0007821, // +/- 3.9199e-05 @ ID = 16
        0.000307127, // +/- 4.20525e-05 @ ID = 17
        0.000901249, // +/- 3.47661e-05 @ ID = 18
        0.000538588, // +/- 3.43818e-05 @ ID = 19
        -0.000337252, // +/- 5.19687e-05 @ ID = 20
        -0.000949559, // +/- 5.39633e-05 @ ID = 21
        -0.000273674, // +/- 4.44044e-05 @ ID = 22
        -0.00115169, // +/- 4.19273e-05 @ ID = 23
        -0.000605791, // +/- 4.60719e-05 @ ID = 24
        -0.00108891, // +/- 4.61121e-05 @ ID = 25
        -0.000502793, // +/- 4.09786e-05 @ ID = 26
        -0.000806878, // +/- 3.7625e-05 @ ID = 27
        0.000927614, // +/- 3.67905e-05 @ ID = 28
        0.000648031, // +/- 3.96454e-05 @ ID = 29
        0.000885014, // +/- 3.08732e-05 @ ID = 30
        0.000467921, // +/- 3.05803e-05 @ ID = 31
        0 };

  const int ID = (arm * 4 + station) * 4 + sector % 4;

  if (ID < 0 or ID >= maxID)
    {
      cerr
          << "TFvtxMPTrack::TFvtx1DTrkPar::get_w_abs_fit_cor - Error: Wrong sector ID "
          << ID << endl;
      return 0;
    }

  return get_w_abs_fit(z) + FitDiff[ID];
}

//______________________________________________________
// Perform 1-D fit in the w-z plane (rather than r-z plane as in mFvtxStraightLineFit)
void
TFvtxMPTrack::StraightLineFitInWZ()
{
//
  double SZ = 0;
  double SW = 0;
  double SZZ = 0;
  double SZW = 0;
  double SWW = 0;
  const int n = nNodes_FVTX;
//
//  // retrieve coordinates
  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      const double z_det = node->p_det.z();
      const double w_det = node->meas;
      const double w_det_abs = fabs(w_det);

      SZ += z_det;
      SW += w_det_abs;

      SZZ += z_det * z_det;
      SZW += z_det * w_det_abs;
      SWW += w_det_abs * w_det_abs;

    }

  const double tw = (n * SZW - SZ * SW) / (n * SZZ - SZ * SZ);
  const double w = SW / n - tw * SZ / n;

  //output results

  trk_par_wz_fit.set(w, tw);

  z0reco_fvtx = trk_par_wz_fit.get_vertex();

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {

      TFvtxMPNode_FVTX * node =
          dynamic_cast<TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      node->w_fit_1D = trk_par_wz_fit.get_w_abs_fit(node->p_det.z())
          * FVTXOO::SIGN(node->meas);
      node->residu_1D = node->meas - node->w_fit_1D;

    }
}

//______________________________________________________
// Perform 1-D fit in the w-z plane (rather than r-z plane as in mFvtxStraightLineFit)
void
TFvtxMPTrack::StraightLineFitInWZ_Sili()
{
//
  double SZ = 0;
  double SW = 0;
  double SZZ = 0;
  double SZW = 0;
  double SWW = 0;
  const int n = nNodes_FVTX + nNodes_VTX;

  assert(n>=2);

//
  //  // retrieve coordinates
  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      const double z_det = node->p_det.z();
      const double w_det = node->meas;
      const double w_det_abs = fabs(w_det);

      SZ += z_det;
      SW += w_det_abs;

      SZZ += z_det * z_det;
      SZW += z_det * w_det_abs;
      SWW += w_det_abs * w_det_abs;

    }
  //  // retrieve coordinates
  for (unsigned int i = 0; i < nNodes_VTX; i++)
    {
      const TFvtxMPNode_VTX * node =
          dynamic_cast<const TFvtxMPNode_VTX *>((*Nodes_VTX)[i]);
      assert(node);

      const double z_det = node->p_det.z();
      const double w_det = node->meas;
      const double w_det_abs = fabs(w_det);

      SZ += z_det;
      SW += w_det_abs;

      SZZ += z_det * z_det;
      SZW += z_det * w_det_abs;
      SWW += w_det_abs * w_det_abs;

    }

  const double tw = (n * SZW - SZ * SW) / (n * SZZ - SZ * SZ);
  const double w = SW / n - tw * SZ / n;

  //output results

  trk_par_wz_fit.set(w, tw);

  z0reco_fvtx = trk_par_wz_fit.get_vertex();

}

//______________________________________________________
ClassImp(TFvtxMPTrack::TFvtxTrkPar);

//______________________________________________________
double
TFvtxMPTrack::fvtx_sum_residual() const
{
  double sum_residual = 0;

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      sum_residual += node->residu_kalman * node->get_w_sign();
    }

  return sum_residual;
}

//______________________________________________________
double
TFvtxMPTrack::fvtx_min_z() const
{
  double min_z = 40;

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      if (abs(node->p_det.z()) < abs(min_z))
        min_z = node->p_det.z();
    }

  return min_z;
}

//______________________________________________________
double
TFvtxMPTrack::fvtx_max_z() const
{
  double max_z = 0;

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      if (abs(node->p_det.z()) > abs(max_z))
        max_z = node->p_det.z();
    }

  return max_z;
}

//______________________________________________________
const TFvtxMPNode_FVTX *
TFvtxMPTrack::fvtx_node_max_z() const
{
  double max_z = 0;
  const TFvtxMPNode_FVTX * node_max = NULL;

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      if (abs(node->p_det.z()) > abs(max_z))
        {
          max_z = node->p_det.z();
          node_max = node;
        }
    }

  return node_max;
}

//______________________________________________________
bool
TFvtxMPTrack::fvtx_one_hit_per_station(const bool allow_two_side_hits, //
    const bool allow_missing_st0_or_3, //
    bool verbose //
    ) const
{
  unsigned int flag[FVTXOO::MAX_ARM][FVTXOO::MAX_STATION][2];

  for (int arm = 0; arm < FVTXOO::MAX_ARM; arm++)
    for (int station = 0; station < FVTXOO::MAX_STATION; station++)
      {

        flag[arm][station][0] = 0;
        flag[arm][station][1] = 0;

      }

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      if (node->arm >= FVTXOO::MAX_ARM || node->station >= FVTXOO::MAX_STATION)
        {
          cout
              << "TFvtxMPTrack::fvtx_one_hit_per_station - Error - invalid node #"
              << i << endl;
          continue;
        }
      const int side = node->sector % 2;

      flag[node->arm][node->station][side]++;
    }

  bool proper_flag = true;
  for (int arm = 0; arm < FVTXOO::MAX_ARM; arm++)
    {

      proper_flag = true;

      bool forgiven_station_0_or_3 = false;

      for (int station = 0; station < FVTXOO::MAX_STATION; station++)
        {

          const unsigned int side1 = flag[arm][station][0];
          const unsigned int side2 = flag[arm][station][1];
          const bool can_be_forgiven = //
              (station == 0 || station == 3) // station 0 or 3
              && allow_missing_st0_or_3 // allowed
                  && !forgiven_station_0_or_3 // not already forgiven
                  ;

          if (side1 > 1 || side2 > 1)
            {
              if (verbose)
                cout
                    << "TFvtxMPTrack::fvtx_one_hit_per_station - Info - More than one hit per side at arm "
                    << arm << " station " << station << endl;

              proper_flag = false; // -9999 stuff and multi-hit per side

            }

          else if (!allow_two_side_hits && side1 + side2 > 1)
            {
              if (verbose)
                cout << "TFvtxMPTrack::fvtx_one_hit_per_station - Info - Have "
                    << side1 + side2 << " hit at arm " << arm << " station "
                    << station << endl;

              proper_flag = false; // single hit per station
            }

          else if (side1 + side2 == 0)
            {

              if (verbose)
                cout
                    << "TFvtxMPTrack::fvtx_one_hit_per_station - Info - No hit at arm "
                    << arm << " station " << station << endl;

              if (can_be_forgiven)
                {
                  if (verbose)
                    cout
                        << "TFvtxMPTrack::fvtx_one_hit_per_station - Info - Forgive above problem"
                        << endl;
                  forgiven_station_0_or_3 = true;
                }
              else
                proper_flag = false; // single hit per station

            }

          if (!proper_flag)
            break; // this arm is not good, go to next one

        }

      if (proper_flag)
        {
          if (verbose)
            cout
                << "TFvtxMPTrack::fvtx_one_hit_per_station - Info - find one good arm "
                << arm << endl;
          break; // found one good arm, return good
        }
      else
        {
          if (verbose)
            cout
                << "TFvtxMPTrack::fvtx_one_hit_per_station - Info - no good track in arm "
                << arm << endl;
        }
    }

  return proper_flag;
}

//______________________________________________________
bool
TFvtxMPTrack::fvtx_cluster_cut(const int max_hits) const
{
  bool proper_cluster = true;

  for (unsigned int i = 0; i < nNodes_FVTX; i++)
    {
      const TFvtxMPNode_FVTX * node =
          dynamic_cast<const TFvtxMPNode_FVTX *>((*Nodes_FVTX)[i]);
      assert(node);

      proper_cluster = node->hit_size <= max_hits and node->hit_size > 0;
      if (!proper_cluster)
        break;
    }

  return proper_cluster;
}

//______________________________________________________
bool
TFvtxMPTrack::vtx_one_hit_per_layer() const
{

  static const int max_layer_vtx = 2;
  vector<int> count_per_layer(max_layer_vtx, 0);

  for (unsigned int i = 0; i < nNodes_VTX; i++)
    {
      const TFvtxMPNode_VTX * node =
          dynamic_cast<const TFvtxMPNode_VTX *>((*Nodes_VTX)[i]);
      assert(node);

      //! SVX section: 0 - Barrel; 1 - North; 2 - South
      if (node->svxSection != 0)
        {
          cout << "TFvtxMPTrack::vtx_one_hit_per_layer - Error - "
              << "VTX hit matched to FVTX, but not on the barrel layers:"
              << endl;

          node->Print();
          return false;
        }

      if (node->layer >= max_layer_vtx or node->layer < 0)
        {
          cout << "TFvtxMPTrack::vtx_one_hit_per_layer - Error - "
              << "VTX layer number do not make sense to me for hit:" << endl;

          node->Print();
          return false;
        }

      if (node->switch_r_phi == true)
        count_per_layer[node->layer]++;
    }

  bool proper_hit = true;
  for (size_t i = 0; i < count_per_layer.size(); i++)
    {

      if (count_per_layer[i] > 1)
        proper_hit = false;

    }

  return proper_hit;
}

