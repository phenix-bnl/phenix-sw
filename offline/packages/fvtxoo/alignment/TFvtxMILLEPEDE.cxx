/*
 * TFvtxMILLEPEDE.cxx
 *
 *  Created on: Oct 12, 2012
 *      Author: jinhuang
 */

// $$Id: TFvtxMILLEPEDE.cxx,v 1.8 2015/09/09 01:50:17 jinhuang Exp $$
/*!
 * \file TFvtxMILLEPEDE.cxx
 *  \brief   OOP warpper for MILLEPEDE and a generalized least squares fit for residual evaluation. Many code moved from \ref FvtxGlobalAlign by Z.Y. You
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $$Revision: 1.8 $$
 * \date $$Date: 2015/09/09 01:50:17 $$
 */

#include "TFvtxMILLEPEDE.h"

#include "TFvtxMPNode.h"
#include "TFvtxMPTrack.h"
#include "TForwardMPTrack.h"
#include <PHCylPoint.h>
#include <PHGeometry.h>
#include <MuonUtil.h>

#include <TClonesArray.h>
#include <TMath.h>
#include <TVectorD.h>
#include <TMatrixD.h>
#include <TRandom3.h>
#include <TTree.h>

#include <fstream>
#include <iostream>
#include <algorithm>    // std::min_element, std::max_element
#include <cassert>
#include <cmath>
#include <boost/math/special_functions/sign.hpp>

#include <FVTXOO.h>
#include <TMutGeo.h>
#include <TMuiGeo.h>
#include <FvtxGeom.h>

#include <TFvtxAlignmentCorrection.h>
#include <TMutAlignmentCorrection.h>

#include <TFvtxAlign.h>
#include <TMuiAlign.h>
#include <TMutAlign.h>

using namespace std;
using namespace MILLEPEDE;

TFvtxMILLEPEDE::TFvtxMILLEPEDE() :
    _output_misalignment("alignment_corrections.Output.txt"), // txt out of misalignment
    _alignment_tree(0), // empty tree
    _scratch_filename("./scratch"), // scratch file
    _n_std_dev(2), _n_track_para(0), _par_init(false) // millepede control
{
  _flags = ALIGN_W | ITERATE | USE_CONSTRAINTS_CAGE_Z;

  _flags_mu_arm = USE_CONSTRAINTS_MUID_WithInPanel;

  // initialize global parameters and number of tracks/detector
  _dergb.assign(0);
  _derlc.assign(0);
  _par.assign(0);
  _n_tracks.assign(0);
  _n_filled_par.assign(0);

  _detector_status.assign(DET_ENABLE);

  reset_misalignment_variables();

  _sigma_rescaling = 1;
}

TFvtxMILLEPEDE::~TFvtxMILLEPEDE()
{
}

void
TFvtxMILLEPEDE::MILLEPEDE_Fit(TFvtxMPTrack * track, int verbosity)
{
  assert(track);

  assert(get_n_track_para() == 4);

  const unsigned int n_data = track->get_n_nodes();

  for (unsigned int i = 0; i < n_data; i++)
    {

      const TFvtxMPNode * node = track->get_node(i);
      assert(node);

      float w_det = track->use_kalman_fit ? node->residu_kalman : node->meas;
      float sigma = node->sigma * _sigma_rescaling;

      _derlc[0] = node->dwdx; //dwdx;
      _derlc[1] = node->dwdtx; //dwdtx;
      _derlc[2] = node->dwdy; //dwdy;
      _derlc[3] = node->dwdty; //dwdty;

      if (verbosity > 0)
        {
          cout
              << "TFvtxMILLEPEDE::MILLEPEDE_Fit (TFvtxMPTrack) - INFO - print node #"
              << i << endl;
          cout << "                              - derlc: " << _derlc[0] << ","
              << _derlc[1] << "," << _derlc[2] << "," << _derlc[3] << endl;
          cout << "                              - w_det: " << w_det
              << " error: " << sigma << endl;
        }

      if (node->get_node_type() == TFvtxMPNode::FVTX and get_flag(ALIGN_FVTX))
        {
          const TFvtxMPNode_FVTX * node_fvtx =
              dynamic_cast<const TFvtxMPNode_FVTX *>(node);
          assert(node_fvtx);

          int index_half = get_index_half(node_fvtx->arm, node_fvtx->cage,
              node_fvtx->station, node_fvtx->sector, node_fvtx->column);
          int index_station = get_index_station(node_fvtx->arm, node_fvtx->cage,
              node_fvtx->station);

          // derivative wrt w alignment parameter
          _dergb[NPARPLAN * index_half + IDX_HALF_W] = -1.;
          _n_filled_par[NPARPLAN * index_half + IDX_HALF_W]++;

          // derivative wrt z alignment parameter
          _dergb[NPARPLAN * index_half + IDX_HALF_Z] = node_fvtx->get_wrt_z(); //cos_phi*tx+sin_phi*ty;
          _n_filled_par[NPARPLAN * index_half + IDX_HALF_Z]++;

          // derivative wrt phi alignment parameter
          //-sin_phi*x_fit + cos_phi*y_fit;
          _dergb[NPARPLAN * index_half + IDX_HALF_PHI] =
              node_fvtx->get_wrt_phi();
          _n_filled_par[NPARPLAN * index_half + IDX_HALF_PHI]++;

          _dergb[NPARPLAN * index_station + IDX_STA_X] = -1.0 * node->dwdx; // cos_phi
          _dergb[NPARPLAN * index_station + IDX_STA_Y] = -1.0 * node->dwdy; // sin_phi
          _n_filled_par[NPARPLAN * index_station + IDX_STA_X]++;
          _n_filled_par[NPARPLAN * index_station + IDX_STA_Y]++;

          _dergb[NPARPLAN * index_station + IDX_STA_Z] = node_fvtx->get_wrt_z();
          _n_filled_par[NPARPLAN * index_station + IDX_STA_Z]++;

          _dergb[NPARPLAN * index_station + IDX_STA_PHI] =
              node_fvtx->get_wrt_phi();
          _n_filled_par[NPARPLAN * index_station + IDX_STA_PHI]++;

          _dergb[NPARPLAN * index_station + IDX_STA_PSIX] = 0.0; // psix
          _dergb[NPARPLAN * index_station + IDX_STA_PSIY] = 0.0; // psiy
          _n_filled_par[NPARPLAN * index_station + IDX_STA_PSIX]++;
          _n_filled_par[NPARPLAN * index_station + IDX_STA_PSIY]++;

          // increment number of 'tracks' for this detector
          _n_tracks[index_station]++;
          _n_tracks[index_half]++;

          if (verbosity > 0)
            {
              cout << "                              - FVTX Half : "
                  << node_fvtx->arm << ", " << node_fvtx->cage << ", "
                  << node_fvtx->station << ", " << node_fvtx->sector << ", "
                  << node_fvtx->column << endl;
              cout << "                              - index_half : "
                  << index_half << " _dergb: " << _dergb[NPARPLAN * index_half]
                  << "," << _dergb[NPARPLAN * index_half + 1] << ","
                  << _dergb[NPARPLAN * index_half + 2] << endl;
              cout << "                              - index_station : "
                  << index_station << " _dergb: "
                  << _dergb[NPARPLAN * index_station] << ","
                  << _dergb[NPARPLAN * index_station + 1] << ","
                  << _dergb[NPARPLAN * index_station + 2] << ","
                  << _dergb[NPARPLAN * index_station + 3] << ","
                  << _dergb[NPARPLAN * index_station + 4] << ","
                  << _dergb[NPARPLAN * index_station + 5] << endl;
              cout << "                              - Non zero _dergb = "
                  << endl;
              for (int i = 0; i < get_nb_det() * NPARPLAN; i++)
                {
                  if (_dergb[i] != 0)
                    cout << "                              \t[" << i << "] = "
                        << _dergb[i] << endl;
                }
            }
        }

      // book local/global derivatives, measurement, error
      equloc_(&_dergb[0], &_derlc[0], &w_det, &sigma);

    } // iteration of nodes

  fitloc_();

}

void
TFvtxMILLEPEDE::MILLEPEDE_Fit(TForwardMPTrack * track, int verbosity)
{
  assert(track);

  assert(get_n_track_para() == 8);

  const unsigned int n_data = track->get_n_nodes();

  for (unsigned int i = 0; i < n_data; i++)
    {

      const TFvtxMPNode * node = track->get_node(i);
      assert(node);

      float w_det = track->use_kalman_fit ? node->residu_kalman : node->meas;
      float sigma = node->sigma * _sigma_rescaling;

      const bool is_after_absorber = track->is_after_absorber(node->p_det.z());

      bool is_mul_scattering_constraint = node->is_mul_scattering_constraint();

      if (!is_mul_scattering_constraint)
        {
          _derlc[0] = node->dwdx; //dwdx;
          _derlc[1] = node->dwdtx; //dwdtx;
          _derlc[2] = node->dwdy; //dwdy;
          _derlc[3] = node->dwdty; //dwdty;
        }

      if (is_mul_scattering_constraint)
        {
          _derlc[0 + 4] = node->dwdx; //dwdx;
          _derlc[1 + 4] = node->dwdtx; //dwdtx;
          _derlc[2 + 4] = node->dwdy; //dwdy;
          _derlc[3 + 4] = node->dwdty; //dwdty;
        }

      if (verbosity > 0)
        {
          cout
              << "TFvtxMILLEPEDE::MILLEPEDE_Fit(TForwardMPTrack) - INFO - print node #"
              << i;
          cout << endl;
        }

      if (node->get_node_type() == TFvtxMPNode::FVTX)
        {
          if (verbosity > 0)
            {
              cout << " FVTX";
              cout << endl;
            }
          const TFvtxMPNode_FVTX * node_fvtx =
              dynamic_cast<const TFvtxMPNode_FVTX *>(node);
          assert(node_fvtx);

          if (get_flag(ALIGN_FVTX))
            {

              int index_half = get_index_half(node_fvtx->arm, node_fvtx->cage,
                  node_fvtx->station, node_fvtx->sector, node_fvtx->column);
              int index_station = get_index_station(node_fvtx->arm,
                  node_fvtx->cage, node_fvtx->station);

              // derivative wrt w alignment parameter
              _dergb[NPARPLAN * index_half + IDX_HALF_W] = -1.;
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_W]++;

              // derivative wrt z alignment parameter
              _dergb[NPARPLAN * index_half + IDX_HALF_Z] =
                  node_fvtx->get_wrt_z(); //cos_phi*tx+sin_phi*ty;
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_Z]++;

              // derivative wrt phi alignment parameter
              //-sin_phi*x_fit + cos_phi*y_fit;
              _dergb[NPARPLAN * index_half + IDX_HALF_PHI] =
                  node_fvtx->get_wrt_phi();
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_PHI]++;

              _dergb[NPARPLAN * index_station + IDX_STA_X] = -1.0 * node->dwdx; // cos_phi
              _dergb[NPARPLAN * index_station + IDX_STA_Y] = -1.0 * node->dwdy; // sin_phi
              _n_filled_par[NPARPLAN * index_half + IDX_STA_X]++;
              _n_filled_par[NPARPLAN * index_half + IDX_STA_Y]++;

              _dergb[NPARPLAN * index_station + IDX_STA_Z] =
                  node_fvtx->get_wrt_z();
              _n_filled_par[NPARPLAN * index_half + IDX_STA_Z]++;

              _dergb[NPARPLAN * index_station + IDX_STA_PHI] =
                  node_fvtx->get_wrt_phi();
              _n_filled_par[NPARPLAN * index_half + IDX_STA_PHI]++;

              _dergb[NPARPLAN * index_station + IDX_STA_PSIX] = 0.0; // psix
              _dergb[NPARPLAN * index_station + IDX_STA_PSIY] = 0.0; // psiy
              _n_filled_par[NPARPLAN * index_half + IDX_STA_PSIX]++;
              _n_filled_par[NPARPLAN * index_half + IDX_STA_PSIY]++;

              // increment number of 'tracks' for this detector
              _n_tracks[index_station]++;
              _n_tracks[index_half]++;

              if (verbosity > 0)
                {
                  cout << "                       FVTX  - index_half : "
                      << index_half << " _dergb: "
                      << _dergb[NPARPLAN * index_half] << ","
                      << _dergb[NPARPLAN * index_half + 1] << ","
                      << _dergb[NPARPLAN * index_half + 2] << endl;
                  cout << "                              - index_station : "
                      << index_station << " _dergb: "
                      << _dergb[NPARPLAN * index_station] << ","
                      << _dergb[NPARPLAN * index_station + 1] << ","
                      << _dergb[NPARPLAN * index_station + 2] << ","
                      << _dergb[NPARPLAN * index_station + 3] << ","
                      << _dergb[NPARPLAN * index_station + 4] << ","
                      << _dergb[NPARPLAN * index_station + 5] << endl;
                  cout << "                              - Non zero _dergb = "
                      << endl;
                  for (int i = 0; i < get_nb_det() * NPARPLAN; i++)
                    {
                      if (_dergb[i] != 0)
                        cout << "                              \t[" << i
                            << "] = " << _dergb[i] << endl;
                    }
                }
            }
        } //      if (node->get_node_type() == TFvtxMPNode::FVTX)

      else if (node->get_node_type() == TFvtxMPNode::MuTr)
        {
          if (verbosity > 0)
            {
              cout << " MuTr";
              cout << endl;
            }
          const TFvtxMPNode_MuTr * node_mutr =
              dynamic_cast<const TFvtxMPNode_MuTr *>(node);
          assert(node_mutr);

          assert(is_after_absorber);
          _derlc[0 + 4] = node_mutr->dwdx; //dwdx;
          _derlc[1 + 4] = node_mutr->dwdtx_multi_scat; //dwdtx;
          _derlc[2 + 4] = node_mutr->dwdy; //dwdy;
          _derlc[3 + 4] = node_mutr->dwdty_multi_scat; //dwdty;

          if (get_flag(ALIGN_MUTR))
            {

              int index_half(
                  get_index_half_octant(node_mutr->arm, node_mutr->station,
                      node_mutr->octant, node_mutr->half_octant, node_mutr->gap,
                      node_mutr->cathode));

              // derivative wrt w alignment parameter
              _dergb[NPARPLAN * index_half + IDX_HALF_W] = -1.;
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_W]++;

              // derivative wrt z alignment parameter
              _dergb[NPARPLAN * index_half + IDX_HALF_Z] =
                  node_mutr->get_wrt_z(); //cos_phi*tx+sin_phi*ty;
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_Z]++;

              // derivative wrt phi alignment parameter
              //-sin_phi*x_fit + cos_phi*y_fit;
              _dergb[NPARPLAN * index_half + IDX_HALF_PHI] =
                  node_mutr->get_wrt_phi();
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_PHI]++;

              // increment number of 'tracks' for this detector
              _n_tracks[index_half]++;

              if (verbosity > 0)
                {
                  cout << "                        MuTr  - index_half : "
                      << index_half << " _dergb: "
                      << _dergb[NPARPLAN * index_half] << ","
                      << _dergb[NPARPLAN * index_half + 1] << ","
                      << _dergb[NPARPLAN * index_half + 2] << endl;
                  cout << "                              - Non zero _dergb = "
                      << endl;
                  for (int i = 0; i < get_nb_det() * NPARPLAN; i++)
                    {
                      if (_dergb[i] != 0)
                        cout << "                              \t[" << i
                            << "] = " << _dergb[i] << endl;
                    }
                }
            }
        } //      if (node->get_node_type() == TFvtxMPNode::MuTr)

      else if (node->get_node_type() == TFvtxMPNode::MuID)
        {
          if (verbosity > 0)
            {
              cout << " MuID";
              cout << endl;
            }
          const TFvtxMPNode_MuID * node_muid =
              dynamic_cast<const TFvtxMPNode_MuID *>(node);
          assert(node_muid);

          assert(is_after_absorber);
          _derlc[0 + 4] = node_muid->dwdx; //dwdx;
          _derlc[1 + 4] = node_muid->dwdtx_multi_scat; //dwdtx;
          _derlc[2 + 4] = node_muid->dwdy; //dwdy;
          _derlc[3 + 4] = node_muid->dwdty_multi_scat; //dwdty;

          if (get_flag(ALIGN_MUID))
            {
              int index_half(
                  get_index_panel(node_muid->arm, node_muid->plane,
                      node_muid->panel, node_muid->orientation));

              // derivative wrt w alignment parameter
              _dergb[NPARPLAN * index_half + IDX_HALF_W] = -1.;
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_W]++;

              // derivative wrt z alignment parameter
              _dergb[NPARPLAN * index_half + IDX_HALF_Z] =
                  node_muid->get_wrt_z(); //cos_phi*tx+sin_phi*ty;
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_Z]++;

              // derivative wrt phi alignment parameter
              //-sin_phi*x_fit + cos_phi*y_fit;
              _dergb[NPARPLAN * index_half + IDX_HALF_PHI] =
                  node_muid->get_wrt_phi();
              _n_filled_par[NPARPLAN * index_half + IDX_HALF_PHI]++;

              // increment number of 'tracks' for this detector
              _n_tracks[index_half]++;

              if (verbosity > 0)
                {
                  cout << "                       MuID   - index_half : "
                      << index_half << " _dergb: "
                      << _dergb[NPARPLAN * index_half] << ","
                      << _dergb[NPARPLAN * index_half + 1] << ","
                      << _dergb[NPARPLAN * index_half + 2] << endl;
                  cout << "                              - Non zero _dergb = "
                      << endl;
                  for (int i = 0; i < get_nb_det() * NPARPLAN; i++)
                    {
                      if (_dergb[i] != 0)
                        cout << "                              \t[" << i
                            << "] = " << _dergb[i] << endl;
                    }
                }
            }
        } //          if (node->get_node_type() == TFvtxMPNode::MuID and get_flag(ALIGN_MUID))
      else
        {

          if (verbosity > 0)
            {
              cout << " Other Node type";
            }
        }

      //NaN check
        {
          for (int idx = 0; idx < MILLEPEDE::NGLB; idx++)
            {
              if (isnan(_dergb[idx]))
                {
                  cout
                      << "TFvtxMILLEPEDE::MILLEPEDE_Fit - Error - NaN check failed for _dergb. Reset to zero"
                      << endl;
                  node->Print();

                  _dergb[idx] = 0;
                }
            }
          for (int idx = 0; idx < MAX_NPARTRK; idx++)
            {
              if (isnan(_derlc[idx]))
                {
                  cout
                      << "TFvtxMILLEPEDE::MILLEPEDE_Fit - Error - NaN check failed for _derlc. Reset to zero"
                      << endl;
                  node->Print();

                  _derlc[idx] = 0;
                }
            }
          if (isnan(w_det))
            {
              cout
                  << "TFvtxMILLEPEDE::MILLEPEDE_Fit - Error - NaN check failed for w_det. Reset to zero"
                  << endl;
              node->Print();

              w_det = 0;
            }
          if (isnan(sigma))
            {
              cout
                  << "TFvtxMILLEPEDE::MILLEPEDE_Fit - Error - NaN check failed for w_det. Reset to 1"
                  << endl;
              node->Print();

              sigma = 1;
            }
        }

      if (verbosity > 0)
        {
          cout << "                              - derlc: " << _derlc[0] << ","
              << _derlc[1] << "," << _derlc[2] << "," << _derlc[3] << ","
              << _derlc[4] << "," << _derlc[5] << "," << _derlc[6] << ","
              << _derlc[7] << endl;
          cout << "                              - w_det: " << w_det
              << " error: " << sigma << endl;
        }
      // book local/global derivatives, measurement, error
      equloc_(&_dergb[0], &_derlc[0], &w_det, &sigma);

    } // iteration of nodes

  fitloc_();

}

//______________________________________________________
bool
TFvtxMILLEPEDE::init_parameters(bool dump)
{
  cout << "TFvtxMILLEPEDE::init_parameters" << endl;
  if (_par_init)
    {
      cout << "TFvtxMILLEPEDE::init_parameters - WARNING - already initialized"
          << endl;
      return true;
    }

  // define total number of detectors
//  get_nb_det() = _nb_fvtx_station * 2 + _nb_fvtx_wedge; // one (wedge) det has 3(NPARPLAN) parameters, station * 2 because it has 6 parameters
  cout << "TFvtxMILLEPEDE::init_parameters - get_nb_det()=" << get_nb_det()
      << endl;
  assert( get_nb_det() <= NPLAN);

  check_detector_index();

  // initialize global parameters and number of tracks/detector
  _dergb.assign(0);
  _derlc.assign(0);
  _par.assign(0);
  _n_tracks.assign(0);

  /*
   initialize millepede
   NPARPLAN is defined in millepede.h
   */
//  _n_std_dev = n_std_dev;
  // special mode to use 1-D local fits to reduce correlations
  const int n_track_para = get_n_track_para();
  assert(n_track_para<=MAX_NPARTRK);

  cout << "TFvtxMILLEPEDE::init_parameters - local parameter size = "
      << n_track_para << endl;

  C_INITGL( get_nb_det()*NPARPLAN, n_track_para, _n_std_dev, int(dump)-1);
//  cout <<"-----------------------------------------------------------------------------"<<endl;
//  cout <<"TFvtxMILLEPEDE::init_parameters - WARNING : Special Try 1-D fittings"<<endl;
//  cout <<"-----------------------------------------------------------------------------"<<endl;
//  C_INITGL( get_nb_det()*NPARPLAN, 2, _n_std_dev, int(dump)-1);

  _par_init = true;
  return true;
}

//______________________________________________________
bool
TFvtxMILLEPEDE::init_minimize(void)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::init_minimize");

  int n_fix = 0;

  if (get_flag(ALIGN_FVTX))
    {
      // Check which parameters to align
      if (!(get_flag(ALIGN_W) || get_flag(ALIGN_Z) || get_flag(ALIGN_PHI)))
        {
          cout
              << "TFvtxMILLEPEDE::init_minimize - ERROR: nothing to minimize.\n";
          return false;
        }
      else
        {
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_FVTX - _align_w: "
              << ((get_flag(ALIGN_W)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_FVTX - _align_z: "
              << ((get_flag(ALIGN_Z)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_FVTX - _align_phi: "
              << ((get_flag(ALIGN_PHI)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_FVTX - _align_psix: "
              << ((get_flag(ALIGN_PSIX)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_FVTX - _align_psiy: "
              << ((get_flag(ALIGN_PSIY)) ? "true" : "false") << endl;
        }

      // fix all parameters if alignment not required
      for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
        for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            {
              const int index(NPARPLAN * get_index_station(arm, cage, station));

              if (!get_flag(ALIGN_W))
                {
                  n_fix++;
                  C_PARSIG(index + 1 + IDX_STA_X, 0.0);
                  n_fix++;
                  C_PARSIG(index + 1 + IDX_STA_Y, 0.0);
                }
              if (!get_flag(ALIGN_Z))
                {
                  n_fix++;
                  C_PARSIG(index + 1 + IDX_STA_Z, 0.0);
                }
              if (!get_flag(ALIGN_PHI))
                {
                  n_fix++;
                  C_PARSIG(index + 1 + IDX_STA_PHI, 0.0);
                }
              if (!get_flag(ALIGN_PSIX))
                {
                  n_fix++;
                  C_PARSIG(index + 1 + IDX_STA_PSIX, 0.0);
                }
              if (!get_flag(ALIGN_PSIY))
                {
                  n_fix++;
                  C_PARSIG(index + 1 + IDX_STA_PSIY, 0.0);
                }
            }

      for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
        for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            for (int sector = 0; sector < FVTXGEOM::NumberOfSectors; sector++)
              for (int half = 0; half < FVTXGEOM::NumberOfColumns; half++)
                {
                  int index(
                      NPARPLAN
                          * get_index_half(arm, cage, station, sector, half));
                  if (!get_flag(ALIGN_W))
                    {
                      n_fix++;
                      C_PARSIG( index + 1 + IDX_HALF_W, 0.0);
                    }
                  if (!get_flag(ALIGN_Z))
                    {
                      n_fix++;
                      C_PARSIG( index + 1 + IDX_HALF_Z, 0.0);
                    }
                  if (!get_flag(ALIGN_PHI))
                    {
                      n_fix++;
                      C_PARSIG( index + 1 + IDX_HALF_PHI, 0.0);
                    }
                }

    } //  if (get_flag(ALIGN_FVTX))

  if (get_flag(ALIGN_MUTR))
    {

      // Check which parameters to align
      if (!(get_flag(ALIGN_W_MU_ARM) || get_flag(ALIGN_Z_MU_ARM)
          || get_flag(ALIGN_PHI_MU_ARM)))
        {
          cout
              << "TFvtxMILLEPEDE::init_minimize - ERROR: nothing to minimize.\n";
          exit(1);
          return false;
        }
      else
        {
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_MUTR - _align_w: "
              << ((get_flag(ALIGN_W_MU_ARM)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_MUTR - _align_z: "
              << ((get_flag(ALIGN_Z_MU_ARM)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_MUTR - _align_phi: "
              << ((get_flag(ALIGN_PHI_MU_ARM)) ? "true" : "false") << endl;
        }

      // fix all parameters if alignment not required
      for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
        for (int station = 0; station < MUTOO::NumberOfStations; station++)
          for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
            for (int octant = 0; octant < MUTOO::NumberOfOctants; octant++)
              for (int half_octant = 0;
                  half_octant < MUTOO::NumberOfHalfOctants; half_octant++)
                for (int cath = 0; cath < MUTOO::NumberOfCathodePlanes; cath++)
                  {
                    int index = NPARPLAN
                        * get_index_half_octant(arm, station, octant,
                            half_octant, gap, cath);
                    if (!get_flag(ALIGN_W_MU_ARM))
                      {
                        n_fix++;
                        C_PARSIG(index + 1 + IDX_HALF_W, 0.0);
                      }
                    if (!get_flag(ALIGN_Z_MU_ARM))
                      {
                        n_fix++;
                        C_PARSIG(index + 1 + IDX_HALF_Z, 0.0);
                      }
                    if (!get_flag(ALIGN_PHI_MU_ARM))
                      {
                        n_fix++;
                        C_PARSIG(index + 1 + IDX_HALF_PHI, 0.0);
                      }
                  }
    }

  if (get_flag(ALIGN_MUID))
    {

      // Check which parameters to align
      if (!(get_flag(ALIGN_W_MU_ARM) || get_flag(ALIGN_Z_MU_ARM)
          || get_flag(ALIGN_PHI_MU_ARM)))
        {
          cout
              << "TFvtxMILLEPEDE::init_minimize - ERROR: nothing to minimize.\n";
          return false;
        }
      else
        {
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_MUID - _align_w: "
              << ((get_flag(ALIGN_W_MU_ARM)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_MUID - _align_z: "
              << ((get_flag(ALIGN_Z_MU_ARM)) ? "true" : "false") << endl;
          cout << "TFvtxMILLEPEDE::init_minimize::ALIGN_MUID - _align_phi: "
              << ((get_flag(ALIGN_PHI_MU_ARM)) ? "true" : "false") << endl;
        }

      // fix all parameters if alignment not required
      for (int arm = 0; arm < MUIOO::MAX_ARM; arm++)
        for (int plane = 0; plane < MUIOO::MAX_PLANE; plane++)
          for (int panel = 0; panel < MUIOO::MAX_PANEL; panel++)
            for (int orientation = 0; orientation < MUIOO::MAX_ORIENTATION;
                orientation++)
              {
                int index = NPARPLAN
                    * get_index_panel(arm, plane, panel, orientation);
                if (!get_flag(ALIGN_W_MU_ARM))
                  {
                    n_fix++;
                    C_PARSIG(index + 1 + IDX_HALF_W, 0.0);
                  }
                if (!get_flag(ALIGN_Z_MU_ARM))
                  {
                    n_fix++;
                    C_PARSIG(index + 1 + IDX_HALF_Z, 0.0);
                  }
                if (!get_flag(ALIGN_PHI_MU_ARM))
                  {
                    n_fix++;
                    C_PARSIG(index + 1 + IDX_HALF_PHI, 0.0);
                  }
              }
    }

  cout
      << "TFvtxMILLEPEDE::init_minimize - Total fix detector at initial stage = "
      << n_fix << endl;

  // initialize global/local matres to 0
  zerloc_(&_dergb[0], &_derlc[0]);

  // tell minimization to iterate
  if (get_flag(ITERATE))
    C_INITUN(11, 10000., const_cast<char*>( _scratch_filename ));

  // force two half sector to move together
  // C_CONSTF(2,0.0);

  return true;
}

//______________________________________________________
void
TFvtxMILLEPEDE::end_minimize()
{

  MUTOO::TRACE("TFvtxMILLEPEDE::end_minimize, fitglo");
  fitglo_(&_par[0]);
  C_PRTGLO(20);
  MUTOO::TRACE("TFvtxMILLEPEDE::end_minimize, done");

}

//______________________________________________________
void
TFvtxMILLEPEDE::check_detector_index(void) const
{

  MUTOO::PRINT(cout, "TFvtxMILLEPEDE::check_detector_index");

  set<int> index_set;
  set<int> index_set_subsys;

  if (get_flag(ALIGN_FVTX))
    {
      index_set_subsys.clear();
      // fvtx tracker station indices
//  set<int> station_index_set;
      for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
        for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            {
              int index(get_index_station(arm, cage, station));
              if (index_set.find(index) != index_set.end())
                cout << "fvtx [" << arm << "," << cage << "," << station << "] "
                    << "duplicated (" << index << ")" << endl;
              index_set.insert(index);
              index_set.insert(index + 1); // FVTX Station has doubled sets of parameters
              index_set_subsys.insert(index);
              index_set_subsys.insert(index + 1); // FVTX Station has doubled sets of parameters
            }

      cout << "TFvtxMILLEPEDE::check_detector_index - FVTX station index range "
          << *min_element(index_set_subsys.begin(), index_set_subsys.end())
          << " to "
          //
          << *max_element(index_set_subsys.begin(), index_set_subsys.end())
          << endl;

      index_set_subsys.clear();

      // fvtx tracker half wedge indices
//  set<int> half_index_set;
      for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
        for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            for (int sector = 0; sector < FVTXGEOM::NumberOfSectors; sector++)
              for (int half = 0; half < FVTXGEOM::NumberOfColumns; half++)
                {
                  int index(get_index_half(arm, cage, station, sector, half));
                  if (index_set.find(index) != index_set.end())
                    cout << "fvtx [" << arm << "," << cage << "," << station
                        << "," << sector << "," << half << "] "
                        << "duplicated (" << index << ")" << endl;
                  index_set.insert(index);
                  index_set_subsys.insert(index);
                }
      cout << "TFvtxMILLEPEDE::check_detector_index - FVTX wedge index range "
          << *min_element(index_set_subsys.begin(), index_set_subsys.end())
          << " to "
          //
          << *max_element(index_set_subsys.begin(), index_set_subsys.end())
          << endl;
    } //  if (get_flag(ALIGN_FVTX))

  // muon tracker half octant indices
  if (get_flag(ALIGN_MUTR))
    {
      index_set_subsys.clear();

      cout << "mutr_half_octant_index" << endl;
      for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
        for (int station = 0; station < MUTOO::NumberOfStations; station++)
          for (int octant = 0; octant < MUTOO::NumberOfOctants; octant++)
            for (int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants;
                half_octant++)
              for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
                for (int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes;
                    cathode++)
                  {
                    int index(
                        get_index_half_octant(arm, station, octant, half_octant,
                            gap, cathode));
                    if (index_set.find(index) != index_set.end())
                      cout << "mutr [" << arm << "," << station << "," << octant
                          << "," << half_octant << "," << gap << "," << cathode
                          << "] " << "duplicated (" << index << ")" << endl;
                    index_set.insert(index);
                    index_set_subsys.insert(index);
                  }
      cout << "TFvtxMILLEPEDE::check_detector_index - MuTr index range "
          << *min_element(index_set_subsys.begin(), index_set_subsys.end())
          << " to "
          //
          << *max_element(index_set_subsys.begin(), index_set_subsys.end())
          << endl;
    }

  // muid panel indices
  if (get_flag(ALIGN_MUID))
    {
      index_set_subsys.clear();
      cout << "muid_panel_index" << endl;
      for (int arm = 0; arm < MUIOO::MAX_ARM; arm++)
        for (int plane = 0; plane < MUIOO::MAX_PLANE; plane++)
          for (int panel = 0; panel < MUIOO::MAX_PANEL; panel++)
            for (int orientation = 0; orientation < MUIOO::MAX_ORIENTATION;
                orientation++)
              {
                int index(get_index_panel(arm, plane, panel, orientation));
                if (index_set.find(index) != index_set.end())
                  cout << "muid [" << arm << "," << plane << "," << panel << ","
                      << orientation << "] " << "duplicated (" << index << ")"
                      << endl;
                index_set.insert(index);
                index_set_subsys.insert(index);
              }
      cout << "TFvtxMILLEPEDE::check_detector_index - MuID index range "
          << *min_element(index_set_subsys.begin(), index_set_subsys.end())
          << " to "
          //
          << *max_element(index_set_subsys.begin(), index_set_subsys.end())
          << endl;
    }

  MUTOO::PRINT(cout, "**");

}

//________________________________________
void
TFvtxMILLEPEDE::export_misalignment_to_text()
{

  //  cout <<"TFvtxMILLEPEDE::export_misalignment_to_text - this function is obsolete. Best use a macro to print alignment output"<<endl;
  cout << "TFvtxMILLEPEDE::export_misalignment_to_text - output to files *_"
      << _output_misalignment << endl;

  try
    {
      if (get_flag(ALIGN_FVTX))
        {

          TFvtxAlignmentCorrection alignmentCorrections;

          // set flags
          alignmentCorrections.set_z_alignment_enabled(get_flag(ALIGN_Z));

          alignmentCorrections.set_fvtx_alignment_enabled(
              get_flag(ALIGN_FVTX_STATION) || get_flag(ALIGN_FVTX_WEDGE));

          // read from tree, write output to text file
          alignmentCorrections.initialize(_alignment_tree,
              (string("fvtx_") + string(_output_misalignment)).c_str());
        }

      if (get_flag(ALIGN_MU_ARM))
        {

          TMutAlignmentCorrection alignmentCorrections;

          // set flags
          alignmentCorrections.set_z_alignment_enabled(
              get_flag(ALIGN_Z_MU_ARM));
          alignmentCorrections.set_mutr_alignment_enabled(get_flag(ALIGN_MUTR));
          alignmentCorrections.set_muid_alignment_enabled(get_flag(ALIGN_MUID));

          // read from tree, write output to text file
          alignmentCorrections.initialize(_alignment_tree,
              (string("mu_arm_") + string(_output_misalignment)).c_str());

        }
    }

  catch (exception &e)
    {

      cout << "TFvtxMILLEPEDE::export_misalignment_to_text() - Error - "
          << e.what() << endl;

//    throw e;
    }

  cout << "TFvtxMILLEPEDE::export_misalignment_to_text - Done!" << endl;
}

void
TFvtxMILLEPEDE::reset_misalignment_variables(void)
{

  //! track arm
  /*int*/_arm = -9999;

  _octant = -9999;

  //! track cage
  /*int*/_cage = -9999;

  //! track station
  /*int*/_station = -9999;

  //! track sector
  /*int*/_sector = -9999;

  _gap = -9999;

  //! millepede x correction
  /*double*/_delta_x_millepede = 0;

  //! millepede y correction
  /*double*/_delta_y_millepede = 0;

  // desalignment tree parameters
  //! millepede z correction
  /*double*/_delta_z_millepede = 0;

  //! millepede w correction
  /*double*/_delta_w_millepede = 0;

  //! millepede phi correction
  /*double*/_delta_phi_millepede = 0;

  //! millepede psix correction
  /*double*/_delta_psix_millepede = 0;

  //! millepede psiy correction
  /*double*/_delta_psiy_millepede = 0;

  //! error on millepede x correction
  /*double*/_error_x = -9999;

  //! error on millepede y correction
  /*double*/_error_y = -9999;

  //! error on millepede z correction
  /*double*/_error_z = -9999;

  //! error on millepede w correction
  /*double*/_error_w = -9999;

  //! error on millepede phi correction
  /*double*/_error_phi = -9999;

  //! error on millepede psix correction
  /*double*/_error_psix = -9999;

  //! error on millepede psiy correction
  /*double*/_error_psiy = -9999;

  //! input x misalignment
  /*double*/_delta_x = 0;

  //! input y misalignment
  /*double*/_delta_y = 0;

  //! input z misalignment
  /*double*/_delta_z = 0;

  //! input w misalignment
  /*double*/_delta_w = 0;

  //! input phi misalignment
  /*double*/_delta_phi = 0;

  //! input psix misalignment
  /*double*/_delta_psix = 0;

  //! input psiy misalignment
  /*double*/_delta_psiy = 0;

  //! column location
  /*int*/_half = -9999;

  _cathode = -9999;

  //! strip location
  /*int*/_strip = -9999;

  //! set to 1 if the parameter corresponds to a fvtx detector
  /*int*/_detector_id = kUNKNOWN_DETECTOR;

  _plane = -9999;
  _panel = -9999;
  _orientation = -9999;

  //! detector index
  /*int*/_detector_index = -9999;

  //! number of tracks in the detector
  /*int*/_nb_tracks = 0;

  //! strip angle
  /*double*/_angle = -9999;
}

//________________________________________
// from TFvtxMILLEPEDE::initialize_alignment_tree
void
TFvtxMILLEPEDE::build_misalignment_tree(void)
{

  // create tree (for now the tree is empty)

  _alignment_tree = new TTree("misalignment", " misalignments parameters");
  _alignment_tree->Branch("delta_x", &_delta_x, "delta_x/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_y", &_delta_y, "delta_y/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_z", &_delta_z, "delta_z/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_w", &_delta_w, "delta_w/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_phi", &_delta_phi, "delta_phi/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_psix", &_delta_psix, "delta_psix/D",
      BUFFER_SIZE);
  _alignment_tree->Branch("delta_psiy", &_delta_psiy, "delta_psiy/D",
      BUFFER_SIZE);
  _alignment_tree->Branch("delta_w_millepede", &_delta_w_millepede,
      "delta_w_millepede/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_z_millepede", &_delta_z_millepede,
      "delta_z_millepede/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_phi_millepede", &_delta_phi_millepede,
      "delta_phi_millepede/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_psix_millepede", &_delta_psix_millepede,
      "delta_psix_millepede/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_psiy_millepede", &_delta_psiy_millepede,
      "delta_psiy_millepede/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_x_millepede", &_delta_x_millepede,
      "delta_x_millepede/D", BUFFER_SIZE);
  _alignment_tree->Branch("delta_y_millepede", &_delta_y_millepede,
      "delta_y_millepede/D", BUFFER_SIZE);

  _alignment_tree->Branch("arm", &_arm, "arm/I", BUFFER_SIZE);
  _alignment_tree->Branch("cage", &_cage, "cage/I", BUFFER_SIZE);
  _alignment_tree->Branch("station", &_station, "station/I", BUFFER_SIZE);
  _alignment_tree->Branch("sector", &_sector, "sector/I", BUFFER_SIZE);
  _alignment_tree->Branch("half", &_half, "half/I", BUFFER_SIZE);
  _alignment_tree->Branch("gap", &_gap, "gap/I", BUFFER_SIZE);
  _alignment_tree->Branch("octant", &_octant, "octant/I", BUFFER_SIZE);
  _alignment_tree->Branch("cathode", &_cathode, "cathode/I", BUFFER_SIZE);
  _alignment_tree->Branch("plane", &_plane, "plane/I", BUFFER_SIZE);
  _alignment_tree->Branch("panel", &_panel, "panel/I", BUFFER_SIZE);
  _alignment_tree->Branch("orientation", &_orientation, "orientation/I",
      BUFFER_SIZE);

  _alignment_tree->Branch("error_x", &_error_x, "error_x/D", BUFFER_SIZE);
  _alignment_tree->Branch("error_y", &_error_y, "error_y/D", BUFFER_SIZE);
  _alignment_tree->Branch("error_z", &_error_z, "error_z/D", BUFFER_SIZE);
  _alignment_tree->Branch("error_w", &_error_w, "error_w/D", BUFFER_SIZE);
  _alignment_tree->Branch("error_phi", &_error_phi, "error_phi/D", BUFFER_SIZE);
  _alignment_tree->Branch("error_psix", &_error_psix, "error_psix/D",
      BUFFER_SIZE);
  _alignment_tree->Branch("error_psiy", &_error_psiy, "error_psiy/D",
      BUFFER_SIZE);
  _alignment_tree->Branch("nb_tracks", &_nb_tracks, "nb_tracks/I", BUFFER_SIZE);
  _alignment_tree->Branch("angle", &_angle, "angle/D", BUFFER_SIZE);

  _alignment_tree->Branch("detector_id", &_detector_id, "detector_id/I",
      BUFFER_SIZE);
  _alignment_tree->Branch("detector_index", &_detector_index,
      "detector_index/I", BUFFER_SIZE);

  _alignment_tree->SetAutoSave(AUTO_SAVE);

}

//_________________________________
string
TFvtxMILLEPEDE::format(const char* format, ...)
{

  char buffer[2048];
  va_list p;
  va_start(p, format);
  vsprintf(buffer, format, p);
  va_end(p);

  return string(buffer);
}

//________________________________________
void
TFvtxMILLEPEDE::export_fvtx_misalignment_to_tree(ostream &out_cp)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::print_fvtx_parameters_to_stream");

  assert(get_flag(ALIGN_FVTX));

  reset_misalignment_variables();

  boost::array<double, 2> angle;
  boost::array<double, 2> delta_w;
  boost::array<double, 2> error_w2;

  // dump fvtx alignment
  MUTOO::PRINT(out_cp, "FVTX");

  cout
      << "TFvtxMILLEPEDE::export_fvtx_misalignment_to_tree - INFO - process wedges..."
      << endl;

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
        for (int sector = 0; sector < FVTXGEOM::NumberOfSectors; sector++)
          {
            for (int half = 0; half < FVTXGEOM::NumberOfColumns; half++)
              {

                int index = get_index_half(arm, cage, station, sector, half);

                // get misalignment parameters
                TFvtxAlign::WedgeParameters _wedge_para =
                    TFvtxAlign::get_wedge_parameters(arm, cage, station,
                        sector);

                _delta_x = _wedge_para._delta_x;
                _delta_y = _wedge_para._delta_y;
                _delta_z = _wedge_para._delta_z;
                _delta_phi = _wedge_para._delta_phi;
                _angle = angle[half] = get_half_angle(arm, cage, station,
                    sector, half, 0);
                _delta_w = -sin(angle[half]) * _delta_x
                    + cos(angle[half]) * _delta_y;
                _delta_psix = 0.0;
                _delta_psiy = 0.0;
                out_cp << " arm = " << arm << " ; cage = " << cage
                    << " ; station = " << station << " ; sector = " << sector
                    << " ; half sector = " << half << " - ";

                // location parameters
                _arm = arm;
                _cage = cage;
                _station = station;
                _sector = sector;
                _half = half;
                _detector_id = kFVTX_WEDGE;

                // Dump W
                int j = NPARPLAN * index + IDX_HALF_W + 1;
                double err = errpar_(&j);
                if (err == 0 && get_flag(ALIGN_W))
                  err = -999.9;

                out_cp << "W: "
                    << format("%10.4f %10.4f ",
                        _par[NPARPLAN * index + IDX_HALF_W], err);
                _delta_w_millepede = delta_w[half] = _par[NPARPLAN * index
                    + IDX_HALF_W];
                _error_w = err;
                error_w2[half] = _error_w;

                // Dump Z
                j = NPARPLAN * index + IDX_HALF_Z + 1;
                err = errpar_(&j);
                if (err == 0 && get_flag(ALIGN_Z))
                  err = -999.9;
                out_cp << " Z :"
                    << format("%10.4f %10.4f ",
                        _par[NPARPLAN * index + IDX_HALF_Z], err);
                _delta_z_millepede = _par[NPARPLAN * index + IDX_HALF_Z];
                _error_z = err;

                // Dump Phi
                j = NPARPLAN * index + IDX_HALF_PHI + 1;
                err = errpar_(&j);
                if (err == 0 && get_flag(ALIGN_PHI))
                  err = -999.9;
                out_cp << " Phi: "
                    << format("%10.4f %10.4f  ",
                        _par[NPARPLAN * index + IDX_HALF_PHI], err);
                _delta_phi_millepede = _par[NPARPLAN * index + IDX_HALF_PHI];
                _error_phi = err;

                // strip angle
                out_cp << " angle: " << _angle;

                // Dump number of tracks
                out_cp << " n_tracks[" << index << "] : " << _n_tracks[index];

                out_cp << endl;

                _nb_tracks = _n_tracks[index];
                if (half == 1)
                  {
                    out_cp << " angle1=" << angle[1] << " angle0=" << angle[0]
                        << endl;
                    if (abs(angle[0] - angle[1]) < 0.00001)
                      {

                        // use average delta w between the two half sector instead !
                        _delta_x_millepede = -(delta_w[0] * sin(angle[0])
                            + delta_w[1] * sin(angle[1])) / 2;
                        _delta_y_millepede = (delta_w[0] * cos(angle[0])
                            + delta_w[1] * cos(angle[1])) / 2;
                        _error_x = sqrt(
                            pow(sin(angle[0]) / 2, 2) * pow(error_w2[0], 2)
                                + pow(sin(angle[1]) / 2, 2)
                                    * pow(error_w2[1], 2));
                        _error_y = sqrt(
                            pow(cos(angle[0]) / 2, 2) * pow(error_w2[0], 2)
                                + pow(cos(angle[1]) / 2, 2)
                                    * pow(error_w2[1], 2));

                      }
                    else
                      {

                        out_cp << "sin ( )" << sin(angle[1] - angle[0]) << endl;
                        _delta_x_millepede = (cos(angle[1]) * delta_w[0]
                            - cos(angle[0]) * delta_w[1])
                            / sin(angle[1] - angle[0]);
                        _delta_y_millepede = (sin(angle[1]) * delta_w[0]
                            - sin(angle[0]) * delta_w[1])
                            / sin(angle[1] - angle[0]);
                        _error_x = sqrt(
                            pow(cos(angle[1]) / sin(angle[1] - angle[0]), 2)
                                * pow(error_w2[0], 2)
                                + pow(cos(angle[0]) / sin(angle[1] - angle[0]),
                                    2) * pow(error_w2[1], 2));
                        _error_y = sqrt(
                            pow(sin(angle[1]) / sin(angle[1] - angle[0]), 2)
                                * pow(error_w2[0], 2)
                                + pow(sin(angle[0]) / sin(angle[1] - angle[0]),
                                    2) * pow(error_w2[1], 2));

                      }

                  }
                else
                  {
                    _delta_x_millepede = -9999;
                    _delta_y_millepede = -9999;
                    _error_x = -9999;
                    _error_y = -9999;
                  }

                // Fill misalignment tree
                _alignment_tree->Fill();
              }
          }

  cout
      << "TFvtxMILLEPEDE::export_fvtx_misalignment_to_tree - INFO - process stations..."
      << endl;

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
        {
          int index = get_index_station(arm, cage, station);

          // get misalignment parameters
          TFvtxAlign::StationParameters _station_para =
              TFvtxAlign::get_station_parameters(arm, cage, station);

          _delta_x = _station_para._delta_x;
          _delta_y = _station_para._delta_y;
          _delta_z = _station_para._delta_z;
          _delta_phi = _station_para._delta_phi;
          _delta_w = 0.0;
          _delta_psix = _station_para._delta_psix;
          _delta_psiy = _station_para._delta_psiy;
          out_cp << " arm = " << arm << " ; cage = " << cage << " ; station = "
              << station << " - ";

          // location parameters
          _arm = arm;
          _cage = cage;
          _station = station;
          _sector = -9999;
          _half = -9999;
//          _is_fvtx_wedge = 0;
          _detector_id = kFVTX_STATION;
          _angle = -9999;

//           Dump W
          int j = -9999;
          double err = -9999;
//          if (err == 0 && get_flag(ALIGN_W))
//            err = -999.9;

          _delta_w_millepede = -9999;
          _error_w = -9999;

          j = NPARPLAN * index + IDX_STA_X + 1;
          err = errpar_(&j);
          if (err == 0 && get_flag(ALIGN_W))
            err = -999.9;
          out_cp << "X: "
              << format("%10.4f %10.4f ", _par[NPARPLAN * index + IDX_STA_X],
                  err);
          _delta_x_millepede = _par[NPARPLAN * index + IDX_STA_X];
          _error_x = err;

          j = NPARPLAN * index + IDX_STA_Y + 1;
          err = errpar_(&j);
          if (err == 0 && get_flag(ALIGN_W))
            err = -999.9;
          out_cp << "Y: "
              << format("%10.4f %10.4f ", _par[NPARPLAN * index + IDX_STA_Y],
                  err);
          _delta_y_millepede = _par[NPARPLAN * index + IDX_STA_Y];
          _error_y = err;

          // Dump Z
          j = NPARPLAN * index + IDX_STA_Z + 1;
          err = errpar_(&j);
          if (err == 0 && get_flag(ALIGN_Z))
            err = -999.9;
          out_cp << " Z :"
              << format("%10.4f %10.4f ", _par[NPARPLAN * index + IDX_STA_Z],
                  err);
          _delta_z_millepede = _par[NPARPLAN * index + IDX_STA_Z];
          _error_z = err;

          // Dump Phi
          j = NPARPLAN * index + IDX_STA_PHI + 1;
          err = errpar_(&j);
          if (err == 0 && get_flag(ALIGN_PHI))
            err = -999.9;
          out_cp << " Phi: "
              << format("%10.4f %10.4f  ", _par[NPARPLAN * index + IDX_STA_PHI],
                  err);
          _delta_phi_millepede = _par[NPARPLAN * index + IDX_STA_PHI];
          _error_phi = err;

          // Dump Psix
          j = NPARPLAN * index + IDX_STA_PSIX + 1;
          err = errpar_(&j);
          if (err == 0 && get_flag(ALIGN_PSIX))
            err = -999.9;
          out_cp << " Psix: "
              << format("%10.4f %10.4f  ",
                  _par[NPARPLAN * index + IDX_STA_PSIX], err);
          _delta_psix_millepede = _par[NPARPLAN * index + IDX_STA_PSIX];
          _error_psix = err;

          // Dump Psiy
          j = NPARPLAN * index + IDX_STA_PSIY + 1;
          err = errpar_(&j);
          if (err == 0 && get_flag(ALIGN_PSIY))
            err = -999.9;
          out_cp << " Psiy: "
              << format("%10.4f %10.4f  ",
                  _par[NPARPLAN * index + IDX_STA_PSIY], err);
          _delta_psiy_millepede = _par[NPARPLAN * index + IDX_STA_PSIY];
          _error_psiy = err;

          // strip angle
          out_cp << " angle: " << _angle;

          // Dump number of tracks
          out_cp << " n_tracks[" << index << "] : " << _n_tracks[index];

          out_cp << endl;

          _nb_tracks = _n_tracks[index];

          // Fill misalignment tree
          _alignment_tree->Fill();
        }

  cout << "TFvtxMILLEPEDE::export_fvtx_misalignment_to_tree - INFO - Done!"
      << endl;
  MUTOO::PRINT(out_cp, "**");

  return;

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_fvtx_station(int arm, int cage, int station,
    unsigned int flags)
{

  cout << "TFvtxMILLEPEDE::fix_fvtx_station - [" << arm << "," << cage << ","
      << station << "] flags:" << flags << endl;

  assert(get_flag(ALIGN_FVTX));
  FvtxStationId id(arm, cage, station);
  FvtxStationId::Map::iterator iter(_fixed_fvtx_stations.find(id));
  if (iter == _fixed_fvtx_stations.end())
    {
      _fixed_fvtx_stations.insert(make_pair(id, flags));
    }
  else
    {
      iter->second |= flags;
    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_fvtx_wedge(int arm, int cage, int station, int sector,
    unsigned int flags)
{

  cout << "TFvtxMILLEPEDE::fix_fvtx_wedge - [" << arm << "," << cage << ","
      << station << "," << sector << "] flags:" << flags << endl;

  assert(get_flag(ALIGN_FVTX));
  FvtxWedgeId id(arm, cage, station, sector);
  FvtxWedgeId::Map::iterator iter(_fixed_fvtx_wedges.find(id));
  if (iter == _fixed_fvtx_wedges.end())
    {
      _fixed_fvtx_wedges.insert(make_pair(id, flags));
    }
  else
    {
      iter->second |= flags;
    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::register_fixed_detectors(void)
{

  MUTOO::TRACE("TFvtxMILLEPEDE::register_fixed_detectors");

  if (get_flag(ALIGN_FVTX))
    {
      cout << "TFvtxMILLEPEDE::register_fixed_detectors - fvtx_station : "
          << _fixed_fvtx_stations.size() << endl;

      // fix fvtx stations
      for (FvtxStationId::Map::iterator iter = _fixed_fvtx_stations.begin();
          iter != _fixed_fvtx_stations.end(); ++iter)
        {

          fix_parameter_fvtx_station(iter->first._arm, iter->first._cage,
              iter->first._station, iter->second);
        }

      cout << "TFvtxMILLEPEDE::register_fixed_detectors - fvtx_wedge : "
          << _fixed_fvtx_wedges.size() << endl;

      // fix fvtx wedges
      for (FvtxWedgeId::Map::iterator iter = _fixed_fvtx_wedges.begin();
          iter != _fixed_fvtx_wedges.end(); ++iter)
        {

          fix_parameter_fvtx_wedge(iter->first._arm, iter->first._cage,
              iter->first._station, iter->first._sector, iter->second);

        }

    }

  if (get_flag(ALIGN_MUTR))
    {

      cout << "TFvtxMILLEPEDE::register_fixed_detectors - mutr: "
          << _fixed_mutr_detectors.size() << endl;
      // fix mutr detectors
      for (TFvtxMILLEPEDE::MutrDetId::Map::iterator iter =
          _fixed_mutr_detectors.begin(); iter != _fixed_mutr_detectors.end();
          ++iter)
        {

          fix_parameter_mutr(iter->first._arm, iter->first._station,
              iter->first._gap, iter->first._cathode, iter->second);

        }

//        {
//          cout << "TFvtxMILLEPEDE::register_fixed_detectors - WARNING - "
//              << "Special fix papamerter for debug MuTr" << endl;
//
//          for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
//            for (int station = 0; station < MUTOO::NumberOfStations; station++)
//              for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
//                for (int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes;
//                    cathode++)
//                  {
//                    const int first_oct = arm == 0 ? 0 : 1;
//
//                    for ( //
//                    int octant = first_oct; //
//                    octant < MUTOO::NumberOfOctants; octant++)
//                      for (int half_octant = 0;
//                          half_octant < MUTOO::NumberOfHalfOctants;
//                          half_octant++)
//                        {
//                          fix_parameter_mutr(arm, station, octant, half_octant,
//                              gap, cathode, ALL);
//                        }
//                  }
//
//          fix_parameter_mutr(0, 2, 2, 0, ALL);
//          fix_parameter_mutr(0, 2, 2, 1, ALL);
//          fix_parameter_mutr(1, 2, 2, 0, ALL);
//          fix_parameter_mutr(1, 2, 2, 1, ALL);
//
//        }

      // possibly constrain both half-octants
      if (get_flag(USE_CONSTRAINTS_MU_ARM))
        {

          const int max_station =
              (get_flag(USE_CONSTRAINTS_MU_ARM_LastStation)) ?
                  (MUTOO::NumberOfStations) : (MUTOO::NumberOfStations - 1);

          for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
            for (int station = 0; station < max_station; station++)
              for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
                for (int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes;
                    cathode++)
                  for (int octant = 0; octant < MUTOO::NumberOfOctants;
                      octant++)
                    {
                      constraint_mutr_half_octant_rotation(arm, station, gap,
                          cathode, octant);
                    }

          for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
            for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
              for (int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes;
                  cathode++)
                {
                  constraint_quards_station0(arm, gap, cathode);
                }

          for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
            for (int station = 0; station < MUTOO::NumberOfStations; station++)
              for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
                for (int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes;
                    cathode++)
                  for (int octant = 0; octant < MUTOO::NumberOfOctants;
                      octant++)
                    {

                      if (is_fixed_mutr_cathode(arm, station, gap, cathode,
                          PAR_W))
                        continue;

                      double angles[MUTOO::NumberOfHalfOctants] =
                        { 0 };

                      for (int half_octant = 0;
                          half_octant < MUTOO::NumberOfHalfOctants;
                          half_octant++)
                        {
                          angles[half_octant] = get_half_angle_mutr(arm,
                              station, octant, half_octant, gap, cathode, 0);
                        }

                      if (abs(sin(angles[0] - angles[1])) < 0.0001)
                        constraint_mutr_half_octant_translation(arm, station,
                            gap, cathode, octant);

//                      cout <<"constraint_mutr_half_octant_translation done: " << " arm = " << arm << " station = " << station << " gap = " << gap
//                          << " cath = " << cathode << " octant = " << octant<<endl;
                    }

          if (get_flag(USE_CONSTRAINTS_MU_ARM_WithInStation))
            for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
              for (int station = 0; station < MUTOO::NumberOfStations;
                  station++)
                {
                  constraint_mutr_station(arm, station);
                }
        }
    }
  if (get_flag(ALIGN_MUID))
    {

      cout << "TFvtxMILLEPEDE::register_fixed_detectors - muid: "
          << _fixed_muid_detectors.size() << endl;

      // fixed muid detectors
      for (TFvtxMILLEPEDE::MuidDetId::Map::iterator iter =
          _fixed_muid_detectors.begin(); iter != _fixed_muid_detectors.end();
          ++iter)
        {
          fix_parameter_muid(iter->first._arm, iter->first._plane,
              iter->first._orientation, iter->second);
        }

      // possibly constrain both half-sectors
      if (get_flag(USE_CONSTRAINTS_MU_ARM) && get_flag(ALIGN_PHI_MU_ARM))
        if (get_flag(USE_CONSTRAINTS_MUID_WithInPanel))
          for (int arm = 0; arm < MUIOO::MAX_ARM; arm++)
            for (int plane = 0; plane < MUIOO::MAX_PLANE; plane++)
              for (int panel = 0; panel < MUIOO::MAX_PANEL; panel++)
                {
                  constraint_muid_orientations(arm, plane, panel);
                }

    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_parameter_fvtx_station(int arm, int cage, int station,
    int parameter_bit)
{
  assert(get_flag(ALIGN_FVTX));
  if (!_par_init)
    {
      cout
          << "TFvtxMILLEPEDE::fix_w - fvtx station parameters not initialized.\n";
      return;
    }

    {
      int index = get_index_station(arm, cage, station);
      if (parameter_bit & PAR_W)
        {
          C_PARSIG( index*NPARPLAN+IDX_STA_X+1, 0.0);
          C_PARSIG( index*NPARPLAN+IDX_STA_Y+1, 0.0);
        }
      if (parameter_bit & PAR_Z)
        {
          C_PARSIG( index*NPARPLAN+IDX_STA_Z+1, 0.0);
        }
      if (parameter_bit & PAR_PHI)
        C_PARSIG( index*NPARPLAN+IDX_STA_PHI+1, 0.0);
      if (parameter_bit & PAR_PSIX)
        C_PARSIG( index*NPARPLAN+IDX_STA_PSIX+1, 0.0);
      if (parameter_bit & PAR_PSIY)
        C_PARSIG( index*NPARPLAN+IDX_STA_PSIY+1, 0.0);
    }

  return;

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_parameter_fvtx_wedge(int arm, int cage, int station,
    int sector, int parameter_bit)
{
  assert(get_flag(ALIGN_FVTX));
  if (!_par_init)
    {
      cout
          << "TFvtxMILLEPEDE::fix_w - fvtx wedge parameters not initialized.\n";
      return;
    }

  for (int half = 0; half < FVTXGEOM::NumberOfColumns; half++)
    {
      int index = get_index_half(arm, cage, station, sector, half);
      if (parameter_bit & PAR_W)
        C_PARSIG( index*NPARPLAN+IDX_HALF_W+1, 0.0);
      if (parameter_bit & PAR_Z)
        C_PARSIG( index*NPARPLAN+IDX_HALF_Z+1, 0.0);
      if (parameter_bit & PAR_PHI)
        C_PARSIG( index*NPARPLAN+IDX_HALF_PHI+1, 0.0);
    }

  return;

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_halfs(int arm, int cage, int station, int sector)
{

//  cout << "TFvtxMILLEPEDE::constraint_halfs " << endl;
//  for (int i_par = 1; i_par < 4; i_par++)
  cout
      << "TFvtxMILLEPEDE::constraint_halfs w/ tmp fix to avoid overflow of Millepede # of constraint "
      << endl;
  assert(get_flag(ALIGN_FVTX));

  for (int i_par = IDX_HALF_W; i_par <= IDX_HALF_W; i_par++)
    {
      int idx[FVTXGEOM::NumberOfColumns] =
        { -1, -1 };

      vector<float> t(get_nb_det() * NPARPLAN, 0);
      for (int half = 0; half < FVTXGEOM::NumberOfColumns; half++)
        {

          int half_index = get_index_half(arm, cage, station, sector, half);

          const double w_sign = get_w_sign(arm, cage, station, sector, half);

          if (half == 0)
            t[half_index * NPARPLAN + i_par] = 1 * w_sign;
          else
            t[half_index * NPARPLAN + i_par] = -1 * w_sign;

          idx[half] = half_index * NPARPLAN + i_par;
        }
      C_CONSTF(&t[0], 0.0);

      cout << "TFvtxMILLEPEDE::constraint_halfs - for indedx " << idx[0]
          << " and " << idx[1] << endl;
    }
  return true;

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_azimuthal_modulation(int arm, int station)
{

//  cout << "TFvtxMILLEPEDE::constraint_halfs " << endl;
//  for (int i_par = 1; i_par < 4; i_par++)
  cout << "TFvtxMILLEPEDE::constraint_azimuthal_modulation - INFO - for arm "
      << arm << " station" << station << endl;

  assert(get_flag(ALIGN_FVTX));

  for (int i_par = IDX_HALF_W; i_par <= IDX_HALF_W; i_par++)
    {
      for (int mod = 0; mod <= 2; mod++)
        {

//          vector<float> ts(get_nb_det() * NPARPLAN, 0);
//          vector<float> tc(get_nb_det() * NPARPLAN, 0);
//          for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
//            for (int sector = 0; sector < FVTXGEOM::NumberOfSectors; sector++)
//              for (int half = 0; half < FVTXGEOM::NumberOfColumns; half++)
//                {
////                  const double phi = 2 * TMath::Pi()
////                      * (cage * FVTXGEOM::NumberOfSectors + sector
////                          + half * 1. / FVTXGEOM::NumberOfColumns)
////                      / (FVTXGEOM::NumberOfCages * FVTXGEOM::NumberOfSectors);
//
//                  int half_index = get_index_half(arm, cage, station, sector,
//                      half);
//                  double phi = get_half_angle(arm, cage, station, sector, half,
//                      0);
//                  const double w_sign = get_w_sign(arm, cage, station, sector,
//                      half);
//                  if (w_sign < 0)
//                    phi += TMath::Pi();
//
//                  ts[half_index * NPARPLAN + i_par] = TMath::Sin(phi * mod);
//                  tc[half_index * NPARPLAN + i_par] = TMath::Cos(phi * mod);
//
//                }
//          C_CONSTF(&ts[0], 0.0);
//          if (mod > 0)
//            C_CONSTF(&tc[0], 0.0);

        }
    }
  return true;

}

//________________________________________
void
TFvtxMILLEPEDE::set_wedge_status(int arm, int cage, int station, int sector,
    enu_detector_status status)
{
  cout << "TFvtxMILLEPEDE::set_wedge_status - "
      << ((status == DET_EXCLUDE) ? "Exclude" : "Enable") << " Wedge (" << arm
      << ", " << cage << ", " << station << ", " << sector << ") " << endl;

  assert(get_flag(ALIGN_FVTX));

  for (int half = 0; half < FVTXGEOM::NumberOfHalfWedges; half++)
    {

      _detector_status[get_index_half(arm, cage, station, sector, 0)] = status;
      _detector_status[get_index_half(arm, cage, station, sector, 1)] = status;

    }
}

//________________________________________
TFvtxMILLEPEDE::enu_detector_status
TFvtxMILLEPEDE::get_wedge_status(int arm, int cage, int station, int sector,
    int half) const
{
//  assert(get_flag(ALIGN_FVTX));

  return _detector_status[get_index_half(arm, cage, station, sector, half)];
}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_fvtx_all_stations(void)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::fix_fvtx_all_stations");
  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
        {
          fix_fvtx_station(arm, cage, station,
              PAR_Z | PAR_W | PAR_PHI | PAR_PSIX | PAR_PSIY);
        }

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_fvtx_stations(void)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::fix_fvtx_stations");
  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
        {
          if (station != 0)
            continue;
          fix_fvtx_station(arm, cage, station,
              PAR_Z | PAR_W | PAR_PHI | PAR_PSIX | PAR_PSIY);
        }

}

//______________________________________________________
void
TFvtxMILLEPEDE::constrain_fvtx_stations(void)
{
  //constrain fvtx stations to avoid global movements

  MUTOO::TRACE("TFvtxMILLEPEDE::constrain_fvtx_stations");

  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      // get average z location for each station
      TVectorD station_z(FVTXGEOM::NumberOfStations);
      station_z.Zero();

      for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
        {
          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            {

              FvtxStation* strip_sta =
                  FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station);

              assert(strip_sta);

              station_z[station] += strip_sta->get_z();
            }
        }

      station_z *= (1. / FVTXGEOM::NumberOfCages);
      station_z -= station_z.Sum() / FVTXGEOM::NumberOfStations;

      // build constraints

      // y, dy/dz constraints - each arm is constraint independantly
      vector<float> arm_y(get_nb_det() * NPARPLAN, 0);
      vector<float> arm_ty(get_nb_det() * NPARPLAN, 0);

      // x, dx/dz constraints - each cage is constraint independantly
      vector<float> cage_x(get_nb_det() * NPARPLAN, 0);
      vector<float> cage_tx(get_nb_det() * NPARPLAN, 0);

      for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
        {

          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            {
              const int index = get_index_station(arm, cage, station);

              arm_y[NPARPLAN * index + IDX_STA_Y] = 1;
              arm_ty[NPARPLAN * index + IDX_STA_Y] = station_z[station];

              cage_x[NPARPLAN * index + IDX_STA_X] = 1;
              cage_tx[NPARPLAN * index + IDX_STA_X] = station_z[station];
            }

          if (get_flag(USE_CONSTRAINTS_CAGE_POS))
            {
              cout
                  << "TFvtxMILLEPEDE::constrain_fvtx_stations - constraint arm "
                  << arm << " cage " << cage << " on cage X" << endl;
              C_CONSTF(&cage_x[0], 0.0);
            }
          cout << "TFvtxMILLEPEDE::constrain_fvtx_stations - constraint arm "
              << arm << " cage " << cage << " on cage TX" << " ["
              << NPARPLAN * get_index_station(arm, cage, 0) + IDX_STA_X << ", "
              << NPARPLAN * get_index_station(arm, cage, 1) + IDX_STA_X << ", "
              << NPARPLAN * get_index_station(arm, cage, 2) + IDX_STA_X << ", "
              << NPARPLAN * get_index_station(arm, cage, 3) + IDX_STA_X << "]"
              << "= ("
              << cage_tx[NPARPLAN * get_index_station(arm, cage, 0) + IDX_STA_X]
              << ", "
              << cage_tx[NPARPLAN * get_index_station(arm, cage, 1) + IDX_STA_X]
              << ", "
              << cage_tx[NPARPLAN * get_index_station(arm, cage, 2) + IDX_STA_X]
              << ", "
              << cage_tx[NPARPLAN * get_index_station(arm, cage, 3) + IDX_STA_X]
              << ")" << endl;
          C_CONSTF(&cage_tx[0], 0.0);

        } // for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      if (get_flag(USE_CONSTRAINTS_CAGE_POS))
        {
          cout << "TFvtxMILLEPEDE::constrain_fvtx_stations - constraint arm "
              << arm << " on cage Y" << endl;
          C_CONSTF(&arm_y[0], 0.0);
        }
      cout << "TFvtxMILLEPEDE::constrain_fvtx_stations - constraint arm " << arm
          << " on cage TY" << endl;
      C_CONSTF(&arm_ty[0], 0.0);

    } //   for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)

}

//______________________________________________________
void
TFvtxMILLEPEDE::constrain_fvtx_cage_z_shear(void)
{
  //constrain fvtx stations to avoid global movements

  MUTOO::TRACE("TFvtxMILLEPEDE::constrain_fvtx_cage_z_shear");

  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
        {
          // get average z location for each station
          TVectorD station_z(FVTXGEOM::NumberOfStations);
          station_z.Zero();

          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            {
              FvtxStation* strip_sta =
                  FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station);

              assert(strip_sta);

              station_z[station] = strip_sta->get_z();
            }
          station_z -= station_z.Sum() / FVTXGEOM::NumberOfStations;

          vector<float> const_z(get_nb_det() * NPARPLAN, 0);

          for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
            {
              const int index = get_index_station(arm, cage, station);

              const_z[NPARPLAN * index + IDX_STA_Z] = station_z[station];

              cout
                  << "TFvtxMILLEPEDE::constrain_fvtx_cage_z_shear - constraint z to have no shear within "
                  << " arm " << arm << " cage " << cage << " station "
                  << station << " ID " << NPARPLAN * index + IDX_STA_Z
                  << " with constraint constatnt" << station_z[station] << endl;
            }

          C_CONSTF(&const_z[0], 0.0);

        } //   for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::constrain_fvtx_cage_z(void)
{
  //constrain fvtx stations to avoid global movements

  MUTOO::TRACE("TFvtxMILLEPEDE::constrain_fvtx_cage_z");

  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
        {
          const int index_ref = get_index_station(arm, cage, 0);

          for (int station = 1; station < FVTXGEOM::NumberOfStations; station++)
            {
              cout
                  << "TFvtxMILLEPEDE::constrain_fvtx_cage_z - constraint z to be equal for"
                  << " arm " << arm << " cage " << cage << " station "
                  << station << " and station 0" << endl;

              const int index = get_index_station(arm, cage, station);

              vector<float> const_z(get_nb_det() * NPARPLAN, 0);

              const_z[NPARPLAN * index_ref + IDX_STA_Z] = 1;
              const_z[NPARPLAN * index + IDX_STA_Z] = -1;

              C_CONSTF(&const_z[0], 0.0);
            }

        } //   for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_fvtx_all_wedges(const ParameterBit fix_flag)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::fix_fvtx_all_wedges");
  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
        for (int sector = 0; sector < FVTXGEOM::NumberOfSectors; sector++)
          {
            fix_fvtx_wedge(arm, cage, station, sector, fix_flag);
          }

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_fvtx_2station_wedges(void)
{

  MUTOO::TRACE("TFvtxMILLEPEDE::fix_fvtx_2station_wedges");
  assert(get_flag(ALIGN_FVTX));

  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    for (int cage = 0; cage < FVTXGEOM::NumberOfCages; cage++)
      for (int station = 0; station < FVTXGEOM::NumberOfStations; station++)
        for (int sector = 0; sector < FVTXGEOM::NumberOfSectors; sector++)
          {
            if (station == 1 || station == 2)
              continue;
            fix_fvtx_wedge(arm, cage, station, sector, PAR_Z | PAR_W | PAR_PHI);
          }

}

//______________________________________________________
int
TFvtxMILLEPEDE::get_index_station(int arm, int cage, int station) const
{
  assert(get_flag(ALIGN_FVTX));

  int local_index(
      station
          + FVTXGEOM::NumberOfStations
              * (cage + FVTXGEOM::NumberOfCages * (arm)));

  return local_index * 2 + get_index_offset_fvtx();

}

//______________________________________________________
int
TFvtxMILLEPEDE::get_index_half(int arm, int cage, int station, int sector,
    int half) const
{

  assert(get_flag(ALIGN_FVTX));

  assert( arm >= 0 && arm < FVTXOO::MAX_ARM);
  assert( cage >= 0 && cage < FVTXOO::MAX_CAGE);
  assert( station >= 0 && station < FVTXOO::MAX_STATION);
  assert( sector >= 0 && sector < FVTXOO::MAX_SECTOR);
  assert( half >= 0 && half < FVTXOO::MAX_COLUMN);

  int local_index(
      _nb_fvtx_station * 2 + half
          + FVTXGEOM::NumberOfColumns
              * (sector
                  + FVTXGEOM::NumberOfSectors
                      * (station
                          + FVTXGEOM::NumberOfStations
                              * (cage + FVTXGEOM::NumberOfCages * (arm)))));

  return local_index + get_index_offset_fvtx();

}

//________________________________________
double
TFvtxMILLEPEDE::get_half_angle(int arm, int cage, int station, int sector,
    int half, int strip)
{

  double angle = 0;
  FvtxStrip *a_strip = 0;
  a_strip =
      FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station)->get_sector(
          sector)->get_column(half)->get_strip(strip);
  if (!a_strip)
    cout << "TFvtxMILLEPEDE::get_half_angle() " << " arm " << arm << " cage "
        << cage << " station " << station << " sector " << sector << " column "
        << half << " strip " << strip << " not found " << endl;
  else
    {
      angle = a_strip->get_angle();
      angle = (angle < -M_PI_2) ? angle + M_PI : angle;
      angle = (angle > M_PI_2) ? angle - M_PI : angle;
    }

  return angle;
}

//________________________________________
int
TFvtxMILLEPEDE::get_w_sign(int arm, int cage, int station, int sector, int half)
{
  int sign = 0;
  FvtxStrip *a_strip = 0;
  a_strip =
      FvtxGeom::get_arm(arm)->get_cage(cage)->get_station(station)->get_sector(
          sector)->get_column(half)->get_strip(0);
  if (!a_strip)
    cout << "TFvtxMILLEPEDE::get_w_sign() " << " arm " << arm << " cage "
        << cage << " station " << station << " sector " << sector << " column "
        << half << " strip " << 0 << " not found " << endl;
  else
    {
      sign = a_strip->get_w_sign();
    }
  return sign;
}

//______________________________________________________
bool
TFvtxMILLEPEDE::check_w_sign_mutr_halfocts()
{
//  assert(get_flag(ALIGN_MUTR) or get_flag(USE_MUTR_HITS));
  cout << "TFvtxMILLEPEDE::check_w_sign_mutr_halfocts starts" << endl;

  bool success = true;

  for (unsigned short arm = 0; arm < MUTGEOM::NumberOfArms; arm++)
    for (unsigned short station = 0; station < MUTGEOM::NumberOfStations;
        station++)
      for (unsigned short octant = 0; octant < MUTGEOM::NumberOfOctants;
          octant++)
        for (unsigned short half = 0; half < MUTGEOM::NumberOfHalfOctants;
            half++)
          for (unsigned short gap = 0; gap < MUTGEOM::NumberOfGaps; gap++)

            // reject station3 gap3 since it does not exist
            if (station != MUTGEOM::Station3 || gap != MUTGEOM::Gap3)
              for (unsigned short cathode = 0;
                  cathode < MUTGEOM::NumberOfCathodePlanes; cathode++)
                {

                  // retrieve matching plane
                  unsigned short plane_id =
                      (cathode == 0) ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2;
                  MutArm* geometry(
                      (arm == MUTGEOM::South) ? SouthArm() : NorthArm());
                  MutHalfOctant *half_octant =
                      geometry->f_pMutStations[station]->f_pMutOctants[octant]->f_pMutHalfOctants[half];
                  assert(half_octant);

                  MutPlane *plane =
                      half_octant->f_pMutGaps[gap]->f_pMutPlanes[plane_id];
                  assert(plane);

                  // check strip angles
                  double min_angle = +2 * M_PI;
                  double max_angle = -2 * M_PI;
                  double min_angle_cor = +2 * M_PI;
                  double max_angle_cor = -2 * M_PI;
                  for (unsigned short strip = 0;
                      strip < plane->f_pMutStrips.size(); ++strip)
                    {
                      const double angle = TMutGeo::get_cathode_angle(arm,
                          station, octant, half, gap, cathode, strip);

                      min_angle = angle < min_angle ? angle : min_angle;
                      max_angle = angle > max_angle ? angle : max_angle;

                      const double angle_cor = get_half_angle_mutr(arm, station,
                          octant, half, gap, cathode, strip);

                      min_angle_cor =
                          angle_cor < min_angle_cor ? angle_cor : min_angle_cor;
                      max_angle_cor =
                          angle_cor > max_angle_cor ? angle_cor : max_angle_cor;
                    }

                  if (abs(min_angle - max_angle) > 1e-4
                      or (arm == 0 and station == 0 and octant == 6 and gap == 0
                          and cathode == 0))
                    {
                      cout
                          << Form(
                              "TFvtxMILLEPEDE::check_w_sign_mutr_halfocts::[%d %d %d %d %d %d] ",
                              arm, station, octant, half, gap, cathode)
                          << "- WARNING - inconsistent strip angle range from "
                          << min_angle << " to " << max_angle << " w/ diff \t= "
                          << max_angle - min_angle
                          << ". After corrections range from " << min_angle_cor
                          << " to " << max_angle_cor << " w/ diff \t= "
                          << max_angle_cor - min_angle_cor << endl;

                      success = false;
                    }
                }

  return success;
}

//______________________________________________________
//! get sign correction for w direction of MuTr
int
TFvtxMILLEPEDE::get_w_sign_cor_mutr(unsigned short arm, unsigned short station,
    unsigned short octant, unsigned short half_octant, unsigned short gap,
    unsigned short cathode, unsigned short strip)
{

  const double angle = TMutGeo::get_cathode_angle(arm, station, octant,
      half_octant, gap, cathode, strip);
  const double angle0 = TMutGeo::get_cathode_angle(arm, station, octant,
      half_octant, gap, cathode, 0);

  return boost::math::sign(cos(angle - angle0));
}

//______________________________________________________
//! get corrected half angle which consistent with that for strip0
double
TFvtxMILLEPEDE::get_half_angle_mutr(unsigned short arm, unsigned short station,
    unsigned short octant, unsigned short half_octant, unsigned short gap,
    unsigned short cathode, unsigned short strip)
{

  const double angle = TMutGeo::get_cathode_angle(arm, station, octant,
      half_octant, gap, cathode, strip);
  const double angle0 = TMutGeo::get_cathode_angle(arm, station, octant,
      half_octant, gap, cathode, 0);

  if (angle - angle0 > M_PI_2)
    {
      assert(
          get_w_sign_cor_mutr(arm, station, octant, half_octant, gap, cathode, strip)<0);
      return angle - M_PI;
    }
  else if (angle - angle0 < -M_PI_2)
    {
      assert(
          get_w_sign_cor_mutr(arm, station, octant, half_octant, gap, cathode, strip)<0);
      return angle + M_PI;
    }
  else
    {
      return angle;
    }

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_mutr_cathode(int arm, int station, int gap, int cathode,
    unsigned int flags)
{

  assert(get_flag(ALIGN_MUTR));

  TFvtxMILLEPEDE::MutrDetId id(arm, station, gap, cathode);
  TFvtxMILLEPEDE::MutrDetId::Map::iterator iter(_fixed_mutr_detectors.find(id));
  if (iter == _fixed_mutr_detectors.end())
    {
      _fixed_mutr_detectors.insert(make_pair(id, flags));
    }
  else
    {
      iter->second |= flags;
    }

  cout << "TFvtxMILLEPEDE::fix_mutr_cathode - [" << arm << "," << station << ","
      << gap << "," << cathode << "] flags:" << flags << ". Total fix:"
      << _fixed_mutr_detectors.size() << endl;
}

//______________________________________________________
bool
TFvtxMILLEPEDE::is_fixed_mutr_cathode(int arm, int station, int gap,
    int cathode, unsigned int flags)
{

  assert(get_flag(ALIGN_MUTR));

  TFvtxMILLEPEDE::MutrDetId id(arm, station, gap, cathode);
  TFvtxMILLEPEDE::MutrDetId::Map::iterator iter(_fixed_mutr_detectors.find(id));
  if (iter == _fixed_mutr_detectors.end())
    {
      return false;
    }
  else
    {
      return (iter->second) & flags;
    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_muid_plane(int arm, int plane, int orientation,
    unsigned int flags)
{
  assert(get_flag(ALIGN_MUID));

  TFvtxMILLEPEDE::MuidDetId id(arm, plane, orientation);
  TFvtxMILLEPEDE::MuidDetId::Map::iterator iter(_fixed_muid_detectors.find(id));
  if (iter == _fixed_muid_detectors.end())
    {
      _fixed_muid_detectors.insert(make_pair(id, flags));
    }
  else
    {
      iter->second |= flags;
    }
}

//______________________________________________________
bool
TFvtxMILLEPEDE::is_fixed_muid_plane(int arm, int plane, int orientation,
    unsigned int flags) const
{

  assert(get_flag(ALIGN_MUID));

  TFvtxMILLEPEDE::MuidDetId id(arm, plane, orientation);
  TFvtxMILLEPEDE::MuidDetId::Map::const_iterator iter(
      _fixed_muid_detectors.find(id));
  if (iter == _fixed_muid_detectors.end())
    {
      return false;
    }
  else
    {
      return (iter->second) & flags;
    }
}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_parameter_mutr(int arm, int station, int gap, int cathode,
    int parameter_bit)
{
  assert(get_flag(ALIGN_MUTR));

  if (!_par_init)
    {
      cout
          << "TFvtxMILLEPEDE::fix_parameter_mutr - mutr parameters not initialized.\n";
      return;
    }

  cout << "TFvtxMILLEPEDE::fix_parameter_mutr - fixing MuTr " << "Arm" << arm
      << " station" << station << " gap" << gap << " cathode" << cathode
      << endl;

  for (int octant = 0; octant < MUTOO::NumberOfOctants; octant++)
    for (int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants;
        half_octant++)
      {
        fix_parameter_mutr(arm, station, octant, half_octant, gap, cathode,
            parameter_bit);
      }

  return;

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_parameter_mutr(int arm, int station, int octant,
    int half_octant, int gap, int cathode, int parameter_bit)
{
  assert(get_flag(ALIGN_MUTR));

  if (!_par_init)
    {
      cout
          << "TFvtxMILLEPEDE::fix_parameter_mutr - mutr parameters not initialized.\n";
      return;
    }

  int half_octant_index = get_index_half_octant(arm, station, octant,
      half_octant, gap, cathode);

  if (parameter_bit & PAR_W)
    C_PARSIG( half_octant_index*NPARPLAN + IDX_HALF_W +1, 0.0);
  if (parameter_bit & PAR_Z)
    C_PARSIG( half_octant_index*NPARPLAN+ IDX_HALF_Z +1, 0.0);
  if (parameter_bit & PAR_PHI)
    C_PARSIG( half_octant_index*NPARPLAN+ IDX_HALF_PHI +1, 0.0);

  return;

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_parameter_muid(int arm, int plane, int orientation,
    int parameter_bit)
{
  assert(get_flag(ALIGN_MUID));

  if (!_par_init)
    {
      cout
          << "TFvtxMILLEPEDE::fix_parameter_muid - muid parameters not initialized.\n";
      return;
    }

  cout << "TFvtxMILLEPEDE::fix_parameter_muid - fixing MuID " << "Arm" << arm
      << " Plane" << plane << " Orientation" << orientation << endl;

  for (int panel = 0; panel < MUIOO::MAX_PANEL; panel++)
    {
      int panel_index = get_index_panel(arm, plane, panel, orientation);
      if (parameter_bit & PAR_W)
        C_PARSIG( panel_index*NPARPLAN + IDX_HALF_W +1, 0.0);
      if (parameter_bit & PAR_Z)
        C_PARSIG( panel_index*NPARPLAN+ IDX_HALF_Z +1, 0.0);
      if (parameter_bit & PAR_PHI)
        C_PARSIG( panel_index*NPARPLAN+ IDX_HALF_PHI +1, 0.0);
    }

  return;

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_mutr_all(void)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::fix_mutr_all");
  assert(get_flag(ALIGN_MUTR));

  for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
    for (int station = 0; station < MUTOO::NumberOfStations; station++)
      for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
        {
          fix_mutr_gap(arm, station, gap, PAR_Z | PAR_W | PAR_PHI);
        }

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_mutr_2stations(void)
{

  MUTOO::TRACE("TFvtxMILLEPEDE::fix_mutr_2stations");
  assert(get_flag(ALIGN_MUTR));

  for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
    for (int station = 0; station < MUTOO::NumberOfStations; station++)
      for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
        {
          if (station == 2 && gap != 2)
            continue;
          fix_mutr_gap(arm, station, gap, PAR_Z | PAR_W | PAR_PHI);
        }

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_mutr_2gaps(void)
{

  MUTOO::TRACE("TFvtxMILLEPEDE::fix_mutr_2gaps");
  assert(get_flag(ALIGN_MUTR));

  for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
    {

//      // fix station 2 cathode 2 (since the detector does not exist
//      fix_mutr_gap(arm, 2, 2, PAR_Z | PAR_W | PAR_PHI);

      // north and south arm
      // fix station0 gap1 and station1 gap0

      if (!get_flag(USE_CONSTRAINTS_MUTR_ZeroStation))
        fix_mutr_gap(arm, 0, 1, PAR_Z | PAR_W | PAR_PHI);

      if (!get_flag(USE_CONSTRAINTS_MUTR_ZeroStation) and //
          !get_flag(USE_CONSTRAINTS_MU_ARM_OneStation))
        fix_mutr_gap(arm, 1, 0, PAR_Z | PAR_W | PAR_PHI);

      if (get_flag(USE_CONSTRAINTS_MU_ARM_WithInStation))
        for (int station = 0; station < 2; station++)
          for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
            fix_mutr_gap(arm, station, gap, PAR_PHI);
    }

}

//______________________________________________________
void
TFvtxMILLEPEDE::fix_muid_2planes(void)
{

  MUTOO::TRACE("TFvtxMILLEPEDE::fix_muid_2planes");
  assert(get_flag(ALIGN_MUID));

  for (int arm = 0; arm < MUIOO::MAX_ARM; arm++)
    {

      if (!get_flag(USE_CONSTRAINTS_MUID_ZeroStation))
        {
          if (!get_flag(USE_CONSTRAINTS_MU_ARM_OneStation))
            fix_muid_plane(arm, 0, PAR_Z | PAR_W | PAR_PHI);
          fix_muid_plane(arm, 4, PAR_Z | PAR_W | PAR_PHI);
        }
    }

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_mutr_half_octant_rotation(int arm, int station,
    int gap, int cath, int octant)
{
  if (is_fixed_mutr_cathode(arm, station, gap, cath, PAR_PHI))
    {
      cout
          << "TFvtxMILLEPEDE::constraint_mutr_half_octant_rotation - ignore for fixed cathode: "
          << " arm = " << arm << " station = " << station << " gap = " << gap
          << " cath = " << cath << " octant = " << octant << endl;

      return false;
    }

  cout << "TFvtxMILLEPEDE::constraint_mutr_half_octant_rotation " << " arm = "
      << arm << " station = " << station << " gap = " << gap << " cath = "
      << cath << " octant = " << octant << endl;
  assert(get_flag(ALIGN_MUTR));

//  for (int i_par = IDX_HALF_Z; i_par <= IDX_HALF_PHI; ++i_par)
  int i_par = IDX_HALF_PHI;
    {
      vector<float> t(get_nb_det() * NPARPLAN, 0);
      for (int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants;
          half_octant++)
        {

          int half_octant_index = get_index_half_octant(arm, station, octant,
              half_octant, gap, cath);

          if (half_octant == 0)
            t[half_octant_index * NPARPLAN + i_par] = 1;
          else
            t[half_octant_index * NPARPLAN + i_par] = -1;
        }
      C_CONSTF(&t[0], 0.0);
    }

  return true;

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_mutr_half_octant_translation(int arm, int station,
    int gap, int cath, int octant)
{
  if (is_fixed_mutr_cathode(arm, station, gap, cath, PAR_W))
    {
      cout
          << "TFvtxMILLEPEDE::constraint_mutr_half_octant_translation - ignore for fixed cathode: "
          << " arm = " << arm << " station = " << station << " gap = " << gap
          << " cath = " << cath << " octant = " << octant << endl;

      return false;
    }

  const double relative_w_sign = boost::math::sign(
      cos(
          get_half_angle_mutr(arm, station, octant, 0, gap, cath, 0)
              - get_half_angle_mutr(arm, station, octant, 1, gap, cath, 0)));

  cout << "TFvtxMILLEPEDE::constraint_mutr_half_octant_translation "
      << " arm = " << arm << " station = " << station << " gap = " << gap
      << " cath = " << cath << " octant = " << octant << ". Relative w sign = "
      << relative_w_sign << endl;
  assert(get_flag(ALIGN_MUTR));
  assert(relative_w_sign!=0);

//  for (int i_par = IDX_HALF_Z; i_par <= IDX_HALF_PHI; ++i_par)
  int i_par = IDX_HALF_W;
    {
      vector<float> t(get_nb_det() * NPARPLAN, 0);
      for (int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants;
          half_octant++)
        {

          int half_octant_index = get_index_half_octant(arm, station, octant,
              half_octant, gap, cath);

          if (half_octant == 0)
            t[half_octant_index * NPARPLAN + i_par] = 1;
          else
            t[half_octant_index * NPARPLAN + i_par] = -relative_w_sign;
        }
      C_CONSTF(&t[0], 0.0);
    }

  return true;

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_mutr_station(int arm, int station)
{

  cout << "TFvtxMILLEPEDE::constraint_mutr_station " << " arm = " << arm
      << " station = " << station << endl;
  assert(get_flag(ALIGN_MUTR));
  assert(!(get_flag(USE_CONSTRAINTS_MU_ARM_WithInStation)));

//  for (int i_par = IDX_HALF_Z; i_par <= IDX_HALF_PHI; ++i_par)
  int i_par = IDX_HALF_PHI;

  for (int octant = 0; octant < MUTOO::NumberOfOctants; octant++)
    {
      // to properly with with constraint_halfs() and constraint_quards_station0()
      if (station == 0 && (octant % 2) == 0)
        continue;
      int half_octant = 0;

//    for (int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants;
//        half_octant++)
      for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
        for (int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++)
          {
            if (gap == 0 && cathode == 0)
              continue;

            if (is_fixed_mutr_cathode(arm, station, gap, cathode, PAR_PHI))
              {
                cout
                    << "TFvtxMILLEPEDE::constraint_mutr_station - ignore for fixed cathode: "
                    << " arm = " << arm << " station = " << station << " gap = "
                    << gap << " cath = " << cathode << " octant = " << octant
                    << endl;

                continue;
              }

            vector<float> t(get_nb_det() * NPARPLAN, 0);

            int half_octant_index_ref = get_index_half_octant(arm, station,
                octant, half_octant, 0, 0);
            int half_octant_index = get_index_half_octant(arm, station, octant,
                half_octant, gap, cathode);

            t[half_octant_index_ref * NPARPLAN + i_par] = 1;

            t[half_octant_index * NPARPLAN + i_par] = -1;

            C_CONSTF(&t[0], 0.0);
          }
    }
  return true;

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_muid_orientations(int arm, int plane,
    int panel) const
{

  cout << "TFvtxMILLEPEDE::constraint_muid_halfs " << " arm = " << arm
      << " plane = " << plane << " panel = " << panel << endl;
  assert(get_flag(ALIGN_MUID));
  assert((get_flag(USE_CONSTRAINTS_MUID_WithInPanel)));

//  for (int i_par = IDX_HALF_Z; i_par <= IDX_HALF_PHI; ++i_par)
  int i_par = IDX_HALF_PHI;

  vector<float> t(get_nb_det() * NPARPLAN, 0);
  for (int ori = 0; ori < MUIOO::MAX_ORIENTATION; ori++)
    {

      if (is_fixed_muid_plane(arm, plane, ori, PAR_PHI))
        {
          cout
              << "TFvtxMILLEPEDE::constraint_muid_orientations - ignore for fixed pannel: "
              << " arm = " << arm << " plane = " << plane << " panel = "
              << panel << " ori = " << ori << endl;

          continue;
        }

      int index = get_index_panel(arm, plane, panel, ori);

      if (ori == 0)
        t[index * NPARPLAN + i_par] = 1;
      else
        t[index * NPARPLAN + i_par] = -1;
    }
  C_CONSTF(&t[0], 0.0);

  return true;

}

//______________________________________________________
bool
TFvtxMILLEPEDE::constraint_quards_station0(int arm, int gap, int cath)
{
  assert(get_flag(ALIGN_MUTR));

  static const int station = 0;

  if (is_fixed_mutr_cathode(arm, station, gap, cath, PAR_PHI))
    {
      cout
          << "TFvtxMILLEPEDE::constraint_quards_station0 - ignore for fixed cathode: "
          << " arm = " << arm << " station = " << station << " gap = " << gap
          << " cath = " << cath << endl;

      return false;
    }

//  static int quards[2][4][2] = //[arm][quard][oct_pair]
  static int quards[MUTOO::NumberOfArms][MUTOO::NumberOfOctants / 2][2] = //[arm][quard][oct_pair]
        {
          {
            { 1, 2 },
            { 3, 4 },
            { 5, 6 },
            { 7, 0 } }, // arm 0

              {
                { 0, 1 },
                { 2, 3 },
                { 4, 5 },
                { 6, 7 } } // arm 1
        };

//  for (int i_par = IDX_HALF_Z; i_par <= IDX_HALF_PHI; ++i_par)
  int i_par = IDX_HALF_PHI;
    {
      vector<float> t(get_nb_det() * NPARPLAN, 0);

      for (int quard = 0; quard < MUTOO::NumberOfOctants / 2; quard++)
        {

          const int half_octant = 0;

          cout << "TFvtxMILLEPEDE::constraint_quards_station0 " << " arm = "
              << arm << " station = " << station << " gap = " << gap
              << " cath = " << cath << " octant ";

            {

              const int octant = quards[arm][quard][0];

              cout << octant << " and ";

              int half_octant_index = get_index_half_octant(arm, station,
                  octant, half_octant, gap, cath);

              t[half_octant_index * NPARPLAN + i_par] = 1;
            }

            {

              const int octant = quards[arm][quard][1];

              cout << octant << endl;

              int half_octant_index = get_index_half_octant(arm, station,
                  octant, half_octant, gap, cath);

              t[half_octant_index * NPARPLAN + i_par] = -1;
            }

          C_CONSTF(&t[0], 0.0);
        }
    }

  return true;

}

//________________________________________
bool
TFvtxMILLEPEDE::print_to_file(const char* filename_align)
{
  MUTOO::TRACE(string("TFvtxMILLEPEDE::print_to_file - ") + filename_align);

  // make_backup( filename_align );
  ofstream out(filename_align, ios::out);
  if (!out)
    {
      cout << "Align::DumpToFile - ERROR: cannot write to file \""
          << filename_align << "\".\n";
      return false;
    }

  if (get_flag(ALIGN_MUTR))
    export_mutr_parameters_to_tree(out);
  if (get_flag(ALIGN_MUID))
    export_muid_parameters_to_tree(out);

  print_fixed_parameters(out);

  MUTOO::PRINT(out, "CONFIGURATION");
  out << "_align_w      :" << ((get_flag(ALIGN_W_MU_ARM)) ? "true" : "false")
      << endl;
  out << "_align_z      :" << ((get_flag(ALIGN_Z_MU_ARM)) ? "true" : "false")
      << endl;
  out << "_align_phi    :" << ((get_flag(ALIGN_PHI_MU_ARM)) ? "true" : "false")
      << endl;
  out << "_align_muid     :" << ((get_flag(ALIGN_MUID)) ? "true" : "false")
      << endl;
  out << "_align_mutr     :" << ((get_flag(ALIGN_MUTR)) ? "true" : "false")
      << endl;
  out << "_iterate      :" << ((get_flag(ITERATE)) ? "true" : "false") << endl;
  out << "_scratch_filename :" << _scratch_filename << endl;
  out << "_n_std_dev    :" << _n_std_dev << endl;
  MUTOO::PRINT(out, "**");

  out.close();

  return true;

}

//________________________________________
void
TFvtxMILLEPEDE::export_mutr_parameters_to_tree(ostream &out)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::print_mutr_parameters_to_stream");

  assert(get_flag(ALIGN_MUTR));
  reset_misalignment_variables();

  boost::array<double, 2> angle;
  boost::array<double, 2> delta_w;
  boost::array<double, 2> error_w2;

  // dump mutr alignment
  MUTOO::PRINT(out, "MUON TRACKER");

  for (int arm = 0; arm < MUTOO::NumberOfArms; arm++)
    for (int station = 0; station < MUTOO::NumberOfStations; station++)
      for (int gap = 0; gap < MUTOO::NumberOfGaps; gap++)
        for (int octant = 0; octant < MUTOO::NumberOfOctants; octant++)
          for (int cath = 0; cath < MUTOO::NumberOfCathodePlanes; cath++)
            {

              for (int half_octant = 0;
                  half_octant < MUTOO::NumberOfHalfOctants; half_octant++)
                {

                  // needed to remove station3 gap3
                  if (station == 2 && gap == 2)
                    continue;

                  int index = get_index_half_octant(arm, station, octant,
                      half_octant, gap, cath);

                  // get misalignment parameters
                  TMutAlign::CathodeParameters _cathode_para =
                      TMutAlign::get_cathode_parameters(arm, station, octant,
                          gap, cath);
                  TMutAlign::AnodeParameters _anode_para =
                      TMutAlign::get_anode_parameters(arm, station, octant,
                          gap);

                  _delta_x = _cathode_para._delta_x;
                  _delta_y = _cathode_para._delta_y;
                  _delta_phi = _cathode_para._delta_phi_halfs[half_octant];
                  _delta_z = _anode_para._delta_z;

                  _angle = angle[half_octant] = get_half_angle_mutr(arm,
                      station, octant, half_octant, gap, cath, 0);
                  _delta_w = -sin(angle[half_octant]) * _delta_x
                      + cos(angle[half_octant]) * _delta_y;
                  out << " arm = " << arm << " ; station = " << station
                      << " ; octant = " << octant << " ; half octant = "
                      << half_octant << " ; gap = " << gap << " ; cathode = "
                      << cath << " - ";

                  // location parameters
                  _arm = arm;
                  _station = station;
                  _gap = gap;
                  _octant = octant;
                  _half = half_octant;
                  _cathode = cath;
                  _detector_id = kMUTR;
                  _plane = 0;
                  _panel = 0;
                  _orientation = 0;

                  // Dump W
                  int j = NPARPLAN * index + IDX_HALF_W + 1;
                  double err = errpar_(&j);
                  if (err == 0 && get_flag(ALIGN_W_MU_ARM))
                    err = -999.9;

                  out << "W: "
                      << format("%10.4f %10.4f ", _par[NPARPLAN * index], err);
                  _delta_w_millepede = delta_w[half_octant] = _par[NPARPLAN
                      * index];
                  _error_w = err;
                  error_w2[half_octant] = _error_w;

                  // Dump Z
                  j = NPARPLAN * index + IDX_HALF_Z + 1;
                  err = errpar_(&j);
                  if (err == 0 && get_flag(ALIGN_Z_MU_ARM))
                    err = -999.9;
                  out << " Z :"
                      << format("%10.4f %10.4f ", _par[NPARPLAN * index + 1],
                          err);
                  _delta_z_millepede = _par[NPARPLAN * index + 1];
                  _error_z = err;

                  // Dump Phi
                  j = NPARPLAN * index + IDX_HALF_PHI + 1;
                  err = errpar_(&j);
                  if (err == 0 && get_flag(ALIGN_PHI_MU_ARM))
                    err = -999.9;
                  out << " Phi: "
                      << format("%10.4f %10.4f  ", _par[NPARPLAN * index + 2],
                          err);
                  _delta_phi_millepede = _par[NPARPLAN * index + 2];
                  _error_phi = err;

                  // strip angle
                  out << " angle: " << _angle;

                  // Dump number of tracks
                  out << " n_tracks: " << _n_tracks[index];

                  out << endl;

                  _nb_tracks = _n_tracks[index];
                  if (half_octant == 1)
                    {
                      out << " angle1=" << angle[1] << " angle0=" << angle[0]
                          << endl;
                      if (abs(angle[0] - angle[1]) < 0.00001)
                        {

                          // use average delta w between the two half octant instead !
                          _delta_x_millepede = -(delta_w[0] * sin(angle[0])
                              + delta_w[1] * sin(angle[1])) / 2;
                          _delta_y_millepede = (delta_w[0] * cos(angle[0])
                              + delta_w[1] * cos(angle[1])) / 2;
                          _error_x = sqrt(
                              pow(sin(angle[0]) / 2, 2) * pow(error_w2[0], 2)
                                  + pow(sin(angle[1]) / 2, 2)
                                      * pow(error_w2[1], 2));
                          _error_y = sqrt(
                              pow(cos(angle[0]) / 2, 2) * pow(error_w2[0], 2)
                                  + pow(cos(angle[1]) / 2, 2)
                                      * pow(error_w2[1], 2));

                        }
                      else
                        {

                          out << "sin ( )" << sin(angle[1] - angle[0]) << endl;
                          _delta_x_millepede = (cos(angle[1]) * delta_w[0]
                              - cos(angle[0]) * delta_w[1])
                              / sin(angle[1] - angle[0]);
                          _delta_y_millepede = (sin(angle[1]) * delta_w[0]
                              - sin(angle[0]) * delta_w[1])
                              / sin(angle[1] - angle[0]);
                          _error_x = sqrt(
                              pow(cos(angle[1]) / sin(angle[1] - angle[0]), 2)
                                  * pow(error_w2[0], 2)
                                  + pow(
                                      cos(angle[0]) / sin(angle[1] - angle[0]),
                                      2) * pow(error_w2[1], 2));
                          _error_y = sqrt(
                              pow(sin(angle[1]) / sin(angle[1] - angle[0]), 2)
                                  * pow(error_w2[0], 2)
                                  + pow(
                                      sin(angle[0]) / sin(angle[1] - angle[0]),
                                      2) * pow(error_w2[1], 2));

                        }

                    }
                  else
                    {
                      _delta_x_millepede = -99999;
                      _delta_y_millepede = -99999;
                      _error_x = -99999;
                      _error_y = -99999;
                    }

                  // Fill misalignment tree
                  _alignment_tree->Fill();
                }
            }

  MUTOO::PRINT(out, "**");

  return;

}

//________________________________________
void
TFvtxMILLEPEDE::export_muid_parameters_to_tree(ostream &out)
{
  MUTOO::TRACE("TFvtxMILLEPEDE::print_muid_parameters_to_stream");

  assert(get_flag(ALIGN_MUID));

  reset_misalignment_variables();

  // dump muid alignment
  MUTOO::PRINT(out, "MUON IDENTIFIER");
  for (int arm = 0; arm < MUIOO::MAX_ARM; arm++)
    for (int plane = 0; plane < MUIOO::MAX_PLANE; plane++)
      for (int panel = 0; panel < MUIOO::MAX_PANEL; panel++)
        for (int orientation = 0; orientation < MUIOO::MAX_ORIENTATION;
            orientation++)
          {
            int index = get_index_panel(arm, plane, panel, orientation);

            // get misalignment parameters
            TMuiAlign::PanelParameters _panel_para =
                TMuiAlign::get_panel_parameters(arm, plane, panel);

            _delta_x = _panel_para._delta_x;
            _delta_y = _panel_para._delta_y;
            _delta_phi = _panel_para._delta_phi;
            _delta_z = _panel_para._delta_z;

            _angle = TMuiGeo::get_panel_angle(arm, plane, panel, orientation);
            _delta_w = -sin(_angle) * _delta_x + cos(_angle) * _delta_y;

            cout << " arm = " << arm << " ; plane = " << plane << " ; panel = "
                << panel << " ; orientation = " << orientation << " - "
                << _panel_para << endl;

            out << " arm = " << arm << " ; plane = " << plane << " ; panel = "
                << panel << " ; orientation = " << orientation << " - ";

            // Tree parameters
            _arm = arm;
            _station = 0;
            _gap = 0;
            _octant = 0;
            _half = 0;
            _cathode = 0;

            _detector_id = kMUID;
            _plane = plane;
            _panel = panel;
            _orientation = orientation;

            // Dump W
            int j = NPARPLAN * index + IDX_HALF_W + 1;
            float err = errpar_(&j);
            if (err == 0 && get_flag(ALIGN_W_MU_ARM))
              err = -999.9;
            out << "W: "
                << format("%10.4f %10.4f ", _par[NPARPLAN * index], err);
            _delta_w_millepede = _par[NPARPLAN * index];
            _error_w = err;

            // Dump Z
            j = NPARPLAN * index + IDX_HALF_Z + 1;
            err = errpar_(&j);
            if (err == 0 && get_flag(ALIGN_Z_MU_ARM))
              err = -999.9;
            out << " Z :"
                << format("%10.4f %10.4f ", _par[NPARPLAN * index + 1], err);
            _delta_z_millepede = _par[NPARPLAN * index + 1];
            _error_z = err;

            // Dump Phi
            j = NPARPLAN * index + IDX_HALF_PHI + 1;
            err = errpar_(&j);
            if (err == 0 && get_flag(ALIGN_PHI_MU_ARM))
              err = -999.9;
            out << " Phi: "
                << format("%10.4f %10.4f  ", _par[NPARPLAN * index + 2], err);
            _delta_phi_millepede = _par[NPARPLAN * index + 2];
            _error_phi = err;

            // strip angle
            out << " angle: " << _angle;

            // Dump number of tracks
            out << " n_tracks: " << _n_tracks[index];

            out << endl;
            _delta_x_millepede = -sin(_angle) * _delta_w_millepede;
            _delta_y_millepede = cos(_angle) * _delta_w_millepede;
            _error_x = sqrt(pow(sin(_angle), 2) * pow(_error_w, 2));
            _error_y = sqrt(pow(cos(_angle), 2) * pow(_error_w, 2));

            out << "_delta_x " << _delta_x << " _delta_y " << _delta_y
                << " _angle " << _angle << " _delta_w " << _delta_w
                << " _delta_w_millepede " << _delta_w_millepede << " _error_w "
                << _error_w << endl;

            _nb_tracks = _n_tracks[index];

            // Fill misalignment tree
            _alignment_tree->Fill();

          }

  MUTOO::PRINT(out, "**");

  return;

}

//________________________________________
void
TFvtxMILLEPEDE::print_fixed_parameters(ostream &out) const
{

  MUTOO::TRACE("TFvtxMILLEPEDE::print_fixed_parameters");
  MUTOO::PRINT(out, "FIXED PARAMETERS");

  if (get_flag(ALIGN_FVTX))
    {
      // fvtx station detectors
      MUTOO::PRINT(out, "FIXED Stations");
      for (FvtxStationId::Map::const_iterator iter =
          _fixed_fvtx_stations.begin(); iter != _fixed_fvtx_stations.end();
          ++iter)
        {
          out << " arm = " << iter->first._arm << " ; cage = "
              << iter->first._cage << " ; station = " << iter->first._station
              << " - ";

          if (iter->second & PAR_W)
            out << " FIX_W(X/Y)";
          if (iter->second & PAR_Z)
            out << " FIX_Z";
          if (iter->second & PAR_PHI)
            out << " FIX_PHI";
          if (iter->second & PAR_PSIX)
            out << " FIX_PSIX";
          if (iter->second & PAR_PSIY)
            out << " FIX_PSIY";
          out << endl;

        }

      // fvtx wedge detectors
      MUTOO::PRINT(out, "FIXED Wedges");
      for (FvtxWedgeId::Map::const_iterator iter = _fixed_fvtx_wedges.begin();
          iter != _fixed_fvtx_wedges.end(); ++iter)
        {
          out << " arm = " << iter->first._arm << " ; cage = "
              << iter->first._cage << " ; station = " << iter->first._station
              << " ; sector = " << iter->first._sector << " - ";

          if (iter->second & PAR_W)
            out << " FIX_W";
          if (iter->second & PAR_Z)
            out << " FIX_Z";
          if (iter->second & PAR_PHI)
            out << " FIX_PHI";
          out << endl;

        }

    }

  if (get_flag(ALIGN_MUTR))
    {
      // mutr detectors
      for (MutrDetId::Map::const_iterator iter = _fixed_mutr_detectors.begin();
          iter != _fixed_mutr_detectors.end(); ++iter)
        {
//          if (iter->first._station == 2 && iter->first._gap == 2)
//            continue;
          out << " arm = " << iter->first._arm << " ; station = "
              << iter->first._station << " ; gap = " << iter->first._gap
              << " ; cathode = " << iter->first._cathode << " - ";

          if (iter->second & PAR_W)
            out << " FIX_W";
          if (iter->second & PAR_Z)
            out << " FIX_Z";
          if (iter->second & PAR_PHI)
            out << " FIX_PHI";
          out << endl;

        }
    }

  if (get_flag(ALIGN_MUID))
    {
      MUTOO::PRINT(out, "FIXED MuID");
      for (MuidDetId::Map::const_iterator iter = _fixed_muid_detectors.begin();
          iter != _fixed_muid_detectors.end(); ++iter)
        {
          out << " arm = " << iter->first._arm << " ; plane = "
              << iter->first._plane << " ; orientation = "
              << iter->first._orientation << " - ";

          if (iter->second & PAR_W)
            out << " FIX_W";
          if (iter->second & PAR_Z)
            out << " FIX_Z";
          if (iter->second & PAR_PHI)
            out << " FIX_PHI";
          out << endl;
        }
    }

  MUTOO::PRINT(out, "**");

}

//______________________________________________________
int
TFvtxMILLEPEDE::get_index_half_octant(int arm, int station, int octant,
    int half_octant, int gap, int cathode) const
{
  assert(get_flag(ALIGN_MUTR));

  int local_index(
      cathode
          + MUTOO::NumberOfCathodePlanes
              * (gap
                  + MUTOO::NumberOfGaps
                      * (half_octant
                          + MUTOO::NumberOfHalfOctants
                              * (octant
                                  + MUTOO::NumberOfOctants
                                      * (station + MUTOO::NumberOfStations * arm)))));

  return local_index + get_index_offset_mutr();

}

//______________________________________________________
int
TFvtxMILLEPEDE::get_index_cathode(int arm, int station, int gap,
    int cathode) const
{
  assert(get_flag(ALIGN_MUTR));

  // some checks
  if (arm > MUTOO::NumberOfArms || station > MUTOO::NumberOfStations
      || gap > MUTOO::NumberOfGaps || cathode > MUTOO::NumberOfCathodePlanes)
    throw runtime_error("TFvtxMILLEPEDE::get_index_octant - invalid location");

  int local_index(
      cathode
          + MUTOO::NumberOfCathodePlanes
              * (gap
                  + MUTOO::NumberOfGaps
                      * (station + MUTOO::NumberOfStations * arm)));

  return local_index + get_index_offset_mutr();
}

//______________________________________________________
int
TFvtxMILLEPEDE::get_index_panel(int arm, int plane, int panel,
    int orientation) const
{

  assert(get_flag(ALIGN_MUID));

  // some checks
  if (arm > MUIOO::MAX_ARM || plane > MUIOO::MAX_PLANE
      || panel > MUIOO::MAX_PANEL || orientation > MUIOO::MAX_ORIENTATION)
    throw runtime_error("TFvtxMILLEPEDE::get_index_panel - invalid location");

  if (!get_flag(ALIGN_MUID))
    {
      throw DESCRIPTION( "ALIGN_MUID is false" );
      return 0;
    }

  int local_index(
      orientation
          + MUIOO::MAX_ORIENTATION
              * (panel + MUIOO::MAX_PANEL * (plane + MUIOO::MAX_PLANE * arm)));

  return get_index_offset_muid() + local_index;

}

//______________________________________________________
int
TFvtxMILLEPEDE::get_index_orientation(int arm, int plane, int orientation) const
{

  assert(get_flag(ALIGN_MUID));

  // some checks
  if (arm > MUIOO::MAX_ARM || plane > MUIOO::MAX_PLANE
      || orientation > MUIOO::MAX_ORIENTATION)
    throw runtime_error("TFvtxMILLEPEDE::get_index_panel - invalid location");

  if (!get_flag(ALIGN_MUID))
    {
      throw DESCRIPTION( "ALIGN_MUID is false" );
      return 0;
    }

  int local_index(
      orientation + MUIOO::MAX_ORIENTATION * (plane + MUIOO::MAX_PLANE * arm));

  return get_index_offset_muid() + local_index;

}

//_________________________________
string
TFvtxMILLEPEDE::make_backup(const string& filename_align)
{
  if (access(filename_align.c_str(), R_OK))
    return filename_align;

  string backup;
  unsigned int ver = 0;

  do
    {
      ostringstream what;
      what << filename_align << "." << ver;
      backup = what.str();
      ver++;
    }
  while (!access(backup.c_str(), R_OK));

  ostringstream what;
  what << "cp " << filename_align << " " << backup;
  system(what.str().c_str());

  cout << "TFvtxMILLEPEDE::make_backup - file \"" << backup << "\" created.\n";
  return backup;
}
