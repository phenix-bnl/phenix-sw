/*
 * TFvtxMPTrack.h
 *
 *  Created on: Nov 6, 2012
 *      Author: jinhuang
 */
// $$Id: TFvtxMPTrack.h,v 1.10 2015/09/09 01:50:17 jinhuang Exp $$
/*!
 * \file TFvtxMPTrack.h
 * \brief
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $$Revision: 1.10 $$
 * \date $$Date: 2015/09/09 01:50:17 $$
 */

#ifndef TFVTXMPTRACK_H_
#define TFVTXMPTRACK_H_

#include <TFvtxMPNode.h>
#include <TMutTrkPar.hh>

#include <string>
#include <cstdarg>
#include <cstdio>
#include <vector>
#include <cassert>
#include <iostream>
#include <utility>
#include <cmath>

#include <TObject.h>
#include <TVector3.h>

class TClonesArray;

//! storage for tracks and processing for
class TFvtxMPTrack : public TObject
{
  ////////////////////////////////////////////////////////
  //!@name inherited functions
  //@{

public:
  TFvtxMPTrack();

  virtual
  ~TFvtxMPTrack();


public:
  virtual void
  Reset();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual bool
  IsValid(Option_t *option = "") const;

  //@}

  ////////////////////////////////////////////////////////
  //!@name track operations
  //@{

public:
  //! process track after filling nodes
  virtual void
  process_fvtx_track(void);

  //! process track after filling nodes for MuTr-only tracks
  virtual void
  process_mutr_only_track(void);

  //! get average half angle for a track and save to _phi_avg
  virtual void
  calculate_average_half_angle();

  //! get acceptable phi window of a track and save to _phi_acpt_*
  virtual void
  calculate_phi_aceptance(int verbosity = 0, bool include_vtx = false);

  //@}

  ////////////////////////////////////////////////////////
  //!@name FVTX service functions
  //@{

public:
  //! sum of Kalman fit residuals
  double
  fvtx_sum_residual() const;

  /**
   * cut on one cluster per stations
   * @param allow_two_side_hits whether allow two clusters per station, one on each side
   * @param allow_missing_st1_or_3 whether to allow missing either station 0 or 3, but have good hit for the rest of three stations
   */
  bool
  fvtx_one_hit_per_station(const bool allow_two_side_hits,
      const bool allow_missing_st0_or_3, bool verbose = false) const;

  //! min and max hit per FVTX cluster
  bool
  fvtx_cluster_cut(const int max_hits) const;

  //! z coordinate for FVTX cluster with max abs(z)
  double
  fvtx_max_z(void) const;

  //! The node for FVTX cluster with max abs(z)
  const TFvtxMPNode_FVTX *
  fvtx_node_max_z(void) const;


  //! z coordinate for FVTX cluster with max abs(z)
  double
  fvtx_min_z(void) const;
  //@}

  ////////////////////////////////////////////////////////
  //!@name VTX service functions
  //@{

  //! true if there are max one hit on VTX per VTX layer
  bool
  vtx_one_hit_per_layer() const;

  //@}

  ////////////////////////////////////////////////////////
  //!@name W-Z fit for fvtx hits
  //@{

public:

  //! to store result for very simple linear fit in abs(w) and z plane
  class TFvtx1DTrkPar : public TObject
  {
  public:

    TFvtx1DTrkPar()
    {
      set();
    }

    void
    set(double w_in = 0, double tw_in = 0)
    {
      w = w_in;
      tw = tw_in;
    }

    double
    get_w() const
    {
      return w;
    }
    double
    get_tw() const
    {
      return tw;
    }
    double
    get_vertex() const
    {
      return -w / tw;
    }

    double
    get_w_abs_fit(const double z) const
    {
      return w + tw * z;
    }

    // with correction between 1D fit and Kalman fits
    double
    get_w_abs_fit_cor(const double z, const short unsigned int arm,
        const short unsigned int station,
        const short unsigned int sector) const;

  public:

    //! measured value in the sensitive direciton
    double w;

    //! dw/dz
    double tw;

  ClassDef(TFvtx1DTrkPar,1)

  };

protected:

public:
  //! fit result for W-Z fit
  TFvtx1DTrkPar trk_par_wz_fit;

  //@}

  ////////////////////////////////////////////////////////
  //!@name data nodes
  //@{

public:

  // ! add a new node with specific type
  TFvtxMPNode *
  add_node(TFvtxMPNode::node_type type);

  void
  remove_last_node_fvtx();

  void
  remove_nodes(TFvtxMPNode::node_type type);

  // ! @brief   how many nodes in this track
  // ! @param   type specifc type of node, or ANY_NODE_TYPE
  unsigned int
  get_n_nodes(TFvtxMPNode::node_type type) const;

  unsigned int
  get_n_nodes() const
  {
    return get_n_nodes(TFvtxMPNode::ANY_NODE_TYPE);
  }

  // ! @brief   get one node in this track
  // ! @param   type    specifc type of node, or ANY_NODE_TYPE
  // ! @param   index   index of the node
  TFvtxMPNode *
  get_node(TFvtxMPNode::node_type type, unsigned int index);

  // ! @brief   get one node in this track
  // ! @param   type    specifc type of node, or ANY_NODE_TYPE
  // ! @param   index   index of the node
  const TFvtxMPNode *
  get_node(TFvtxMPNode::node_type type, unsigned int index) const;

  TFvtxMPNode *
  get_node(int index)
  {
    return get_node(TFvtxMPNode::ANY_NODE_TYPE, index);
  }

  const TFvtxMPNode *
  get_node(int index) const
  {
    return get_node(TFvtxMPNode::ANY_NODE_TYPE, index);
  }

  //! for reference of nodes. User do not own the member pointers
  typedef std::vector<TFvtxMPNode *> t_vec_nodes;
  typedef std::vector<const TFvtxMPNode *> t_vec_nodes_const;

protected:

  enum
  {
    MAX_N_NODES = 10
  };

  unsigned int nNodes_FVTX;
  TClonesArray *Nodes_FVTX;

  unsigned int nNodes_VTX;
  TClonesArray *Nodes_VTX;

  unsigned int nNodes_MuTr;
  TClonesArray *Nodes_MuTr;

  unsigned int nNodes_MuID;
  TClonesArray *Nodes_MuID;

  unsigned int nNodes_Constraint;
  TClonesArray *Nodes_Constraint;

  //@}

  ////////////////////////////////////////////////////////
  //!@name W-Z fit for fvtx hits
  //@{

public:

  //! to store result for very simple linear fit in abs(w) and z plane
  class TFvtxTrkPar : public TObject
  {
  public:

    TFvtxTrkPar()
    {
      set();
    }

    void
    set(double x_in = 0, double y_in = 0, double z_in = 0, double tx_in = 0,
        double ty_in = 0)
    {
      x0 = x_in;
      y0 = y_in;
      z0 = z_in;
      tx = tx_in;
      ty = ty_in;
    }

    double
    get_x0() const
    {
      return x0;
    }
    double
    get_y0() const
    {
      return y0;
    }
    double
    get_z0() const
    {
      return z0;
    }
    double
    get_tx() const
    {
      return tx;
    }
    double
    get_ty() const
    {
      return ty;
    }

    const double
    get_x(const double & z) const
    {
      return x0 + tx * (z - z0);
    }

    const double
    get_y(const double & z) const
    {
      return y0 + ty * (z - z0);
    }

    TVector3
    get_proj(const double & z) const
    {
      return TVector3(get_x(z), get_y(z), z);
    }

    double get_measurement(const TFvtxMPNode *node)
    {
      return//
          x0*node->dwdx + //
          y0*node->dwdy + //
          tx*node->dwdtx + //
          ty*node->dwdty ;
    }

  public:
    //! reference x
    double x0;

    //! reference y
    double y0;

    //! reference z
    double z0;

    //!dx/dz
    double tx;

    //!dy/dz
    double ty;

  ClassDef(TFvtxTrkPar,1)

  };

public:

  //! internal generalized least squares fit of full track in 3D
  //! result saved to trk_par_vzcon and nodes
  virtual void
  interanl_fit();

  //! internal generalized least squares fit of full track in 3D
  //! result saved to disp_lateral_vtx, disp_pt_vtx and z0reco_no_VTX_DCA
  virtual void
  interanl_fit_no_VTX_DCA_Constraint();

  //! internal generalized least squares fit of full track in 3D except the given station
  //! return the projected residual at the given station
  //! require fvtx_one_hit_per_station(0,0) == true
  virtual double
  interanl_fit_FVTX_self_proj(const int station);


//  //! internal generalized least squares fit of Mutr track
//  //! result saved to mutr_disp_lateral_vtx, mutr_disp_pt_vtx and mutr_disp_theta
//  virtual void
//  interanl_fit_MuTr_Alone();


  virtual double
  residual_interanl_fit_MuTr_self_proj(
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
      short cathode);

  virtual bool
  can_interanl_fit_MuTr_self_proj(
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
      short cathode);

  virtual int
  MuTr_strip_hit(
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
      short cathode);

  virtual double
  MuTr_p_fit_v_proj(
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
      short cathode);

  virtual double
  MuTr_p_fit_w_proj(
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
      short cathode);

  //! Perform 1-D fit in the w-z plane (rather than r-z plane as in mFvtxStraightLineFit)
  //! results saved to fvtx_wz_fit and Nodes_FVTX;
  virtual void
  StraightLineFitInWZ();

  //! Perform 1-D fit in the w-z plane for both FVTX and VTX, but do not fill the w_fit_1D variables
  virtual void
  StraightLineFitInWZ_Sili();

protected:

  //! internal generalized least squares fit for vec_nodes and save to trk_par_fit
  virtual void
  interanl_fit(const t_vec_nodes_const& vec_nodes, TFvtxTrkPar &trk_par_fit);

public:

  //! Internal fit setting, vertex z0; not the vertex of fit
  double z0_fit;

  //! fit point project to z
  virtual TVector3
  internal_fit_proj(const double z) const
  {
    return trk_par.get_proj(z);
  }

  //! internal track fit parameter
  TFvtxTrkPar trk_par;

  //@}

  ////////////////////////////////////////////////////////
  //!@name event property
  //@{

public:

  enum
  {
    INVALID = -9999
  };

  //! run IDs
  int run_num;

  //! event IDs
  int event_num;

  //! track IDs
  int track_index;

  //! precise vertex from vtx (if available) or unprecise vtx vertex (otherwise)
  TVector3 vtx_point;

  //! vtx vertex z
  double vtxzp;

  //! bbc vertex z
  double bbcz;

  //! whether the event is rejected
  bool reject_event;

  //@}

  ////////////////////////////////////////////////////////
  //!@name track properties
  //@{

  //! to know if the track is rejected(non-zero) or not (0); different non-zero value means different reason to reject
  unsigned short reject_track;

  //! whether to use external fit residuals as internal fit inputs
  //! if true: subtract kalman-fit value from measurement and pass residual to Millepede
  //! if alse: directly pass the measured value and use Millepede linear fit
  //! important to set it to "true" if field on data is used to compensate track bending
  bool use_kalman_fit;

  //! MuTr track chisquare
  double chi_square_mutr;

  //! MuTr track number of degrees of freedom
  int ndf_mutr;

  //! track chisquare
  double chi_square_fvtx;

  //! track number of degrees of freedom
  int ndf_fvtx;

  //! FVTX track projection to z axis, simple W-Z fit only, no beam XY correction
  double z0reco_fvtx;

  //! average strip angle, folded to -pi/2 to pi/2
  double phi_avg;

  //! center of track phi acceptance window if it originated from the beam
  double phi_acpt_cent;

  //! width of track phi acceptance window, negative value means invalid
  double phi_acpt_width;

  //! track lateral (pT x z) displacement from beam at VTX vertex z
  double disp_lateral_vtx;

  //! track displacement from beam in pT direction at VTX vertex z
  double disp_pt_vtx;

  //! fitted vertex if there with no VTX DCA constraint
  double z0reco_no_VTX_DCA;

//  //! momentum vector from Kalman fit
//  TVector3 p_kalman;

  //! track fit from Kalman fit
  TMutTrkPar trk_par_kalman;

  //! track fit from Kalman fit of MuTr alone
  TMutTrkPar trk_par_kalman_mutr;

  //! track fit from Kalman fit
  TMutTrkPar trk_par_kalman_zref;

  //! track fit from Kalman fit of MuTr alone
  TMutTrkPar trk_par_kalman_mutr_zref;

  //! MuTr track lateral (pT x z) displacement from beam at VTX vertex z
  double mutr_disp_lateral_vtx;

  //! MuTr track displacement from beam in pT direction at VTX vertex z
  double mutr_disp_pt_vtx;

  //  kalman fit vertex fit point @ z = vtx_point.z()
  TVector3 vtx_point_kalman;

  // z location of ref point for track matching crossing the absorbver
  double z_ref;

  //  kalman fit vertex fit point @ z = vtx_point.z()
  TVector3 z_ref_point_kalman;


  //! MuTr track displacement from beam in pT direction at VTX vertex z
  unsigned short muid_lastgap;

  //! MuTr track displacement from beam in pT direction at VTX vertex z
  double dg0;

  //! MuTr track displacement from beam in pT direction at VTX vertex z
  double ddg0;
  //@}

  ClassDef(TFvtxMPTrack,9) // storage for tracks and processing for
};

#endif /* TFVTXMPTRACK_H_ */
