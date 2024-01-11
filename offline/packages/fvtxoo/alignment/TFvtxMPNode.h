/*
 * TFvtxMPNode.h
 *
 *  Created on: Nov 6, 2012
 *      Author: jinhuang
 */
// $$Id: TFvtxMPNode.h,v 1.6 2015/09/10 02:05:32 jinhuang Exp $$
/*!
 * \file TFvtxMPNode.h
 * \brief
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $$Revision: 1.6 $$
 * \date $$Date: 2015/09/10 02:05:32 $$
 */

#ifndef TFVTXMPNODE_H_
#define TFVTXMPNODE_H_

#include "TObject.h"
#include "TVector3.h"
#include <map>
#include <cassert>
#include <string>

//! storage class for hits and constrains
class TFvtxMPNode : public TObject
{
public:
  TFvtxMPNode();
  virtual
  ~TFvtxMPNode();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual bool
  IsValid(Option_t *option = "") const;

  enum node_type
  {
    ANY_NODE_TYPE = 0, FVTX, VTX, MuTr, MuID, Constraint
  };

  virtual node_type
  get_node_type() const = 0;

  virtual
  bool
  is_mul_scattering_constraint() const
  {
    return false;
  }

public:

  //!@name measurement and error
  //@{

  // ! for a measurement point of (x, y), calculate the measured value for position measurement
  double
  get_measurement(double x, double y) const;

  // ! for a measurement angle of (x, y), calculate the measured value for angle measurement
  double
  get_measurement_angle(double x, double y) const;

  // ! for a measurement point of (x, y), calculate the measured value using  dwdx and dwdy
  double
  get_measurement() const
  {
//    assert(p_det.X()!=INVALID);
    return get_measurement(p_det.X(), p_det.Y());
  }

  //! r_meas
  double meas;

  //! sigma
  double sigma;

  //! measured point
  TVector3 p_det;
  //@}

  //!@name derivatives for local track parameters, x, y, tx = dx/dz, ty = dy/dz
  //@{

  // ! for given dwdx/dwdy, fill values for dwdtx and dwdty
  void
  fill_slope_derivatives(const double z_det, const double z0_fit);

  //! dwdx
  double dwdx;

  //! dwdtx
  double dwdtx;

  //! dwdy
  double dwdy;

  //! dwdty
  double dwdty;

  //! wrt_z - derivatives for global geometry parameters
  double
  get_wrt_z() const;

  //! wrt_phi - derivatives for global geometry parameters
  double
  get_wrt_phi() const;

  //@}

  //!@name external Kalman fits
  //@{

  // ! given measurement and derivatives variables, fill kalman fit variables for position measurement
  void
  fill_kalman_fit(const double x_fit, const double y_fit);

  // ! given measurement and derivatives variables, fill kalman fit variables for angle meausrement
  void
  fill_kalman_fit_angle(const double x_fit, const double y_fit);

  //! residual
  double residu_kalman;

  //! Kalman fit value
  double fit_kalman;

  //! external kalman fit point
  TVector3 p_kalman;

  //! external kalman fit momentum
  TVector3 momentum_kalman;

  //@}

  //!@name interanl fits
  //@{

  //! assistant function to project fit position to w-direction
  //! this is suppose to same as this.fit, good for cross check
  double
  get_p_fit_w_proj() const
  {
    return dwdx * p_fit.x() + dwdy * p_fit.y();
  }

  //! assistant function to project fit position to v-direction (that perpendicular to w)
  double
  get_p_fit_v_proj() const
  {
    return -dwdy * p_fit.x() + dwdx * p_fit.y();
  }

  //! residual
  double residu;

  //! Internal fit value
  double fit;

  //! internal fit point
  TVector3 p_fit;

  //! internal fit vector
  TVector3 v_fit;

  //@}

  enum
  {
    INVALID = -9999
  };

ClassDef(TFvtxMPNode,4)
};

class TFvtxMPNode_FVTX : public TFvtxMPNode
{
public:
  TFvtxMPNode_FVTX();
  virtual
  ~TFvtxMPNode_FVTX();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual node_type
  get_node_type() const
  {
    return FVTX;
  }

  int
  get_w_sign() const;

public:

  //! arm
  short arm;

  //! cage
  short cage;

  //! station
  short station;

  //! sector
  short sector;

  //! half wedge
  short column;

  //! peak strip
  short strip;

  //! number of hit in cluster
  short hit_size;

  //! strip angle converted to -pi to pi to follow the definition of w
  double half_angle;

  //! measured charge
  double q_total;

  //! W-Z 1D fit extrapolated fitted w position in fvtx
  double w_fit_1D;

  //! W-Z 1D fit residual in fvtx
  double residu_1D;

  //! start point of strip hits
  TVector3 p_strip_begin;

  //! end point of strip hits
  TVector3 p_strip_end;

//  //! wrt_z - derivatives for global geometry parameters
//  double wrt_z;
//
//  //! wrt_phi - derivatives for global geometry parameters
//  double wrt_phi;

ClassDef(TFvtxMPNode_FVTX,3)
};

class TFvtxMPNode_VTX : public TFvtxMPNode
{
public:
  TFvtxMPNode_VTX();
  virtual
  ~TFvtxMPNode_VTX();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual node_type
  get_node_type() const
  {
    return VTX;
  }

public:

  //! apply track constraint on the direction of R or Phi. true = R
  bool switch_r_phi;

  //! layer number
  short layer;

  //! ladder number
  short ladder;

  //! sensor number
  short sensor;

  //! SVX section: 0 - Barrel; 1 - North; 2 - South
  short svxSection;

ClassDef(TFvtxMPNode_VTX,3)
};

class TFvtxMPNode_MuTr : public TFvtxMPNode
{
public:
  TFvtxMPNode_MuTr();
  virtual
  ~TFvtxMPNode_MuTr();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual node_type
  get_node_type() const
  {
    return MuTr;
  }

  // ! for given dwdx/dwdy, fill values for  dwdtx, dwdty, dwdtx_multi_scat and dwdty_multi_scat
  void
  fill_slope_derivatives(const double z_det, const double z0_fit, const double z_ref)
  {
    TFvtxMPNode::fill_slope_derivatives(z_det,  z0_fit);

    dwdtx_multi_scat = (dwdx * (z_det - z_ref));
    dwdty_multi_scat = (dwdy * (z_det - z_ref));
  }

public:

  //! Arm [0,1]
  short arm;

  //! Station [0,2]
  short station;

  //! Octant [0,7]
  short octant;

  //! Half octant [0,1]
  short half_octant;

  //! Gap [0,2]
  short gap;

  //! Index [0,1]
  short cathode;

  //! Index [0,1023]
  short strip;

  //! start point of strip hits
  TVector3 p_strip_begin;

  //! end point of strip hits
  TVector3 p_strip_end;

  //! dwdtx
  double dwdtx_multi_scat;

  //! dwdty
  double dwdty_multi_scat;

//  //! wrt_z - derivatives for global geometry parameters
//  double wrt_z;
//
//  //! wrt_phi - derivatives for global geometry parameters
//  double wrt_phi;

ClassDef(TFvtxMPNode_MuTr,7)
};

class TFvtxMPNode_MuID : public TFvtxMPNode
{
public:
  TFvtxMPNode_MuID();
  virtual
  ~TFvtxMPNode_MuID();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual node_type
  get_node_type() const
  {
    return MuID;
  }

  // ! for given dwdx/dwdy, fill values for  dwdtx, dwdty, dwdtx_multi_scat and dwdty_multi_scat
  void
  fill_slope_derivatives(const double z_det, const double z0_fit, const double z_ref)
  {
    TFvtxMPNode::fill_slope_derivatives(z_det,  z0_fit);

    dwdtx_multi_scat = (dwdx * (z_det - z_ref));
    dwdty_multi_scat = (dwdy * (z_det - z_ref));
  }

public:

  //! Arm [0,1]
  short arm;

  //! plane
  short plane;

  //! panel
  short panel;

  //! orientation
  short orientation;

  //! start point of strip hits
  TVector3 p_strip_begin;

  //! end point of strip hits
  TVector3 p_strip_end;

  //! dwdtx
  double dwdtx_multi_scat;

  //! dwdty
  double dwdty_multi_scat;

ClassDef(TFvtxMPNode_MuID,2)
};

class TFvtxMPNode_Constraint : public TFvtxMPNode
{
public:
  TFvtxMPNode_Constraint();
  virtual
  ~TFvtxMPNode_Constraint();

  virtual void
  Clear(Option_t *option = "");

  virtual void
  Print(Option_t *option = "") const;

  virtual node_type
  get_node_type() const
  {
    return Constraint;
  }

public:
  enum enu_constraint_type
  {
    //! lateral constraint at VTX vertex
    LAT_CONTRAINT_VTX = 1,

    //! DCA constraint at VTX vertex
    DCA_CONTRAINT_VTX = 2,

    //! lateral constraint at station 3
    LAT_CONTRAINT_STAION3 = 10,

    //! constraint on the multiple scattering term
    MULT_SCAT_CONSTRAINT_START = 100,

    MULT_SCAT_CONSTRAINT_DR_PT,
    MULT_SCAT_CONSTRAINT_DR_LATERAL,
    MULT_SCAT_CONSTRAINT_DTHETA_PT,
    MULT_SCAT_CONSTRAINT_DTHETA_LATERAL,

    //! constraint on the multiple scattering term
    MULT_SCAT_CONSTRAINT_END,

    DUMMY = 0 // ! not for use
  };

  virtual
  bool
  is_mul_scattering_constraint() const
  {
    return (constraint_type
        >= TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_START
        and constraint_type <= TFvtxMPNode_Constraint::MULT_SCAT_CONSTRAINT_END);
  }

  //! return string for the enu_constraint_type
  static const char *
  constraint_type_string(enu_constraint_type e)
  {
    static std::map<enu_constraint_type, const char *> name;

    if (name.size() == 0)
      {

        name[LAT_CONTRAINT_VTX] = "LAT_CONTRAINT_VTX";
        name[LAT_CONTRAINT_STAION3] = "LAT_CONTRAINT_STAION3";
        name[DCA_CONTRAINT_VTX] = "DCA_CONTRAINT_VTX";

        name[MULT_SCAT_CONSTRAINT_DR_PT] = "MULT_SCAT_CONSTRAINT_DR_PT";
        name[MULT_SCAT_CONSTRAINT_DR_LATERAL] =
            "MULT_SCAT_CONSTRAINT_DR_LATERAL";
        name[MULT_SCAT_CONSTRAINT_DTHETA_PT] = "MULT_SCAT_CONSTRAINT_DTHETA_PT";
        name[MULT_SCAT_CONSTRAINT_DTHETA_LATERAL] =
            "MULT_SCAT_CONSTRAINT_DTHETA_LATERAL";

        name[DUMMY] = "Invalid type";
        name[MULT_SCAT_CONSTRAINT_START] = "Invalid type";
        name[MULT_SCAT_CONSTRAINT_END] = "Invalid type";
      }

    return name[e];
  }

  //! type of constraint imposed
  enu_constraint_type constraint_type;

public:

ClassDef(TFvtxMPNode_Constraint,2)
};

#endif /* TFVTXMPNODE_H_ */
