
//-------------------------------------------------------------------
// SvxKalmanNode.h
// Author: Matt Wysocki, July 1, 2011
//
/// \class SvxKalmanNode
/// Kalman filter node for one SvxCluster
///
/// This class represents one step ("node") in the Kalman Filter
/// routine, and inherits the basic linear algebra of the filter from
/// the KalmanNode base class.  SvxKalmanNode knows the VTX-specific
/// parts, such as the dimensions of each measurement (2), the
/// transformation from global Cartesian coordinates to local (sensor)
/// coordinates, as well as how to convert between measurement and
/// state vectors (the transformation matrix H).
///
/// The state vector I use is (1/p, lambda, phi, v, w) where lambda and
/// phi represent the momentum direction (phi=atan(py/px),
/// lambda=atan(pz/pT)=90-theta).  The measurement vector is (v,w)
/// where v and w are in the plane of the VTX sensor (this is true for
/// the state vector as well).
//-------------------------------------------------------------------

#ifndef __SVX_KALMAN_NODE_H
#define __SVX_KALMAN_NODE_H

#include "KalmanNode.h"
#ifdef USEG4
#include "InfinitePlaneTarget.h"
#include <boost/shared_ptr.hpp>
#endif
class SvxSensor;
class SvxCluster;
class SvxClusterInfo;

class SvxKalmanNode : public KalmanNode
{
 public:
  SvxKalmanNode();
  virtual ~SvxKalmanNode();

 	/// set the sensor element for this measurement
 	void set_sensor(SvxSensor* s) {_sensor = s;}
  
 	/// set the sensor element for this measurement
 	SvxSensor* get_sensor() {return _sensor;}

  /// setup the coordinate transformation matrices from the SvxSensor geometry
  void load_sensor_geometry(const SvxSensor* sensor);

  /// load the measured position and uncertainty from SvxCluster*
  void load_measurement(const SvxCluster* cluster);

  /// load the measured position and uncertainty from SvxClusterInfo*
  void load_measurement(const SvxClusterInfo* cluster);

  /// load the measured position and uncertainty from the cluster position and layer
  void load_measurement(const Eigen::VectorXd &cluster_pos, const int &cluster_layer);

  /// do or do not use energy loss in predictions
  void use_energy_loss(bool b) {_use_energy_loss = b;}

  /// energy lost since the previous layer
  void set_energy_loss(double d) {_energy_loss = d;}

  /// initialize the first node as having been filtered
  void initialize_first_node(const Eigen::VectorXd &init_p, const float relperr=0);

  /// convenience function that sets up the initial G4ErrorFreeTrajState from the previous node's info
  bool predict(const KalmanNode* prev_node);

#ifdef USEG4
  /// function that does the prediction for this layer based on input trajectory
  bool predict(G4ErrorTrajState*);
#endif

  /// wrapper to KalmanNode::filter() that does some extra coordinate transforms
  void filter();

  /// wrapper to KalmanNode::smooth() that repropagates the track first
  bool repropagate_and_smooth(const KalmanNode* prev);

  /// reset everything to defaults (zeros), also calls KalmanNode::reset()
  void reset();

#ifdef USEG4
  /// get the final trajectory at this node
  G4ErrorTrajState* get_traj_state() const;
#endif

  /// transformation matrix to go from curvilinear to local (sensor) coordinates
  Eigen::MatrixXd get_rot_curvilinear_to_local() const {return _rot_curvilinear_to_local;}

  /// return the best of {predicted,filtered,smoothed} covariance matrix, rotated into curvilinear coordinates
  Eigen::MatrixXd get_best_cov_matrix_curvilinear() const;

  Eigen::VectorXd state_vec_to_global_xyz(const Eigen::VectorXd &state) const;
  Eigen::VectorXd global_to_local_xyz(const Eigen::VectorXd &position) const;

  Eigen::VectorXd state_vec_to_global_mom(const Eigen::VectorXd &) const;
  Eigen::VectorXd global_to_local_mom(const Eigen::VectorXd &) const;
  Eigen::VectorXd local_to_global_mom(const Eigen::VectorXd &) const;

  Eigen::MatrixXd get_F_SC2SD() const {return _F_SC2SD;}
  Eigen::MatrixXd get_F_SD2SC() const {return _F_SD2SC;}

  /// make this a dummy node (ie. there is no measurement for filtering here)
  void set_dummy_node(bool b) {_dummy_node=b;}

  //-------------------------------------
 protected:
  virtual void _post_smoothing(); ///< derived class stuff that has to be done after KalmanNode::smooth() 
  virtual void _build_jacobians(); ///< construct the curvilinear <---> local/planar transformation Jacobians (curvilinear definition needs the momentum vector)
  virtual void _build_Q(); ///< take the current momentum vector and calculate multiple scattering widths for Q

  SvxSensor* _sensor; ///< SvxSensor corresponding to this node

  double _X0;        ///< radiation lengths of material in this layer

  bool _dummy_node; ///< indicates that this node has no measurement (so skip filtering)

  bool _use_energy_loss; ///< include energy loss in the prediction stage
  double _energy_loss;   ///< amount of energy loss

  ///@{ @name these define the transformation from global to local (sensor) coordinates
  /// matrix to rotate local-to-global coordinates
  Eigen::MatrixXd _rot_to_global;
  /// translation vector pointing from global to local origin
  Eigen::VectorXd _vector_global_to_local_origin;
  ///@}

  Eigen::MatrixXd _F_SC2SD;  ///< curvilinear-to-detector plane Jacobian
  Eigen::MatrixXd _F_SD2SC;  ///< detector plane-to-curvilinear Jacobian

  Eigen::VectorXd _u_axis;  ///< Local u-axis (normal to plane, away from vertex)
  Eigen::VectorXd _v_axis;  ///< Local v-axis (in plane)
  Eigen::VectorXd _w_axis;  ///< Local w-axis (in plane)

  Eigen::MatrixXd _rot_curvilinear_to_local; ///< full transformation matrix to go from curvilinear to local (sensor) coordinates
};

#endif // __SVX_KALMAN_NODE_H
