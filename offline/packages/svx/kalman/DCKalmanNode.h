
//-------------------------------------------------------------------
// DCKalmanNode.h
// Author: Matt Wysocki, July 1, 2011
//
/// \class DCKalmanNode
/// Kalman filter node for DC/PHCentralTrack
///
/// This class represents one step ("node") in the Kalman Filter
/// routine, and inherits the basic linear algebra of the filter from
/// the KalmanNode base class.  DCKalmanNode knows the DC-specific
/// parts, such as the dimensions of each measurement (4) and the state
/// vector (5), as well as how to convert between those two spaces (the
/// transformation matrix H).
///
/// The state vector I use is (1/p, lambda, phi, v, w) where lambda and
/// phi represent the momentum direction (phi=atan(py/px),
/// lambda=atan(pz/pT)=90-theta).  The measurement vector is (tan(alpha), tan(beta), v, w)
/// where v and w are in a plane tangent to the DC reference radius.
//-------------------------------------------------------------------

#ifndef __DC_KALMAN_NODE_H
#define __DC_KALMAN_NODE_H

#include "KalmanNode.h"
class PHSnglCentralTrack;

class DCKalmanNode : public KalmanNode
{
 public:
  DCKalmanNode();
  ~DCKalmanNode();

  void load_measurement(const PHSnglCentralTrack* track);

  /// do or do not use energy loss in predictions
  void use_energy_loss(bool b) {_use_energy_loss = b;}

  /// energy lost before this layer
  void set_energy_loss(double d) {_energy_loss = d;}

  /// initialize the first node as having been filtered
  void initialize_first_node(const Eigen::VectorXd &init_p, const float relperr=0);

  /// convenience function that sets up the initial G4ErrorFreeTrajState from the previous node's info
  bool predict(const KalmanNode* prev_node);

  /// wrapper to KalmanNode::filter() that does some extra coordinate transforms
  void filter();

  /// reset everything to defaults (zeros), also calls KalmanNode::reset()
  void reset();

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

  void set_dummy_node(bool b) {_dummy_node=b;}

  //-------------------------------------
 protected:
  virtual void _post_smoothing(); ///< derived class stuff that has to be done after KalmanNode::smooth() 
  virtual void _build_jacobians(); ///< construct the curvilinear <---> local/planar transformation Jacobians (curvilinear definition needs the momentum vector)
  virtual void _build_Q(); ///< take the current momentum vector and calculate multiple scattering widths for Q

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

#endif // __DC_KALMAN_NODE_H
