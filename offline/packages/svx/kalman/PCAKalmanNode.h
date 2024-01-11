
//-------------------------------------------------------------------
// PCAKalmanNode.h
// Author: Matt Wysocki, July 1, 2011
//
/// \class PCAKalmanNode
/// Kalman filter node for one SvxCluster
///
/// This class represents one step ("node") in the Kalman Filter
/// routine, and inherits the basic linear algebra of the filter from
/// the KalmanNode base class.  PCAKalmanNode knows the VTX-specific
/// parts, such as the dimensions of each measurement (2) and the state
/// vector (5), as well as how to convert between those two spaces (the
/// transformation matrix H).
///
/// The state vector I use is (1/p, lambda, phi, x, z) where lambda and
/// phi represent the momentum direction (phi=atan(py/px),
/// lambda=atan(pz/pT)=90-theta).  The measurement vector is (x,z)
/// where x and z are in the plane of the VTX sensor (this is true for
/// the state vector as well).
//-------------------------------------------------------------------

#ifndef __PCA_KALMAN_NODE_H
#define __PCA_KALMAN_NODE_H

#include "KalmanNode.h"
#include <TVector3.h>

class PCAKalmanNode : public KalmanNode
{
 public:
  PCAKalmanNode();
  ~PCAKalmanNode();

  void load_measurement(const TVector3);

  //! do or do not use energy loss in predictions
  void use_energy_loss(bool b) {_use_energy_loss = b;}

  //! energy lost before this layer
  void set_energy_loss(double d) {_energy_loss = d;}

  //! set the scattering width to use in the process noise matrix
  void set_scattering_delta_theta(double d) {_dtheta_ms = d;}

  //! get the scattering width that is used in the process noise matrix
  double get_scattering_delta_theta() {return _dtheta_ms;}

  //! initialize the first node as having been filtered
  void initialize_first_node(const Eigen::VectorXd &init_p, const float relperr=0);

  //! convenience function that sets up the initial G4ErrorFreeTrajState from the previous node's info
  bool predict(const KalmanNode* prev_node);

  //! wrapper to KalmanNode::filter() that does some extra coordinate transforms
  void filter();

  //! reset everything to defaults (zeros), also calls KalmanNode::reset()
  void reset();

  //! transformation matrix to go from curvilinear to local (sensor) coordinates
  Eigen::MatrixXd get_rot_curvilinear_to_local() const {return _rot_curvilinear_to_local;}

  //! return the best of {predicted,filtered,smoothed} covariance matrix, rotated into curvilinear coordinates
  Eigen::MatrixXd get_best_cov_matrix_curvilinear() const;

  Eigen::VectorXd global_vecs_to_state_vec(const Eigen::VectorXd &position,
                                           const Eigen::VectorXd &momentum) const;

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
  virtual void _post_smoothing();
  virtual void _build_jacobians();
  virtual void _build_local_coordinates();

  double _dtheta_ms; ///< scattering width \delta\theta for this layer

  bool _dummy_node;

  bool _use_energy_loss;
  double _energy_loss;

  //@{
  //! these define the transformation from global to local (sensor) coordinates
  Eigen::MatrixXd _rot_to_global;
  Eigen::VectorXd _vector_global_to_local_origin;
  //@}

  Eigen::MatrixXd _F_SC2SD;  ///< Jacobian transformation matrix
  Eigen::MatrixXd _F_SD2SC;  ///< Jacobian transformation matrix

  Eigen::VectorXd _u_axis;  ///< Local u-axis (normal to plane)
  Eigen::VectorXd _v_axis;  ///< Local u-axis (normal to plane)
  Eigen::VectorXd _w_axis;  ///< Local u-axis (normal to plane)

  Eigen::MatrixXd _rot_curvilinear_to_local; ///< full transformation matrix to go from curvilinear to local (sensor) coordinates
};

#endif // __PCA_KALMAN_NODE_H
