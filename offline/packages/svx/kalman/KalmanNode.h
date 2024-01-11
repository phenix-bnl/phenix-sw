//-------------------------------------------------------------------
// KalmanNode.h
// Author: Matt Wysocki, July 1, 2011
//
/// \class KalmanNode
/// Generic Kalman filter node
/// 
/// This base class represents one step ("node") in the Kalman Filter
/// routine, and contains the basic linear algebra of the Kalman
/// filter.  Detector-specific inherited classes will provide the
/// non-generic information, such as the number of state variables and
/// measurement variables (which determine the matrix dimensions), as
/// well as the measurement uncertainties, coordinate transformations,
/// and actual measurements themselves.
///
/// For details on the Kalman filter algorithm, see (for starters):
/// R. Fruhwirth, Nucl. Inst. and Meth. A, 262 (1987) 444-450
/// "Application of Kalman filtering to track and vertex fitting", 
/// DOI: 10.1016/0168-9002(87)90887-4
///
/// I have tried to keep the equations and variable notation the same
/// as this publication (it varies quite a bit from paper to paper).
//-------------------------------------------------------------------

#ifndef __KALMAN_NODE_H
#define __KALMAN_NODE_H

#ifdef USEG4
class G4ErrorTrajState;
#endif

#define EIGEN_INITIALIZE_MATRICES_BY_ZERO 1
#undef EIGEN_DEFAULT_IO_FORMAT
#define EIGEN_DEFAULT_IO_FORMAT IOFormat(4, 0, ", ", "\n", " [", "]", "\n", "\n")

#include <Eigen/Core>
#include <Eigen/LU>

#include <cmath>
#include <vector>

class KalmanNode
{
 public:
  KalmanNode(int nmeas);

  typedef Eigen::VectorXd StateVector;
  typedef Eigen::MatrixXd StateMatrix;
  typedef Eigen::VectorXd MeasVector;
  typedef Eigen::MatrixXd MeasMatrix;

  void set_verbosity(int v) {_verb=v;}
  void set_use_gain_formalism(bool b) {_gain_formalism=b;}
  void set_reverse_direction(bool b) {_reverse_direction=b;}

  /// initialize the first node as having been filtered
  virtual void initialize_first_node(const Eigen::VectorXd &init_p, const float relperr=0) = 0;

  // Since propagating through the magnetic field needs to know about
  // the actual detector elements, we'll force detector-specific
  // subclasses to define the function
  virtual bool predict(const KalmanNode* prev_node) = 0;
#ifdef USEG4
  virtual bool predict(G4ErrorTrajState*) = 0;
#endif

  /// filtering step
  virtual void filter();

  /// smoothing step (first node, no previous node)
  virtual void smooth();

  /// smoothing step
  virtual void smooth(const KalmanNode* prev_node);

  /// smoothing step using a different propagation Jacobian F
  virtual void smooth(const KalmanNode* prev_node, const StateMatrix F);

  /// reset the node to it's default state
  virtual void reset();

  ///@{ @name the "best" position, momentum, or covariance matrix (whatever was run last: predicted, filtered, or smoothed)

  /// "best" state vector
  virtual StateVector get_best_x() const;
  /// "best" covariance matrix
  virtual StateMatrix get_best_C() const;
  /// "best" fit position vector
  virtual Eigen::VectorXd get_best_pos_global() const;
  /// "best" fit momentum vector
  virtual Eigen::VectorXd get_best_mom_global() const;
  /// "best" covariance matrix
  virtual StateMatrix get_best_cov_matrix() const;
  /// "best" covariance matrix transformed to curvilinear coordinates
  virtual StateMatrix get_best_cov_matrix_curvilinear() const;
  ///@}

  /// predicted position in global coordinates
  virtual Eigen::VectorXd get_predicted_pos_global() const {return _pred_pos;}
  /// predicted momentum in global coordinates
  virtual Eigen::VectorXd get_predicted_mom_global() const {return _pred_mom;}

  /// filtered position in global coordinates
  virtual Eigen::VectorXd get_filtered_pos_global() const {return _filt_pos;}
  /// filtered momentum in global coordinates
  virtual Eigen::VectorXd get_filtered_mom_global() const {return _filt_mom;}

  /// smoothed position in global coordinates
  virtual Eigen::VectorXd get_smoothed_pos_global() const {return _smooth_pos;}
  /// smoothed momentum in global coordinates
  virtual Eigen::VectorXd get_smoothed_mom_global() const {return _smooth_mom;}

  ///@{ @name Accessors for predicted state
  virtual StateVector get_x_pred() const {return _x_pred;}
  virtual StateMatrix get_C_pred() const {return _C_pred;}
  virtual StateMatrix get_C_pred_inv() const;
  ///@}

  ///@{ @name Accessors for filtered state
  virtual StateVector get_x_filt() const {return _x_filt;}
  virtual StateMatrix get_C_filt() const {return _C_filt;}
  virtual MeasVector get_r_filt() const {return _r_filt;}
  virtual MeasMatrix get_R_filt() const {return _R_filt;}
  ///@}

  ///@{ @name Accessors for smoothed state
  virtual StateVector get_x_smooth() const {return _x_smooth;}
  virtual StateMatrix get_C_smooth() const {return _C_smooth;}
  virtual MeasVector get_r_smooth() const {return _r_smooth;}
  virtual MeasMatrix get_R_smooth() const {return _R_smooth;}
  ///@}

  virtual MeasVector get_meas() const {return _meas;}
  virtual MeasVector get_meas_sigma() const {return _meas_sigma;}
  virtual void set_meas(const MeasVector &m) {_meas = m;}
  virtual void set_meas_sigma(const MeasVector &m) {_meas_sigma = m;}

  virtual StateMatrix get_F() const {return _F;}
  virtual StateMatrix get_F_curvilinear() const {return _F_curvilinear;}
  virtual const StateMatrix get_Q() const {return _Q;}

  double get_chi_square_increment() const {return _chisq_inc;}
  double get_chi_square() const {return _chisq;}

  virtual void initialize_from_state(const Eigen::VectorXd &state);

#ifdef USEG4
  virtual G4ErrorTrajState* get_traj_state() const {return 0;}
#endif

  /// set the charge of the particle
  void set_is_positive(bool b) {_is_positive = b;}

  /// get the charge of the particle
  bool is_positive() {return _is_positive;}

  ///@{ @name Convert position vector between global and local coordinates
  virtual Eigen::VectorXd state_vec_to_global_xyz(const Eigen::VectorXd &state) const = 0;
  virtual Eigen::VectorXd global_to_local_xyz(const Eigen::VectorXd &position) const = 0;
  ///@}

  ///@{ @name Convert momentum vector between global and local coordinates
  virtual Eigen::VectorXd state_vec_to_global_mom(const Eigen::VectorXd &) const = 0;
  virtual Eigen::VectorXd global_to_local_mom(const Eigen::VectorXd &) const = 0;
  virtual Eigen::VectorXd local_to_global_mom(const Eigen::VectorXd &) const = 0;
  ///@}

 protected:
  virtual void _post_smoothing() = 0; //{std::cout << "Using pure virtual function!!!" << std::endl;}

  Eigen::VectorXd _pred_pos;  ///< predicted position at this node
  Eigen::VectorXd _pred_mom; ///< predicted momentum at this node

  Eigen::VectorXd _filt_pos;  ///< filtered position at this node
  Eigen::VectorXd _filt_mom; ///< filtered momentum at this node

  Eigen::VectorXd _smooth_pos;  ///< smoothed position at this node
  Eigen::VectorXd _smooth_mom; ///< smoothed momentum at this node

  StateVector _x_pred; ///< predicted state vector
  StateMatrix _C_pred; ///< predicted covariance matrix
  MeasVector _r_pred; ///< predicted residuals vector
  MeasMatrix _R_pred; ///< predicted residuals covariance matrix

  StateVector _x_filt; ///< filtered state vector
  StateMatrix _C_filt; ///< filtered covariance matrix
  MeasVector _r_filt; ///< filtered residuals vector
  MeasMatrix _R_filt; ///< filtered residuals covariance matrix
  double _chisq_inc;   ///< chi^2 increment of this node
  double _chisq;       ///< total chi^2 including this node

  StateVector _x_smooth; ///< smoothed state vector
  StateMatrix _C_smooth; ///< smoothed covariance matrix
  MeasVector _r_smooth; ///< smoothed residuals vector
  MeasMatrix _R_smooth; ///< smoothed residuals covariance matrix

  MeasVector _meas;       ///< vector of measurements
  MeasVector _meas_sigma; ///< vector of measurement uncertainties
  StateVector _x_truth;    ///< truth info (for MC)

  StateMatrix _F;  ///< Jacobian transformation matrix
  StateMatrix _F_curvilinear;  ///< Jacobian transformation matrix

  Eigen::MatrixXd _H;  ///< state -> measurement transformation matrix
  MeasMatrix _V;  ///< measurement variance (uncertainty covariance matrix)
  StateMatrix _Q;  ///< process noise variance (covariance matrix)
  StateMatrix _I;  ///< store the identity matrix since we use it many times
  double _chisq_prev; ///< chi^2 of the previous node (from filter step)

  bool _gain_formalism; ///< do we want to use the filtering equations in the gain formalism or weighted means formalism?
  bool _reverse_direction; ///< we're propagating backwards, so we need to flip the momentum vector at the end

  bool _predict_done;   ///< has prediction been done
  bool _filter_done;    ///< has filtering been done
  bool _smoothing_done; ///< has smoothing been done

  int _verb; ///< the verbosity
  unsigned int _nstate; ///< number of state variables
  unsigned int _nmeas; ///< number of measured variables

  bool _is_positive; ///< charge

  mutable StateMatrix _C_pred_inv; ///< inverse predicted covariance matrix (cached for smoothing)

  double theta(const Eigen::VectorXd &v) const; ///< calculate theta angle of a vector v
};

#endif // __KALMAN_NODE_H
