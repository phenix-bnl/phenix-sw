
#include "KalmanNode.h"
#include <phool.h>
using namespace std;
using Eigen::VectorXd;
using Eigen::MatrixXd;

double sqr(const double &x) {return x*x;}
float sqr(const float &x) {return x*x;}

//-------------------------------------------------------------------
KalmanNode::KalmanNode(int nmeas) :
  _pred_pos(3,1),
  _pred_mom(3,1),
  _filt_pos(3,1),
  _filt_mom(3,1),
  _smooth_pos(3,1),
  _smooth_mom(3,1),
  _x_pred(5,1),
  _C_pred(5,5),
  // _r_pred(nmeas,1),
  // _R_pred(nmeas,nmeas),
  _x_filt(5,1),
  _C_filt(5,5),
  // _r_filt(nmeas,1),
  // _R_filt(nmeas,nmeas),
  _chisq_inc(0),
  _chisq(0),
  _x_smooth(5,1),
  _C_smooth(5,5),
  // _r_smooth(nmeas,1),
  // _R_smooth(nmeas,nmeas),
  // _meas(nmeas,1),
  // _meas_sigma(nmeas,1),
  _x_truth(5,1),
  _F(5,5),
  _F_curvilinear(5,5),
  // _H(nmeas,5),
  // _V(nmeas,nmeas),
  _Q(5,5),
  _I(5,5),
  _chisq_prev(0),
  _gain_formalism(true),
  _reverse_direction(false),
  _predict_done(false),
  _filter_done(false),
  _smoothing_done(false),
  _verb(0),
  _nstate(5),
  _nmeas(nmeas),
  _is_positive(0),
  _C_pred_inv(5,5)
{
  _r_pred.resize(_nmeas);
  _R_pred.resize(_nmeas,_nmeas);
  _r_filt.resize(_nmeas);
  _R_filt.resize(_nmeas,_nmeas);
  _r_smooth.resize(_nmeas);
  _R_smooth.resize(_nmeas,_nmeas);
  _meas.resize(_nmeas);
  _meas_sigma.resize(_nmeas);
  _H.resize(_nmeas,5);
  _V.resize(_nmeas,_nmeas);

  _I.setIdentity();

  reset();
}


//-------------------------------------------------------------------
// The filter step (sometimes called correction step) combines the
// prediction (which is based on all previous nodes) with the
// measurement at this node
//
void KalmanNode::filter()
{
  if(_verb>1) cout << "KalmanNode::filter()" << endl;

  if( _C_pred == MatrixXd::Zero(5,5) ){
    cout << PHWHERE << "Uh oh, somehow you got a covariance matrix = 0!" << endl;
    return;
  }
  
//   Eigen::Matrix<double, 5, 5> C_predE;
//   for(int i=0; i<5; i++)
//     for(int j=0; j<5; j++)
//       C_predE(i,j) = C_pred(i,j);

//   Eigen::Matrix<double, 5, 5> C_predE_inv = C_predE.inverse();
//   cout << "C_pred_inv:" << C_pred_inv << endl;
//   cout << "C_predE_inv:" << C_predE_inv << endl;

//   for(int i=0; i<5; i++)
//     for(int j=0; j<5; j++)
//       C_pred_inv(i,j) = C_predE_inv(i,j);
  
  if(_gain_formalism) {
    // C_k = (I-K*H) * C_pred
//     cout << PHWHERE << endl;
//     PHGslMatrix K = _H.transpose(); // just to get the dimensions
//     K.zero();
    
//     cout << "C_pred: " << C_pred << endl;
//     cout << "_H: " << _H << endl;
//     if( !C_pred.is_zero() ){
//       PHGslMatrix denom = PHGslMatrix::get_ABCt(_H, C_pred, _H);
//       denom += _V;
//     cout << "C_pred:" << C_pred << endl;
//     cout << "_H:" << _H << endl;
//     cout << "denom:" << denom << endl;
//       K = PHGslMatrix::get_ABtCinv(C_pred, _H, denom);
      
//     cout << "K: " << K << endl;
//       PHGslMatrix C_temp = _I;
//       C_temp -= (K*_H);
//       C_filt = C_temp * C_pred;
//     }
    
//     //x_k = x_pred + K*(m_k - H*x_pred);
//     PHGslMatrix x_temp = meas;
//     cout << "meas: " << _meas << endl;
//     cout << "_H" << _H << endl;
//     cout << "x_pred: " << x_pred << endl;
//     x_temp -= (_H*x_pred);
//     cout << "x_temp: " << x_temp << endl;
//     x_filt = K*x_temp;
//     cout << "x_filt: " << x_filt << endl;
//     x_filt += x_pred;
//     cout << "x_filt: " << x_filt << endl;

    // C_k = (I-K*H) * C_pred * (I-K*H)^T + K*V*K^T
    MatrixXd K = _H.transpose(); // just to get the dimensions
    K.setZero();

    if( _C_pred == MatrixXd::Zero(5,5) ){
      cout << PHWHERE
           << "Uh oh, somehow you got a covariance matrix = 0!" << endl;
      return;
    }
    
    if(_verb>1) cout << "C_pred: " << _C_pred << "x_pred:" << _x_pred << "m_k:" << _meas;

    // calculate the Kalman gain matrix K
    MeasMatrix denom = _H*_C_pred*_H.transpose();
    if(_verb>1) cout << "H C_pred H^T: " << denom;
    if(_verb>1) cout << "V: " << _V;
    denom += _V;

    if(_verb>1) cout << "denom^-1: " << denom.inverse() << endl;
    K = _C_pred * _H.transpose() * denom.inverse();
    if(_verb>1) cout << "K: " << K;

    // calculate the covariance matrix
    StateMatrix C_temp = _I;
    C_temp -= (K*_H);
    _C_filt = C_temp * _C_pred * C_temp.transpose();
    _C_filt += K*_V*K.transpose();
    if(_verb>1) cout << "C_filt:" << _C_filt;

    // now the state vector...
    //x_k = x_pred + K*(m_k - H*x_pred);
    MeasVector x_temp = _meas;
    x_temp -= (_H*_x_pred);
    if(_verb>1) cout << "m_k - H*x_pred:" << x_temp;
    _x_filt = K*x_temp;
    _x_filt += _x_pred;
    if(_verb>1) cout << "x_filt:" << _x_filt;

    // calculate the residuals
    MeasMatrix C_temp2(_nmeas,_nmeas);
    C_temp2.setIdentity();
    C_temp2 -= (_H*K);
    _r_filt = C_temp2*_r_pred;
    _R_filt = C_temp2*_V;
    if(_verb>1) {
      cout << "I-HK:" << C_temp2;
      cout << "r_filt:" << _r_filt;
      cout << "R_filt:" << _R_filt;
    }    
  } else {  // weighted means formalism (untested as of 4/19/2011)

    MeasMatrix G = _V.inverse();
    if(_verb>1) cout << "G: " << G << endl;

    // C_k = [C_pred^-1 + H^T*G*H]^-1
    StateMatrix C_inv = _H.transpose() * G * _H;
    C_inv += get_C_pred_inv();
    _C_filt = C_inv.inverse();

    // Now the state vector...
    // x_k = C_k*[C_pred^-1*x_pred + H^T*G*m_k]
    StateVector x_temp = _H.transpose() * G * _meas;
    x_temp += (get_C_pred_inv()*_x_pred);
    _x_filt = _C_filt*x_temp;

    // calculate the residuals
    _r_filt = _meas;
    _r_filt -= _H*_x_filt;
    _R_filt = _V;
    _R_filt -= _H * _C_filt * _H.transpose();
  }

  // MGW - this will only work for 2x2 matrices, and should be fixed
  if(_nmeas==2) {
    float determinant = _R_filt(0,0)*_R_filt(1,1) - _R_filt(0,1)*_R_filt(1,0);
    if(determinant==0.) {
      cout << PHWHERE << "Residual covariance matrix is not invertible, something went wrong!" << endl;
      _chisq_inc = 1.e8;
      _chisq = (_chisq_prev + _chisq_inc);
      return;
    }
  }

  // the chi^2 calculation is the same for both methods
  MatrixXd chisq_tmp = _r_filt.transpose() * _R_filt.inverse() * _r_filt;
  _chisq_inc = chisq_tmp(0,0);
  _chisq = (_chisq_prev + _chisq_inc);

  if(_verb>1) {
    cout << "chi^2 increment: " << _chisq_inc << endl;
    cout << "chi^2 total:     " << _chisq << endl;
    cout << "x_filt: " << _x_filt << endl;
    cout << "C_filt: " << _C_filt << endl;
  }

  _filter_done = true;
  return;
}

//-------------------------------------------------------------------
// The smoothing part goes back and improves previous nodes now that
// we have knowledge based on the entire set of measurements.  This
// isn't needed if all we want is the state vector and uncertainties
// at the last node.
//
void KalmanNode::smooth()
{
  _x_smooth = _x_filt;
  _C_smooth = _C_filt;
  
  if(_verb>1) cout << "x_smooth: " << _x_smooth << endl;
  if(_verb>1) cout << "C_smooth: " << _C_smooth << endl;

  // call the virtual post_smoothing function of the inheriting class
  this->_post_smoothing();
  _smoothing_done = true;
  return;
}

//-------------------------------------------------------------------
void KalmanNode::smooth(const KalmanNode* prev_node)
{
  StateMatrix F = prev_node->get_F();
  smooth(prev_node, F);
  return;
}

//-------------------------------------------------------------------
void KalmanNode::smooth(const KalmanNode* prev_node, const StateMatrix F)
{
  // Smoother gain matrix:
  StateMatrix A = _C_filt * F.transpose() * prev_node->get_C_pred_inv();

  if(_verb>1) cout << "x_filt: " << _x_filt << endl;
  if(_verb>1) cout << "Smoother gain matrix A: " << A << endl;

  // x_k^n = x_k + A*(x_(k+1)^n - x_(k+1)^k)
  StateVector xdiff = prev_node->get_x_smooth();
  xdiff -= prev_node->get_x_pred();
  if(_verb>2) cout << "xdiff: " << xdiff << endl;
  _x_smooth = A*xdiff;
  if(_verb>2) cout << "A*xdiff: " << _x_smooth << endl;
  _x_smooth += _x_filt;

  if(_verb>1) cout << "x_smooth: " << _x_smooth << endl;
  
  // C_k^n = C_k + A*(C_(k+1)^n - C_(k+1)^k)*A^T
  StateMatrix Cdiff = prev_node->get_C_smooth();
  Cdiff -= prev_node->get_C_pred();
  _C_smooth = _C_filt;
  _C_smooth += A * Cdiff * A.transpose();
  
  if(_verb>1) cout << "C_smooth: " << _C_smooth << endl;

  // r_k^n = m_k - H_k * x_k^n
  _r_smooth = _meas;
  _r_smooth -= _H*_x_smooth;
  if(_verb>1) cout << "r_smooth: " << _r_smooth << endl;

  // R_k^n = V_k - H_k C_k^n H_k^T
  _R_smooth = _V;
  _R_smooth -= _H * _C_smooth * _H.transpose();
  if(_verb>1) cout << "R_smooth: " << _R_smooth << endl;

  // call the virtual post_smoothing function of the inheriting class
  this->_post_smoothing();

  _smoothing_done = true;
  return;
}


//-------------------------------------------------------------------
// Zero out all the matrices and reset state
//
void KalmanNode::reset()
{
  _pred_pos.setZero();
  _pred_mom.setZero();
  _filt_pos.setZero();
  _filt_mom.setZero();
  _smooth_pos.setZero();
  _smooth_mom.setZero();

  _x_pred.setZero();
  _C_pred.setZero();
  _r_pred.setZero();
  _R_pred.setZero();
  _C_pred_inv.setZero();

  _x_filt.setZero();
  _C_filt.setZero();
  _r_filt.setZero();
  _R_filt.setZero();
  _chisq_inc = 0;
  _chisq = 0;

  _x_smooth.setZero();
  _C_smooth.setZero();
  _r_smooth.setZero();
  _R_smooth.setZero();

  _meas.setZero();
  _meas_sigma.setZero();
  _x_truth.setZero();

  _H.setZero();
  _F.setZero();
  _F_curvilinear.setZero();
  _V.setZero();
  _Q.setZero();
  _chisq_prev = 0;

  _predict_done = false;
  _filter_done = false;
  _smoothing_done = false;
  return;
}

//-------------------------------------------------------------------
VectorXd KalmanNode::get_best_x() const
{
  if(_smoothing_done) return _x_smooth;
  else if(_filter_done) return _x_filt;
  else return _x_pred;
}

//-------------------------------------------------------------------
MatrixXd KalmanNode::get_best_C() const
{
  return get_best_cov_matrix();
}

//-------------------------------------------------------------------
VectorXd KalmanNode::get_best_pos_global() const
{
  if(_smoothing_done) return _smooth_pos;
  else if(_filter_done) return _filt_pos;
  else return _pred_pos;
}

//-------------------------------------------------------------------
VectorXd KalmanNode::get_best_mom_global() const
{
  if(_smoothing_done) return _smooth_mom;
  else if(_filter_done) return _filt_mom;
  else return _pred_mom;  
}

//-------------------------------------------------------------------
MatrixXd KalmanNode::get_best_cov_matrix() const
{
  if(_smoothing_done) return _C_smooth;
  else if(_filter_done) return _C_filt;
  else return _C_pred;
}

//-------------------------------------------------------------------
MatrixXd KalmanNode::get_best_cov_matrix_curvilinear() const
{
  if(_smoothing_done) return _C_smooth;
  else if(_filter_done) return _C_filt;
  else return _C_pred;
}

//-------------------------------------------------------------------
MatrixXd KalmanNode::get_C_pred_inv() const
{
  if(_C_pred_inv == MatrixXd::Zero(5,5) && _C_pred != MatrixXd::Zero(5,5)) {
    // Calculate the covariance matrix...
    _C_pred_inv = _C_pred.inverse();
  }
  return _C_pred_inv;
}


//-------------------------------------------------------------------
double KalmanNode::theta(const VectorXd &v) const
{
  //return the polar angle
  return (v.x() == 0.0 && v.y() == 0.0 && v.z() == 0.0) ? 0.0 : atan2(sqrt(v.x()*v.x()+v.y()*v.y()), v.z());
}

//-------------------------------------------------------------------
void KalmanNode::initialize_from_state(const Eigen::VectorXd &state)
{
  _x_pred = state;
  _C_pred.setZero();
  _r_pred.setZero();
  _R_pred.setZero();

  _pred_mom = state_vec_to_global_mom(state);
  return;
}
