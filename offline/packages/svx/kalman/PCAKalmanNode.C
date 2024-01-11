#include "PCAKalmanNode.h"

#include "DCKalmanNode.h"
#include "SvxKalmanNode.h"

#include <TVector3.h>
#include <phool.h>

#include <RKTrackRep.h>
#include <GFException.h>

#include <cmath>
#include <vector>
using namespace std;

#include <Eigen/Geometry>
using Eigen::VectorXd;

double sqr(const double &);
float sqr(const float &);

//-------------------------------------------------------------------
PCAKalmanNode::PCAKalmanNode() :
  KalmanNode::KalmanNode(3),
  _dtheta_ms(0),
  _dummy_node(false),
  _use_energy_loss(false),
  _energy_loss(0),
  _rot_to_global(Eigen::MatrixXd::Zero(3,3)),
  _vector_global_to_local_origin(Eigen::Vector3d::Zero()),
  _F_SC2SD(Eigen::MatrixXd::Zero(5,5)),
  _F_SD2SC(Eigen::MatrixXd::Zero(5,5)),
  _u_axis(Eigen::Vector3d::Zero()),
  _v_axis(Eigen::Vector3d::Zero()),
  _w_axis(Eigen::Vector3d::Zero()),
  _rot_curvilinear_to_local(Eigen::MatrixXd::Zero(5,5))
{
  // transformation matrix for state vector -> measurement vector
  // can't set it until we know the PCA
  _H.setZero();

  // PCA projection always goes towards vertex
  _reverse_direction = true;
}

//-------------------------------------------------------------------
PCAKalmanNode::~PCAKalmanNode()
{}

//-------------------------------------------------------------------
bool PCAKalmanNode::predict(const KalmanNode* prev)
{
  _chisq_prev = prev->get_chi_square();
  Eigen::MatrixXd C_kprev( prev->get_best_cov_matrix_curvilinear() );

  VectorXd old_pos = prev->get_best_pos_global();
  VectorXd old_mom = prev->get_best_mom_global();
  old_mom = -old_mom; // propagation is always towards vertex, opposite of momentum
  if(_verb>1){
    cout << "old pos " << old_pos << endl;
    cout << "old mom " << old_mom << endl;
  }

  double oldp = old_mom.norm();
  if(_use_energy_loss) {
    double newp = oldp + _energy_loss;
    VectorXd new_mom = old_mom * (newp/oldp);
    cout << "old mom after eloss " << new_mom << endl;
    old_mom = new_mom;
  }
  
  // If we know the Jacobian matrix (F) to transform the state vector
  // from one layer to the next, we use the following equations:
  //   x_pred = F*x_kprev;
  //   C_pred = PHGslMatrix::get_ABCt(F, C_kprev, F);
  //   C_pred += _Q;
  
  // Unfortunately we don't in a non-uniform magnetic field, so we
  // need to do some numerical integration.  Geant is better at this
  // than I will ever be, so we'll use it for now.
  
  if(_verb>1) cout << "Propagate Errors" << endl;

  TVector3 pos(old_pos(0), old_pos(1), old_pos(2));
  TVector3 mom(old_mom(0), old_mom(1), old_mom(2));
  int pdg = _is_positive ? -11 : 11; // sigh, electrons are "particles" and positrons are "antiparticles" so the signs are reversed!

  if(_reverse_direction) pdg = -pdg;

  RKTrackRep* rk = new RKTrackRep(pos, mom, pdg);
  TMatrixD cov(5,5);
  for(int i=0; i<5; ++i)
    for(int j=0; j<5; ++j)
      cov(i,j) = C_kprev(i,j);
  rk->SetError(cov);

  double distance = 0.;
  TVector3 vertex(_meas(0), _meas(1), _meas(2));
  TVector3 pca;
  TVector3 pca_dir;
  //TMatrixDSym covPred(5,5);
  try {
    distance = rk->extrapolateToPoint(vertex,pca,pca_dir);
  }
  catch(GFException& e){
    std::cerr << e.what() << endl;
    delete rk;
    return false;
  }

  if(_verb>1) cout << "distance: " << distance << endl;

  _pred_pos << pca(0), pca(1), pca(2);
  _pred_mom << pca_dir(0), pca_dir(1), pca_dir(2);
  _pred_mom *= oldp;

  if(_verb>1) cout << "Propagation finished." << endl;

  //double invp = 1./_pred_mom.norm();

  cov = rk->GetError();
  TMatrixD jac = rk->GetTotalTransfMat();
  delete rk;

  for(int i=0; i<5; ++i) {
    for(int j=0; j<5; ++j) {
      _C_pred(i,j) = cov(i,j);
      _F_curvilinear(i,j) = jac(i,j);
    }
  }

  _build_local_coordinates();
  _build_jacobians();

  Eigen::VectorXd localpos( global_to_local_xyz(_pred_pos) );
  float lpos[3] = {localpos(0), localpos(1), localpos(2)};

  double u = lpos[0];
  double v = lpos[1];
  double w = lpos[2];

  if(_verb>1){
    cout << "u-axis: " << _u_axis << endl;
    cout << "v-axis: " << _v_axis << endl;
    cout << "w-axis: " << _w_axis << endl;

    cout << "u,v,w = (" << u << "," << v << "," << w << ")" << endl;

    cout << "new pos " << _pred_pos << endl;
    cout << "new mom " << _pred_mom << endl;

    cout << "dphi: " << atan2(_pred_mom(1), _pred_mom(0)) - atan2(old_mom(1), old_mom(0)) << endl;
    
    cout << "Fill state vector..." << endl;
  }

  Eigen::VectorXd localmom(global_to_local_mom(_pred_mom));
  double pu = localmom(0);
  double pv = localmom(1);
  double pw = localmom(2);
  
  if(_verb>1)
    cout << "(pu, pv, pw): (" << pu << "," << pv << "," << pw << ")" << endl;

  // fill the state vector
  _x_pred = global_vecs_to_state_vec(_pred_pos, _pred_mom);


  const DCKalmanNode* dc_prev_ptr = dynamic_cast<const DCKalmanNode*>(prev);
  const SvxKalmanNode* svx_prev_ptr = dynamic_cast<const SvxKalmanNode*>(prev);
  if( dc_prev_ptr ){
    _F = _F_SC2SD * _F_curvilinear * dc_prev_ptr->get_F_SD2SC();
  } else if( svx_prev_ptr ){
    _F = _F_SC2SD * _F_curvilinear * svx_prev_ptr->get_F_SD2SC();
  } else {
    cout << "ERROR UNKNOWN PREV_NODE TYPE!!!" << endl;
    return false;
  }
  
  if(_verb>1){
    cout << "Cov from traj state:" << _C_pred << endl;
    cout << "Jacobian transformation matrix:" << _F << endl;
    cout << "SC2SD transformation matrix:" << _F_SC2SD << endl;
  }

  // double m = 5.11e-4;

  // // multiply the constant part by 1/(beta*p) for full Moliere formula
  // double psq = 1./(invp*invp);
  // double invpbetasq = (psq+m*m)/(psq*psq);
  // double ms_sigma_sq = invpbetasq * sqr(_dtheta_ms);
  // _Q(1,1) = ms_sigma_sq; // lambda direction
  // _Q(2,2) = ms_sigma_sq; // phi direction

  // if(_verb>1)
  //   cout << "Particle mass: " << m << endl
  //        << "Scattering sigma (degrees): " << sqrt(ms_sigma_sq)*180./M_PI << endl;
  // if(_verb>1) cout << "Physics Noise Q:" << _Q << endl;

  // Add the process noise (scattering, etc.) matrix to the
  // covariance.  Since it's blurring of phi and theta, it's easier to
  // do in SC coordinates.

  // if(_reverse_direction)
  //_C_pred += _Q;


  // convert the Geant covariance matrix to local coords
  _C_pred = _F_SC2SD * _C_pred * _F_SC2SD.transpose();

  // Now fill residuals
  _r_pred = _meas;
  _r_pred -= state_vec_to_global_xyz(_x_pred);
  _R_pred = _V;
  _R_pred += (_H * _C_pred * _H.transpose());

  // Add some uncertainty on the measurement
  for(unsigned int i=0; i<_nmeas; ++i)
    _V(i,i) = sqr(_meas_sigma(i));

  if(_verb>1) cout << "End prediction step" << endl;
  _predict_done = true;
  return true;
}

//-------------------------------------------------------------------
void PCAKalmanNode::filter()
{
  if(!_dummy_node) {
    KalmanNode::filter();
  } else {
    _C_filt = _C_pred;
    if(_verb>1) cout << "C_filt:" << _C_filt;

    _x_filt = _x_pred;
    if(_verb>1) cout << "x_filt:" << _x_filt;
  }

  _filt_pos = state_vec_to_global_xyz(_x_filt);
  _filt_mom = state_vec_to_global_mom(_x_filt);
  _filter_done = true;

  //_build_jacobians();

  return;
}

//-------------------------------------------------------------------
void PCAKalmanNode::initialize_first_node(const Eigen::VectorXd &init_p, const float relperr)
{
  Eigen::VectorXd local_mom(global_to_local_mom(init_p));

  if(_verb>1) {
    cout << endl << "========================================================================================" << endl;
    cout << "PCAKalmanNode::initialize_first_node() --- global mom:"
         << init_p;
  }

  _filt_pos = _meas;
  _filt_mom = init_p;
  if(_reverse_direction) _filt_mom = -_filt_mom;

  _build_local_coordinates();

  _x_filt = global_vecs_to_state_vec(_filt_pos, _filt_mom);

  double invp = _x_filt(0);
  double invperr = 1.;  // 1/p +- 1 GeV^-1
  if(relperr != 0.)
    invperr = invp - invp/(1+relperr);

  // These should be updated once we have a nice fast-fitter
  _C_filt(0,0) = sqr(invperr);  // 1/p +- 0.5 GeV^-1
  _C_filt(1,1) = sqr(1.); // +- ~0.57 degrees
  _C_filt(2,2) = sqr(1.);
  _C_filt(3,3) = sqr(_meas_sigma(0));
  _C_filt(4,4) = sqr(_meas_sigma(1));

  // change 1/p to q/p
  // if(!_is_positive)
  //   _x_pred(0,0) *= -1.;

  _build_jacobians();
  _filter_done = true;
  return;
}

//-------------------------------------------------------------------
Eigen::MatrixXd PCAKalmanNode::get_best_cov_matrix_curvilinear() const
{
  if(_verb>1)
    cout << "PCAKalmanNode::get_best_cov_matrix_curvilinear() - rot. matrix"
         << _F_SD2SC << endl;
  Eigen::MatrixXd temp(5,5);
  if(_smoothing_done)
    temp = _F_SD2SC * _C_smooth * _F_SD2SC.transpose();
  else if(_filter_done)
    temp = _F_SD2SC * _C_filt * _F_SD2SC.transpose();
  else
    temp = _F_SD2SC * _C_pred * _F_SD2SC.transpose();
  return temp;
}

//-------------------------------------------------------------------
void PCAKalmanNode::reset()
{
  KalmanNode::reset();
  
  _rot_to_global.setZero();
  _vector_global_to_local_origin.setZero();

  _pred_pos = Eigen::Vector3d::Zero();  //! predicted position at this node
  _pred_mom = Eigen::Vector3d::Zero(); //! predicted momentum at this node

  _filt_pos = Eigen::Vector3d::Zero();  //! filtered position at this node
  _filt_mom = Eigen::Vector3d::Zero(); //! filtered momentum at this node

  _smooth_pos = Eigen::Vector3d::Zero();  //! smoothed position at this node
  _smooth_mom = Eigen::Vector3d::Zero(); //! smoothed momentum at this node

  _dtheta_ms = 0.; //! scattering width \delta\theta for this layer
  _is_positive = 0; //! charge

  _F_SC2SD.setZero();
  _F_SD2SC.setZero();

  _rot_curvilinear_to_local.setZero(); //! full transformation matrix to go from curvilinear to local (sensor) coordinates

  return;
}


// //-------------------------------------------------------------------
// void PCAKalmanNode::smooth(const KalmanNode &prev_node)
// {
//   // first do the basic smoothing algorithm
//   KalmanNode::smooth(prev_node);
// }

//-------------------------------------------------------------------
void PCAKalmanNode::_post_smoothing()
{
  _smooth_mom = state_vec_to_global_mom(_x_smooth);
  _smooth_pos = state_vec_to_global_xyz(_x_smooth);

  // if we were propagating in reverse, flip all of the momenta back
  // to the correct direction
  if(_reverse_direction) {
    // // lambda => -lambda
    // _x_smooth(1,0) = -_x_smooth(1,0);
    // _x_filt(1,0) = -_x_filt(2,0);
    // _x_pred(1,0) = -_x_pred(1,0);

    // // phi => phi+pi
    // _x_smooth(2,0) += M_PI;
    // if(_x_smooth(2,0) > M_PI) _x_smooth(2,0) -= 2*M_PI;
    // _x_filt(2,0) += M_PI;
    // if(_x_filt(2,0) > M_PI) _x_filt(2,0) -= 2*M_PI;
    // _x_pred(2,0) += M_PI;
    // if(_x_pred(2,0) > M_PI) _x_pred(2,0) -= 2*M_PI;
    
    _smooth_mom = -_smooth_mom;
    _filt_mom = -_filt_mom;
    _pred_mom = -_pred_mom;
  }

  //_build_jacobians();
  _smoothing_done = true;
  return;
}

//-------------------------------------------------------------------
Eigen::VectorXd PCAKalmanNode::state_vec_to_global_xyz(const Eigen::VectorXd &state) const
{
  Eigen::VectorXd local(3,1);
  local(0) = 0;
  local(1) = state(3);
  local(2) = state(4);
  
  Eigen::VectorXd global(_rot_to_global*local);
  global += _vector_global_to_local_origin;
  return global;
}

//-------------------------------------------------------------------
Eigen::VectorXd PCAKalmanNode::state_vec_to_global_mom(const Eigen::VectorXd &state) const
{
  double p = fabs(1./state(0));
  
  Eigen::VectorXd local(3,1);
  local(0) = p/sqrt(1.+sqr(state(1))+sqr(state(2)));
  if(_reverse_direction) local(0) = -local(0); // this handles the sign ambiguities of the tangents
  local(1) = local(0)*state(1);
  local(2) = local(0)*state(2);

  Eigen::VectorXd global = local_to_global_mom(local);
  return global;
}


//-------------------------------------------------------------------
Eigen::VectorXd PCAKalmanNode::global_vecs_to_state_vec(const Eigen::VectorXd &position,
                                                        const Eigen::VectorXd &momentum) const
{
  Eigen::VectorXd local_pos(global_to_local_xyz(position));
  Eigen::VectorXd local_mom(global_to_local_mom(momentum));
  Eigen::VectorXd state(5);

  double invp = 1./momentum.norm();
  state(0) = invp;
  state(1) = local_mom(1)/local_mom(0);
  state(2) = local_mom(2)/local_mom(0);
  state(3) = local_pos(1);
  state(4) = local_pos(2);
  return state;
}

//-------------------------------------------------------------------
Eigen::VectorXd PCAKalmanNode::global_to_local_xyz(const Eigen::VectorXd &position) const
{
  Eigen::VectorXd global = position - _vector_global_to_local_origin;
  Eigen::VectorXd local(_rot_to_global.transpose()*global);
  return local;
}

//-------------------------------------------------------------------
Eigen::VectorXd PCAKalmanNode::global_to_local_mom(const Eigen::VectorXd &mom) const
{
  Eigen::VectorXd local(_rot_to_global.transpose()*mom);
  return local;
}

//-------------------------------------------------------------------
Eigen::VectorXd PCAKalmanNode::local_to_global_mom(const Eigen::VectorXd &mom) const
{
  Eigen::VectorXd global(_rot_to_global*mom);
  return global;
}

//-------------------------------------------------------------------
void PCAKalmanNode::load_measurement(TVector3 vertex)
{
  // Set the measured cluster position in local coords (x,z)
  Eigen::VectorXd meas_pos(3,1);
  _meas << vertex(0), vertex(1), vertex(2);

  _meas_sigma(0) = 0.;
  _meas_sigma(1) = 0.;
  _meas_sigma(2) = 2.0;

  if(_verb>1) {
    cout << "meas_pos global:" << _meas << endl;
    cout << "meas_pos local sigma:" << _meas_sigma << endl;
  }

  return;
}

//-------------------------------------------------------------------
void PCAKalmanNode::_build_local_coordinates()
{
  // Now set up the transverse plane at the PCA

  VectorXd pos(get_best_pos_global());
  VectorXd dir(get_best_mom_global());
  dir /= dir.norm();

  TVector3 plane_norm( dir(0), dir(1), dir(2));
  TVector3 plane_point(pos(0), pos(1), pos(2));

  if(_reverse_direction)
    plane_norm *= -1.; // I want orthogonal axis to be away from vertex

  if(_verb>1) {
    cout << "plane norm: (" << plane_norm(0) << ", " << plane_norm(1) << ", " << plane_norm(2) << ")" << endl;
    cout << "plane point: " << plane_point(0) << ", " << plane_point(1) << ", " << plane_point(2) << ")" << endl;
  }

  _vector_global_to_local_origin(0) = plane_point(0);
  _vector_global_to_local_origin(1) = plane_point(1);
  _vector_global_to_local_origin(2) = plane_point(2);

  Eigen::Vector3d x_axis; x_axis << 1,0,0;
  Eigen::Vector3d z_axis; z_axis << 0,0,1;
  Eigen::Vector3d u_axis = _pred_mom / _pred_mom.norm();
  Eigen::Vector3d w_axis;

  if(u_axis != x_axis)
    w_axis = x_axis.cross(u_axis);
  else
    w_axis = z_axis;

  Eigen::Vector3d v_axis = w_axis.cross(u_axis);
  
  _rot_to_global.col(0) = u_axis;
  _rot_to_global.col(1) = v_axis;
  _rot_to_global.col(2) = w_axis;
  
  if(_verb>1) {
    cout << "_vector_global_to_local_origin:" << _vector_global_to_local_origin << endl;
    cout << "_rot_to_global:" << _rot_to_global << endl;
  }

  // Set up the local u,v,w axes
  Eigen::VectorXd localu(3,1); localu << 1,0,0;
  Eigen::VectorXd localv(3,1); localv << 0,1,0;
  Eigen::VectorXd localw(3,1); localw << 0,0,1;
  Eigen::VectorXd globalu(_rot_to_global*localu);
  Eigen::VectorXd globalv(_rot_to_global*localv);
  Eigen::VectorXd globalw(_rot_to_global*localw);
  _u_axis = globalu;
  _v_axis = globalv;
  _w_axis = globalw;
  
  // print some rotation-debugging stuff
  if( _verb > 1 ){
    cout << "my x-axis: (" << globalu(0) << "," << globalu(1) << ","
         << globalu(2) << ")" << endl;
    cout << "my y-axis: (" << globalv(0) << "," << globalv(1) << ","
         << globalv(2) << ")" << endl;
    cout << "my z-axis: (" << globalw(0) << "," << globalw(1) << ","
         << globalw(2) << ")" << endl;
  }

  // [0, 0, 0, v_x, w_x]
  // [0, 0, 0, v_y, w_y]
  // [0, 0, 0, v_z, w_z]

  _H(0,3) = _rot_to_global(0,1);
  _H(0,4) = _rot_to_global(0,2);
  _H(1,3) = _rot_to_global(1,1);
  _H(1,4) = _rot_to_global(1,2);
  _H(2,3) = _rot_to_global(2,1);
  _H(2,4) = _rot_to_global(2,2);
  
  return;
}


//-------------------------------------------------------------------
void PCAKalmanNode::_build_jacobians()
{
  _F_SC2SD.setZero();
  _F_SD2SC.setZero();
  
  VectorXd position = get_best_pos_global();
  VectorXd momentum = get_best_mom_global();
  
  // make unit vectors (T,U,V) defining the SC (curvilinear) coordinate system
  double mom = momentum.norm();
  Eigen::Vector3d Tvec = momentum/mom;
  Eigen::Vector3d Uvec;
  Uvec << -Tvec.y(), Tvec.x(), 0.;
  Uvec /= Uvec.norm();
  Eigen::Vector3d Vvec = Tvec.cross( Uvec );

  // make unit vectors (I,J,K) defining the SD (detector plane) coordinate system
  Eigen::Vector3d Ivec(_u_axis);
  Eigen::Vector3d Jvec(_v_axis);
  Eigen::Vector3d Kvec(_w_axis);

  if(_verb>1) {
    cout << "T: " << Tvec << endl;
    cout << "U: " << Uvec << endl;
    cout << "V: " << Vvec << endl;
    cout << "I: " << Ivec << endl;
    cout << "J: " << Jvec << endl;
    cout << "K: " << Kvec << endl;
  }

  // get the dot products of vectors
  double UdotI = Uvec.dot(Ivec);
  double UdotJ = Uvec.dot(Jvec);
  double UdotK = Uvec.dot(Kvec);
  double VdotI = Vvec.dot(Ivec);
  double VdotJ = Vvec.dot(Jvec);
  double VdotK = Vvec.dot(Kvec);
  double TdotI = Tvec.dot(Ivec);
  double TdotJ = Tvec.dot(Jvec);
  double TdotK = Tvec.dot(Kvec);

  double invTdotI = 1./TdotI;
  double invTdotI2 = sqr(invTdotI);
  double invTdotI3 = invTdotI*invTdotI2;
  

  double CosLambda = std::sin( theta(momentum) );
  double InvCosLambda = 1./CosLambda;
  double charge = _is_positive ? 1. : -1.;

  //--- Get magnetic field
#ifdef USEG4  
  const G4Field* field = G4TransportationManager::GetTransportationManager()->GetFieldManager()->GetDetectorField();
  if( charge != 0 && field ) {
    double pos1[4] = {position.x()*cm, position.y()*cm, position.z()*cm, 0};
    double h1[6];
    field->GetFieldValue( pos1, h1 );
    Eigen::Vector3d B;
    B << h1[0], h1[1], h1[2];
    B /= tesla;
    double Bmag = B.norm();

#else
  if( charge != 0 ) {
    TVector3 Bpos(position(0), position(1), position(2));
    TVector3 Btemp = RKTrackRep::getFieldValTesla(Bpos);

    Eigen::Vector3d B;
    B << Btemp(0), Btemp(1), Btemp(2);
    double Bmag = B.norm();
#endif
    
    if( Bmag != 0. ) {
      Eigen::Vector3d H = B/Bmag;

      double invP = 1./mom;
      double Q = -0.003*Bmag*charge*invP;
      Eigen::Vector3d Nvec = H.cross(Tvec);
      double alpha = Nvec.norm();
      Nvec /= alpha;
      double UdotN = Uvec.dot(Nvec);
      double VdotN = Vvec.dot(Nvec);

      _F_SD2SC(1,3) = -alpha*Q*TdotJ*VdotN;
      _F_SD2SC(1,4) = -alpha*Q*TdotK*VdotN;
      _F_SD2SC(2,3) = -alpha*Q*TdotJ*UdotN*InvCosLambda;
      _F_SD2SC(2,4) = -alpha*Q*TdotK*UdotN*InvCosLambda;

      double RdotK = (VdotK)*(UdotN) - (UdotK)*(VdotN);
      double RdotJ = (VdotJ)*(UdotN) - (UdotJ)*(VdotN);
            
      _F_SC2SD(1,3) = -alpha*Q*UdotI*RdotK*invTdotI3;
      _F_SC2SD(1,4) = -alpha*Q*VdotI*RdotK*invTdotI3;
      _F_SC2SD(2,3) =  alpha*Q*UdotI*RdotJ*invTdotI3;
      _F_SC2SD(2,4) =  alpha*Q*VdotI*RdotJ*invTdotI3;
    }
  }
  
  _F_SD2SC(0,0) = 1.;
  _F_SD2SC(1,1) = TdotI*VdotJ;
  _F_SD2SC(1,2) = TdotI*VdotK;
  _F_SD2SC(2,1) = TdotI*UdotJ*InvCosLambda;
  _F_SD2SC(2,2) = TdotI*UdotK*InvCosLambda;
  _F_SD2SC(3,3) = UdotJ;
  _F_SD2SC(3,4) = UdotK;
  _F_SD2SC(4,3) = VdotJ;
  _F_SD2SC(4,4) = VdotK;

  _F_SC2SD(0,0) = 1.;
  _F_SC2SD(1,1) = -UdotK*invTdotI2;
  _F_SC2SD(1,2) = CosLambda*VdotK*invTdotI2;
  _F_SC2SD(2,1) = UdotJ*invTdotI2;
  _F_SC2SD(2,2) = -CosLambda*VdotJ*invTdotI2;
  _F_SC2SD(3,3) = VdotK*invTdotI;
  _F_SC2SD(3,4) = -UdotK*invTdotI;
  _F_SC2SD(4,3) = -VdotJ*invTdotI;
  _F_SC2SD(4,4) = UdotJ*invTdotI;

  if(_verb>1) {
    cout << "PCAKalmanNode::_build_jacobians() - F_SD2SC:" << _F_SD2SC << endl;
    cout << "PCAKalmanNode::_build_jacobians() - F_SC2SD:" << _F_SC2SD << endl;
  }
  return;
}
