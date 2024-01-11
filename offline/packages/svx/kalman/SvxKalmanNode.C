#include "SvxKalmanNode.h"
#include "DCKalmanNode.h"

// ROOT includes
#include <TVector3.h>
#include <TH2.h>
#include <TGraph.h>
#include <TFile.h>
#include <TEllipse.h>
#include <TRandom.h>
#include <TParameter.h>

// PHENIX includes
#include <phool.h>
#include <SvxSensor.h>
#include <SvxCluster.h>
#include <SvxClusterInfo.h>

// GenFit classes
#include <RKTrackRep.h>
#include <GFException.h>

// standard library
#include <cmath>
#include <vector>
using namespace std;

// Eigen linear algebra
#include <Eigen/Geometry>
using Eigen::VectorXd;

double sqr(const double &);
float sqr(const float &);

//-------------------------------------------------------------------
SvxKalmanNode::SvxKalmanNode() :
  KalmanNode::KalmanNode(2),
  _sensor(0),
  _X0(0),
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
  _H.setZero();
  _H(0,3) = 1.;
  _H(1,4) = 1.;
}

//-------------------------------------------------------------------
SvxKalmanNode::~SvxKalmanNode()
{}

//-------------------------------------------------------------------
bool SvxKalmanNode::predict(const KalmanNode* prev)
{
  _chisq_prev = prev->get_chi_square();
  
  // Just get the trajectory from the previous node (makes looping
  // over nodes easy)

  Eigen::MatrixXd C_kprev( prev->get_best_cov_matrix_curvilinear() );

  if(!_reverse_direction)
    C_kprev += prev->get_Q();
  else
    C_kprev += _Q;


  // Add some uncertainty on the measurement
  for(unsigned int i=0; i<_nmeas; ++i)
    _V(i,i) = sqr(_meas_sigma(i));

  VectorXd old_pos = prev->get_best_pos_global();
  VectorXd old_mom = prev->get_best_mom_global();
  if(_verb>1){
    cout << "old pos " << old_pos << endl;
    cout << "old mom " << old_mom << endl;
  }

  if(_use_energy_loss) {
    double oldp = old_mom.norm();
    double newp = oldp + _energy_loss;
    VectorXd new_mom = old_mom * (newp/oldp);
    old_mom = new_mom;
    if(_verb>1) cout << "applying energy loss, deltaE = " << _energy_loss << endl
                     << "old mom after eloss " << new_mom << endl;
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

  TVector3 tvec_v(_v_axis(0), _v_axis(1), _v_axis(2));
  TVector3 tvec_w(_w_axis(0), _w_axis(1), _w_axis(2));
  TVector3 tvec_offset(_vector_global_to_local_origin(0),
                       _vector_global_to_local_origin(1),
                       _vector_global_to_local_origin(2));
  GFDetPlane d(tvec_offset, tvec_v, tvec_w);

  double distance = 0.;
  TVectorD statePred(5);
  TMatrixDSym covPred(5,5);
  try {
    distance = rk->extrapolate(d,statePred,covPred);
  }
  catch(GFException& e){
    std::cerr << e.what() << endl;
    delete rk;
    return false;
  }

  rk->setData(statePred,d,&covPred);
  if(_verb>1) cout << "distance: " << distance << endl;

  pos = rk->getPos();
  mom = rk->getMom();
  _pred_pos << pos(0), pos(1), pos(2);
  _pred_mom << mom(0), mom(1), mom(2);

  if(_verb>1) cout << "Propagation finished." << endl;

  // get the position and momentum (global coords) and convert to cm and GeV
  double invp = 1./_pred_mom.norm();

  cov = rk->GetError();
  TMatrixD jac = rk->GetTotalTransfMat();

  for(int i=0; i<5; ++i) {
    for(int j=0; j<5; ++j) {
      _C_pred(i,j) = cov(i,j);
      _F_curvilinear(i,j) = jac(i,j);
    }
  }

  delete rk;
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
  _x_pred(0,0) = invp;
  _x_pred(1,0) = pv/pu;
  _x_pred(2,0) = pw/pu;
  _x_pred(3,0) = v; // local y (v)
  _x_pred(4,0) = w; // local z (w)

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

  _build_Q();

  // Add the process noise (scattering, etc.) matrix to the
  // covariance.  Since it's blurring of phi and theta, it's easier to
  // do in SC coordinates.

  if(_reverse_direction)
    _C_pred += _Q;


  // convert the Geant covariance matrix to local coords
  _C_pred = _F_SC2SD * _C_pred * _F_SC2SD.transpose();

  // Now fill residuals
  _r_pred = _meas;
  _r_pred -= _H*_x_pred;
  _R_pred = _V;
  _R_pred += (_H * _C_pred * _H.transpose());

  if(_verb>1) cout << "End prediction step" << endl;
  _predict_done = true;
  return true;
}

//-------------------------------------------------------------------
void SvxKalmanNode::filter()
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
void SvxKalmanNode::initialize_first_node(const Eigen::VectorXd &init_p, const float relperr)
{
  Eigen::VectorXd local_mom(global_to_local_mom(init_p));

  if(_verb>1) {
    cout << endl << "========================================================================================" << endl;
    cout << "SvxKalmanNode::initialize_first_node() --- global mom:"
         << init_p;
  }

  double p = init_p.norm();
  double invp = 1./p;
  _x_filt(0) = invp;
  _x_filt(1) = local_mom(1)/local_mom(0);
  _x_filt(2) = local_mom(2)/local_mom(0);
  _x_filt(3) = _meas(0);
  _x_filt(4) = _meas(1);

  float invperr = 1.;  // 1/p +- 1 GeV^-1
  if(relperr != 0.)
    invperr = invp - invp/(1+relperr);

  // These should be updated once we have a nice fast-fitter
  _C_filt(0,0) = sqr(invperr);
  _C_filt(1,1) = sqr(1.); // +- ~45 degrees
  _C_filt(2,2) = sqr(1.);
  _C_filt(3,3) = sqr(_meas_sigma(0));
  _C_filt(4,4) = sqr(_meas_sigma(1));

  _filt_pos = state_vec_to_global_xyz(_x_filt);

  _filt_mom(0) = init_p(0);
  _filt_mom(1) = init_p(1);
  _filt_mom(2) = init_p(2);
  if(_reverse_direction) _filt_mom = -_filt_mom;
  _filter_done = true;

  _build_jacobians();
  _build_Q();
  return;
}

//-------------------------------------------------------------------
Eigen::MatrixXd SvxKalmanNode::get_best_cov_matrix_curvilinear() const
{
  if(_verb>1)
    cout << "SvxKalmanNode::get_best_cov_matrix_curvilinear() - rot. matrix"
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
void SvxKalmanNode::reset()
{
  KalmanNode::reset();

  // our state vector to measurement transformation is easy b/c we already store the local x,z
  _H(0,3) = 1.;
  _H(1,4) = 1.;

  _rot_to_global.setZero();
  _vector_global_to_local_origin.setZero();

  _pred_pos = Eigen::Vector3d::Zero(); // predicted position at this node
  _pred_mom = Eigen::Vector3d::Zero(); // predicted momentum at this node

  _filt_pos = Eigen::Vector3d::Zero(); // filtered position at this node
  _filt_mom = Eigen::Vector3d::Zero(); // filtered momentum at this node

  _smooth_pos = Eigen::Vector3d::Zero(); // smoothed position at this node
  _smooth_mom = Eigen::Vector3d::Zero(); // smoothed momentum at this node

  _sensor = 0; // SvxSensor corresponding to this node
  _X0 = 0.; // radiation lengths for this layer
  _is_positive = 0; // charge

  _F_SC2SD.setZero();
  _F_SD2SC.setZero();

  _rot_curvilinear_to_local.setZero(); // full transformation matrix to go from curvilinear to local (sensor) coordinates

  return;
}

//-------------------------------------------------------------------
void SvxKalmanNode::_post_smoothing()
{
  /// After smoothing is done in the KalmanNode base class, the
  /// position and momentum need to be updated from the state vector

  _smooth_mom = state_vec_to_global_mom(_x_smooth);
  _smooth_pos = state_vec_to_global_xyz(_x_smooth);

  // if we were propagating in reverse, flip all of the momenta back
  // to the correct direction
  if(_reverse_direction) {
    _smooth_mom = -_smooth_mom;
    _filt_mom = -_filt_mom;
    _pred_mom = -_pred_mom;
  }

  _smoothing_done = true;
  return;
}

//-------------------------------------------------------------------
Eigen::VectorXd SvxKalmanNode::state_vec_to_global_xyz(const Eigen::VectorXd &state) const
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
Eigen::VectorXd SvxKalmanNode::state_vec_to_global_mom(const Eigen::VectorXd &state) const
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
Eigen::VectorXd SvxKalmanNode::global_to_local_xyz(const Eigen::VectorXd &position) const
{
  Eigen::VectorXd global = position - _vector_global_to_local_origin;
  Eigen::VectorXd local(_rot_to_global.transpose()*global);
  return local;
}

//-------------------------------------------------------------------
Eigen::VectorXd SvxKalmanNode::global_to_local_mom(const Eigen::VectorXd &mom) const
{
  Eigen::VectorXd local(_rot_to_global.transpose()*mom);
  return local;
}

//-------------------------------------------------------------------
Eigen::VectorXd SvxKalmanNode::local_to_global_mom(const Eigen::VectorXd &mom) const
{
  Eigen::VectorXd global(_rot_to_global*mom);
  return global;
}

//-------------------------------------------------------------------
void SvxKalmanNode::load_measurement(const SvxCluster* cluster)
{
  Eigen::VectorXd meas_pos(3,1);
  meas_pos << cluster->get_xyz_global(0), cluster->get_xyz_global(1), cluster->get_xyz_global(2);

  int layer = cluster->get_layer();
  load_measurement(meas_pos, layer);
  return;
}

//-------------------------------------------------------------------
void SvxKalmanNode::load_measurement(const SvxClusterInfo* cluster)
{
  Eigen::VectorXd meas_pos(3,1);
  meas_pos(0) = cluster->getPosition(0);
  meas_pos(1) = cluster->getPosition(1);
  meas_pos(2) = cluster->getPosition(2);

  int layer = cluster->getLayer();
  load_measurement(meas_pos, layer);
  return;
}

//-------------------------------------------------------------------
void SvxKalmanNode::load_measurement(const Eigen::VectorXd &cluster_pos, const int &cluster_layer)
{
  // Set the measured cluster position in local coords (x,z)
  Eigen::VectorXd meas_pos_local( global_to_local_xyz(cluster_pos) );
  _meas(0) = meas_pos_local(1);  // v
  _meas(1) = meas_pos_local(2);  // w

  double sigma_x, sigma_z;
  // Set the uncertainty on the cluster position
  if( cluster_layer<2 ){  // pixels

    // Alan's values
    //sigma_x = .0025;
    //sigma_z = .02125;

    // SvxTracker values 9/30/2011
    // sigma_x = .005 / sqrt(12.);
    // sigma_z = .0425 / sqrt(12.);

    // From looking at my ntuples 2/7/2013
    sigma_x = .0010;
    sigma_z = .0115;

    _X0 = .0128;

  } else {  // now do stripixels

    // Alan's values
    //sigma_x = .004;
    //sigma_z = .05;

    // SvxTracker values 9/30/2011
    // sigma_x = .008 / sqrt(12.);
    // sigma_z = .1 / sqrt(12.);

    // From looking at my ntuples 10/6/2011
    // sigma_x = .0015;
    // sigma_z = .0340;

    // From looking at my ntuples 2/7/2013
    sigma_x = .0015;
    sigma_z = .0300;

    _X0 = .0543;
  }

  _meas_sigma(0) = sigma_x; // v
  _meas_sigma(1) = sigma_z; // w

  if(_verb>1) {
    StateVector tmpstate(5,1);
    tmpstate.setZero();
    tmpstate(3) = _meas(0);
    tmpstate(4) = _meas(1);
    VectorXd tmpmeas( state_vec_to_global_xyz(tmpstate) );
    cout << "meas_pos global:" << cluster_pos << "meas_pos local:" << _meas
         << "meas_pos rotated to global:" << tmpmeas << endl;
    cout << "meas_pos local sigma:" << _meas_sigma << endl;
  }

  return;
}

//-------------------------------------------------------------------
void SvxKalmanNode::load_sensor_geometry(const SvxSensor* sensor)
{
  // Now set up the plane of the sensor for Geant track
  // projection, using one point in the plane and the normal
  // vector

  // if normal vector is local y-axis on sensor, then in global
  // coords it is the middle column of the rotation matrix
  // (which is tranpose of stored matrix, so middle row)
  TVector3 plane_norm(sensor->get_rotMatrix(1,0), 
                      sensor->get_rotMatrix(1,1),
                      sensor->get_rotMatrix(1,2));
  TVector3 plane_point(sensor->get_transVector(0), 
                       sensor->get_transVector(1),
                       sensor->get_transVector(2));
  plane_norm *= -1.; // I want y-axis to be away from vertex
  if(_verb>1) {
    cout << "plane norm: (" << plane_norm(0) << ", " << plane_norm(1) << ", " << plane_norm(2) << ")" << endl;
    cout << "plane point: " << plane_point(0) << ", " << plane_point(1) << ", " << plane_point(2) << ")" << endl;
  }

  const TVector3 point(sensor->get_transVector(0), sensor->get_transVector(1),
                       sensor->get_transVector(2));
  // G4Point3D point_transverse(point(0), point(1), 0);
  // G4Point3D unit_transverse = point_transverse / point.perp();

  // point_transverse = unit_transverse * (plane_point.dot(plane_norm) / unit_transverse.dot(plane_norm));

  _vector_global_to_local_origin(0) = point(0);
  _vector_global_to_local_origin(1) = point(1);
  _vector_global_to_local_origin(2) = point(2);

  // Fill the rotation matrix for the sensor plane (their matrix is global->local)
  Eigen::MatrixXd rot_sensor_to_global(Eigen::MatrixXd::Zero(3,3));
  for(int i=0; i<3; i++) {
    for(int j=0; j<3; j++) {
      rot_sensor_to_global(i,j) = sensor->get_rotMatrix(j,i);
    }
  }

  if(_verb>1)
    cout << "rot_sensor_to_global original SVX geometry:" << rot_sensor_to_global << endl;


  // Correct for sensors where local +z = global -z
  if(rot_sensor_to_global(2,2)<0) {
    Eigen::MatrixXd rot_around_local_y(Eigen::MatrixXd::Zero(3,3));
    rot_around_local_y(0,0) = -1.;
    rot_around_local_y(1,1) = 1.;
    rot_around_local_y(2,2) = -1.;
    rot_sensor_to_global = rot_sensor_to_global * rot_around_local_y;
  }

  // y points towards vertex --> x points away from vertex
  Eigen::MatrixXd rot_around_local_z(Eigen::MatrixXd::Zero(3,3));
  rot_around_local_z(0,1) = 1.;
  rot_around_local_z(1,0) = -1.;
  rot_around_local_z(2,2) = 1.;
  rot_sensor_to_global = rot_sensor_to_global * rot_around_local_z;
  
  _rot_to_global = rot_sensor_to_global;

  if(_verb>1) {
    cout << "_vector_global_to_local_origin:" << _vector_global_to_local_origin << endl;
    cout << "rot_sensor_to_global:" << rot_sensor_to_global << endl;
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
  
  return;
}

//-------------------------------------------------------------------
void SvxKalmanNode::_build_Q()
{
  /// Calculate the multiple scattering angle RMS as in: 
  /// Fruhwirth and Regler, Nucl. Instr. and Meth. A 456 (2001) 369
  /// then add it to the noise matrix (Q)

  //double m = 5.11e-4;
  double m = 1.;

  StateVector x = get_best_x();

  double invp = x(0);
  double psq = 1./(invp*invp);
  double invpbetasq = (psq+m*m)/(psq*psq);
  double invbetasq = psq*invpbetasq;

  double tan_v = x(1);
  double tan_w = x(2);
  double length = _X0 * sqrt((sqr(tan_v)+1)*(sqr(tan_w)+1));;
  double ms_sigma_sq = invpbetasq*length*sqr(0.015*(0.9184 + 0.036*log(invbetasq*length)));

  // Eigen::VectorXd mom = get_best_mom_global();
  // double coslambda = cos( atan2(mom(2), sqrt(sqr(mom(0))+sqr(mom(1)))) );

  //double ms_sigma_sq = invpbetasq * sqr(_dtheta_ms);
  _Q(1,1) = ms_sigma_sq; // lambda direction
  _Q(2,2) = ms_sigma_sq; // phi direction

  if(_verb>1)
    cout << "Particle mass: " << m << endl
         << "Scattering sigma (degrees): " << sqrt(ms_sigma_sq)*180./M_PI << endl;
  if(_verb>1) cout << "Physics Noise Q:" << _Q << endl;
  return;
}

//-------------------------------------------------------------------
void SvxKalmanNode::_build_jacobians()
{
  /// Construct the Jacobian matrices (F) that convert between
  /// curvilinear (SC) and planar (SD) coordinates.  This is based on
  /// the code in Geant4e and the paper: A. Strandlie and W. Wittek,
  /// Nucl. Instr. and Methods A 566 (2006) 687–698,
  /// http://dx.doi.org/10.1016/j.nima.2006.07.032

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
  if( charge != 0 ) {
    TVector3 Bpos(position(0), position(1), position(2));
    TVector3 Btemp = RKTrackRep::getFieldValTesla(Bpos);

    Eigen::Vector3d B;
    B << Btemp(0), Btemp(1), Btemp(2);
    double Bmag = B.norm();
    
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
    cout << "SvxKalmanNode::_build_jacobians() - F_SD2SC:" << _F_SD2SC << endl;
    cout << "SvxKalmanNode::_build_jacobians() - F_SC2SD:" << _F_SC2SD << endl;
  }
  return;
}
