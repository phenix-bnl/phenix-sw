// $Id: PHTrackIntegratorKF.cxx,v 1.6 2017/10/12 01:42:53 shlim Exp $ 

/*!
\file PHTrackIntegratorKF.cxx
\brief propagates track parameters at given z to destination z, using Geant in magnet field and through matter;
updates covariance matrix; calculates derivatives of output state vector wrt input state vector
\author Hugo Pereira, Sean Kelly
\version $Revision: 1.6 $
\date $Date: 2017/10/12 01:42:53 $
*/

#include <PHGeantExtrapP.h>
#include <boost/array.hpp>

#include "PHTrackIntegratorKF.h"
#include "TMutTrkPar.hh"
#include "TMutRecoPar.hh"
#include "TMutKalmanUtil.h"

using namespace std;

//___________________________________________
PHTrackIntegratorKF::PHTrackIntegratorKF() :
  _state_kf( PHGslMatrix( COVAR_ROW, 1 ) ),
  _covar_kf( PHGslMatrix( COVAR_ROW, COVAR_ROW ) ),
  _dout_din_kf( PHGslMatrix( COVAR_ROW, COVAR_ROW ) ),
  _dout_dz_kf( PHGslMatrix( COVAR_ROW, 1 ) ),
  _z(0),
  _direction(-1),
  _z_step( 0.5 ),
  _do_dz_deriv( false ),
  _error( false ),
  _particle( PHTrackIntegratorKF::MUON )
{
  // init covar matrix and dout_din to unit
  _covar_kf.unit();
  _dout_din_kf.unit();
}

//___________________________________________________________________________
void PHTrackIntegratorKF::initialize(TMutTrkPar trk_par)
{
  // state vector initialization
  _state_kf( 0,0 ) = trk_par.get_charge() / trk_par.get_ptot();
  _state_kf( 1,0 ) = trk_par.get_px() / trk_par.get_pz();
  _state_kf( 2,0 ) = trk_par.get_py() / trk_par.get_pz();
  _state_kf( 3,0 ) = trk_par.get_x();
  _state_kf( 4,0 ) = trk_par.get_y();
  _z = trk_par.get_z();
  _direction = ( trk_par.get_pz() > 0 ) ? 1:-1;

  // initialize covariance matrix
  _covar_kf = TMutKalmanUtil::get_covar_kf( trk_par );
  _dout_din_kf.unit();
  _dout_dz_kf.zero();

}

void PHTrackIntegratorKF::clear()
{
  _z = 0;
  _z_step = 0.5;
  _direction = -1;
  _state_kf.zero();
  _covar_kf.unit();
  _dout_din_kf.unit();
  _dout_dz_kf.zero();
  _error = false;
  _do_dz_deriv = false;
  _particle = PHTrackIntegratorKF::MUON;
}


//___________________________________________________________________________
void PHTrackIntegratorKF::extrapolate( double z_out )
{
  
  boost::array<double, COVAR_SIZE> covar_in;
  boost::array<double, COVAR_SIZE> covar_out;
  boost::array<double, COVAR_SIZE> dout_din;
  boost::array<double, COVAR_ROW> in;
  boost::array<double, COVAR_ROW> out;
  
  // initialize everything
  covar_in.assign(0);
  covar_out.assign(0);
  dout_din.assign(0);
  in.assign(0);
  out.assign(0);
  
  // input z
  double z_in = _z;

  // prepare io tables.
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  {
    in[i] = _state_kf( i,0 );
    out[i] = 0;
  }

  for( unsigned int i=0; i<COVAR_ROW; i++ )
  {
    for( unsigned int j=0; j<COVAR_ROW; j++ )
    { covar_in[i*COVAR_ROW+j] = _covar_kf(i,j); }
  }

  // positive direction is for north arm (1); negative for south (0)
  int arm = (_direction > 0 )? 1:0;  
  int failure( 0 );

  /*
    retrieve particle ID from particle type and charge
    particle ID comes from geant
  */

  int pid = 0;
  switch( _particle ) 
  {

    case MUON: pid = ( _state_kf( 0,0 ) > 0 ) ? MUTOO::PISA_PID_MUPOS:MUTOO::PISA_PID_MUNEG; break;
    case PION: pid = ( _state_kf( 0,0 ) > 0 ) ? MUTOO::PISA_PID_PIPOS:MUTOO::PISA_PID_PINEG; break;
    case KAON: pid = ( _state_kf( 0,0 ) > 0 ) ? MUTOO::PISA_PID_KPOS:MUTOO::PISA_PID_KNEG; break;
    case PROTON: pid = ( _state_kf( 0,0 ) > 0 ) ? MUTOO::PISA_PID_PPOS:MUTOO::PISA_PID_PNEG; break;
    case ELECTRON: pid = ( _state_kf( 0,0 ) > 0 ) ? MUTOO::PISA_PID_EPOS:MUTOO::PISA_PID_ENEG; break;
    default:
      throw invalid_argument( DESCRIPTION("wrong particle type"));
      break;
  }

  // check z indeed changes. Do nothing if yes.
  //if( z_out != _z ) 
	if( fabs(z_out-_z)>1e-4 ) //Sanghoon: prevent eq. comparion between floating-point numbers 
  {

    // call geant extrapolator
    phgeantextrap_(
      &arm,
      &pid,
      &z_in,  &in[0],  &covar_in[0], 
      &z_out, &out[0], &covar_out[0], 
      &dout_din[0],       
      &failure );
    _error = bool( failure );

  } else {

    // make dumy extrapolator
    _error = false;
    for( unsigned int i=0; i<COVAR_ROW; i++ ) 
    {
      out[i] = in[i];
      for( unsigned int j=0; j<COVAR_ROW; j++ ) 
      {
        covar_out[i*COVAR_ROW+j] = covar_in[j*COVAR_ROW+i];
        dout_din[j*COVAR_ROW+i] = (i==j)? 1:0;
      }
    }

  }

  if( !_error ) 
  {
    
    // update state vector
    for( unsigned int i=0; i<COVAR_ROW; i++ ) {
      _state_kf( i,0 ) = out[i];
    }
    _z = z_out;

    // update covariance matrix and propagator derivatives
    // WARNING: matrix need to be transposed because of Fortran column/row ordering
    for( unsigned int i=0; i<COVAR_ROW; i++ )
      {
	for( unsigned int j=0; j<COVAR_ROW; j++ ) 
	  {
	    _covar_kf(i,j) = covar_out[j*COVAR_ROW+i];
	    _dout_din_kf(i,j) = dout_din[j*COVAR_ROW+i];
	  }
      }
  }
  
  // stops here if dz derivatives are not required
  if( !_do_dz_deriv ) return;

  // initialize _dout_dz_kf with state vector
  for( unsigned int i=0; i<COVAR_ROW; i++ ) 
  { _dout_dz_kf(i,0) = -out[i]; }

  // add a step to z_in to have derivatives wrt z_in
  z_in += _direction*_z_step;

  // call geant extrapolator to a slightly different z
  phgeantextrap_(
    &arm,
    &pid,
    &z_in,  &in[0],  &covar_in[0], 
    &z_out, &out[0], &covar_out[0], 
    &dout_din[0],       
    &failure );

  if( failure ) return;
  
  // calculate _dout_dz_kf
  for( unsigned int i=0; i<COVAR_ROW; i++ ) 
  {
    _dout_dz_kf(i,0) += out[i];
    _dout_dz_kf(i,0) /= _z_step;
  }

  return;
}

//___________________________________________________________________________
void PHTrackIntegratorKF::finish(TMutTrkPar& trk_par)
{
  // update position
  trk_par.set_x( _state_kf(3,0) );
  trk_par.set_y( _state_kf(4,0) );
  trk_par.set_z( _z );

  // update momentum
  double pz_inv = fabs(_state_kf(0,0))*sqrt( 1+MUTOO::SQUARE(_state_kf(1,0))+MUTOO::SQUARE(_state_kf(2,0)) );
  double pz = (pz_inv) ? 1.0/pz_inv:1.0;
  pz *= _direction;
  trk_par.set_px( pz*_state_kf(1,0) );
  trk_par.set_py( pz*_state_kf(2,0) );
  trk_par.set_pz( pz );
  trk_par.set_charge( (_state_kf(0,0)>0)? 1:-1 );

  PHGslMatrix cov_mutoo(
    TMutKalmanUtil::cov_kalman_to_mutoo(
      trk_par.get_charge(),
      _covar_kf,
      PHVector( trk_par.get_px(), trk_par.get_py(), trk_par.get_pz() )
    )
  );
  for( unsigned int i=0; i<COVAR_ROW; i++ )
    {
      for( unsigned int j=0; j<COVAR_ROW; j++ )
	{
	  trk_par.set_covar( i, j, cov_mutoo(i,j) );
	}
    }

}

