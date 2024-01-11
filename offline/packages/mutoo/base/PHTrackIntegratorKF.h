#ifndef __PHTRACKINTEGRATORKF_H__
#define __PHTRACKINTEGRATORKF_H__

// $Id: PHTrackIntegratorKF.h,v 1.3 2015/06/04 21:45:17 snowball Exp $ 

/*!
\file PHTrackIntegratorKF.h
\brief propagates track parameters at given z to destination z, using Geant in magnet field and through matter;
updates covariance matrix; calculates derivatives of output state vector wrt input state vector
\author Hugo Pereira, Sean Kelly
\version $Revision: 1.3 $
\date $Date: 2015/06/04 21:45:17 $
*/

#include<MUTOO.h>
#include<PHException.h>
#include<PHGslMatrix.h>

class TMutTrkPar;

/*! \ingroup classes */
//! Charged Track Integrator
/*! 
  propagates track parameters at given z to destination z, using Geant in magnet field and through matter;
  update covariance matrix; calculates derivatives of output state vector wrt input state vector             
*/
class PHTrackIntegratorKF
{

  public:

  //! Default constructor 
  PHTrackIntegratorKF();

  //! Destructor
  virtual ~PHTrackIntegratorKF(){}

  
  //! Initialize integrator 
  void initialize(TMutTrkPar par);
  
  //! Extrapolate the system to specified z.  
  void extrapolate( double z);

  //! Update TMutTrkPar with current integrator state 
  void finish(TMutTrkPar& par);  

  //! Clear parameters
  void clear();
  
  //! x position
  void set_x(double value ) 
  { _state_kf(3,0) = value; }
  
  //! y position
  void set_y(double value ) 
  { _state_kf(4,0) = value; } 
  
  //! z position
  void set_z(double value ) 
  { _z = value; } 
  
  //! c/p 
  void set_pinv(double value ) 
  { _state_kf(0,0) = value; } 
  
  //! px/pz
  void set_dxdz(double value )
  { _state_kf(1,0) = value; } 
  
  //! py/pz
  void set_dydz(double value ) 
  { _state_kf(2,0) = value; } 
  
  //! direction (dz sign)
  void set_direction(double value ) 
  { _direction = value; } 
  
  //! sets all kf input 
  void set_kf( 
    PHGslMatrix state_vector, 
    PHGslMatrix covar, 
    double z, 
    double direction )
  {
    _state_kf = state_vector;
    _covar_kf = covar;
    _z = z;
    _direction = direction;
  }
  
  //! sets kf state vector
  void set_state_vector_kf(PHGslMatrix state_kf ) 
  { _state_kf = state_kf; }
  
  //! sets kf covariance matrix
  void set_covar_kf(PHGslMatrix covar_kf ) 
  { _covar_kf = covar_kf; }
      
  //! true if dout_dz derivatives calculations are to be performed
  void set_z_step(double value ) 
  { _z_step = value; }
  
  //! sets z_step needed for dout_dz derivatives calculations
  void set_do_dz_deriv(bool value ) 
  { _do_dz_deriv = value; }

  //! returns 'kalman' state vector (c/p, dx/dz, dy/dz, x, y) in root matrix (5x1)
  PHGslMatrix get_state_vector_kf( void ) const 
  { return _state_kf; }

  //! returns 'kalman' cov matrix (5x5)
  PHGslMatrix get_covar_kf( void ) const 
  { return _covar_kf; }
  
  //! returns derivative matrix of output 'kalman' state vector wrt input 'kalman' state vector (5x5)
  PHGslMatrix get_dout_din_kf( void ) const 
  { return _dout_din_kf; }
  
  //! returns derivative matrix of output 'kalman' state vector wrt extrapolation z (5x1)
  PHGslMatrix get_dout_dz_kf( void ) const
  { return _dout_dz_kf; }
  
  //! returns current z
  double get_z( void ) 
  { return _z; }
  
  //! returns direction ( sign(pz) )
  double get_direction( void )
  { return _direction; }

  //! true if dout_dz derivatives calculations are to be performed
  bool get_do_dz_deriv( void ) 
  { return _do_dz_deriv; }
  
  //! returns z_step needed for dout_dz derivatives calculations
  double get_z_step( void )
  { return _z_step; }
  
  //! returns true if extrapolation failed.
  bool get_error( void ) 
  { return _error; }
  
  //! particle enumeration to calculate energy loss
  enum Particle 
  { 
    MUON=0, 
    PION=1, 
    KAON=2, 
    PROTON=3,
    ELECTRON=4
  };
  
  //! sets particle type
  void set_particle_type(Particle part )
  { _particle = part; }
  
  //! retrieves particle type
  Particle get_particle_type( void ) 
  { return _particle; }
  
  private:

  //! matrix size
  enum { 
    COVAR_ROW=5, 
    COVAR_SIZE=25 
  };
  
  //! (c/p, dxdz, dydz, x, y) (kalman style state vector)
  PHGslMatrix _state_kf;    
  
  //! corresponding covariance matrix
  PHGslMatrix _covar_kf;    
  
  //! output state vector derivatives wrt input state vector
  PHGslMatrix _dout_din_kf; 
  
  //! derivatives wrt extrapolation z
  PHGslMatrix _dout_dz_kf;  
  double _z;
  double _direction;
  
  //! delta Z in the vicinity of extrapolation z to calculate dout_dz derivatives
  double _z_step;    
  
  //! if true dz_derivatives are calculated
  bool _do_dz_deriv;            
  
  //! if false extrapolation failed.
  bool _error;                  
  
  //! particle type. Default is muon.
  Particle _particle;   
  
};

#endif
  
