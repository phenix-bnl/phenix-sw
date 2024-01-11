#include <TMutKalmanUtil.h>
#include <MUTOO.h>

//__________________________________________________________
// state vectors conversions
PHGslMatrix TMutKalmanUtil::cov_kalman_to_mutoo( double charge, PHGslMatrix cov_kalman, PHVector p_vect )
{
  double px = p_vect.getX();
  double py = p_vect.getY();
  double pz = p_vect.getZ();
  double pz2 = MUTOO::SQUARE( pz );
  double p = std::sqrt( MUTOO::SQUARE( px ) + MUTOO::SQUARE( py ) + MUTOO::SQUARE( pz ) );
  double p3 = p*p*p;
  
  // transformation matrix 
  // mutoo to kalman matrix is computed as the algebra is easier
  // matrix is then inverted
  PHGslMatrix m(5,5); 
  m(0,2) = -charge*px/p3; // d(c/p)/dpx 
  m(0,3) = -charge*py/p3; // d(c/p)/dpy
  m(0,4) = -charge*pz/p3; // d(c/p)/dpz
  m(1,2) = 1/pz; // d(px/pz)/dpx
  m(1,4) = -px/pz2; // d(px/pz)/dpz
  m(2,3) = 1/pz; // d(py/pz)/dpy
  m(2,4) = -py/pz2; // d(py/pz)/dpz 
  m(3,0) = 1; // dx/dx
  m(4,1) = 1; // dy/dy
  
  // calculate mutoo covariance matrix 
  // cov_mutoo = m_inv.cov_kalman.m_inv_transposed
  PHGslMatrix m_inv( m.invert() ); 
  PHGslMatrix m_inv_t( m_inv.transpose() );
  return m_inv*cov_kalman*m_inv_t;
}

//______________________________________________________________________
PHVector TMutKalmanUtil::mom_kalman_to_mutoo( int arm, PHVector kal_mom )
{
  double pz_inv = fabs(kal_mom.getX())*sqrt( 1+MUTOO::SQUARE(kal_mom.getY())+MUTOO::SQUARE(kal_mom.getZ()) );
  double pz = (pz_inv) ? 1.0/pz_inv:1.0;  
  if( arm == MUTOO::South ) pz *= -1;
  return PHVector( pz*kal_mom.getY(), pz*kal_mom.getZ(), pz );
}

//__________________________________________________________
PHGslMatrix TMutKalmanUtil::cov_vtx_kalman_to_mutoo( double charge, PHGslMatrix cov_kalman, PHVector p_vect )
{
  double px = p_vect.getX();
  double py = p_vect.getY();
  double pz = p_vect.getZ();
  double pz2 = MUTOO::SQUARE( pz );
  double p  = std::sqrt( MUTOO::SQUARE( px ) + MUTOO::SQUARE( py ) + MUTOO::SQUARE( pz ) );
  double p3 = p*p*p;
  
  // transformation matrix 
  // mutoo to kalman matrix is computed as the algebra is easier
  // matrix is then inverted
  PHGslMatrix m(3,3);
  m(0,0) = -charge*px/p3; // d(c/p)/dpx 
  m(0,1) = -charge*py/p3; // d(c/p)/dpy
  m(0,2) = -charge*pz/p3; // d(c/p)/dpz
  m(1,0) = 1/pz; // d(px/pz)/dpx
  m(1,2) = -px/pz2; // d(px/pz)/dpz
  m(2,1) = 1/pz; // d(py/pz)/dpy
  m(2,2) = -py/pz2; // d(py/pz)/dpz 
  
  // returns mutoo vtx covariance matrix (3x3) 
  // cov_muto = m_inv*cov_kalman*m_inv_transposed
  PHGslMatrix m_inv( m.invert() ); 
  PHGslMatrix m_inv_t( m_inv.transpose() );
  return m_inv*cov_kalman*m_inv_t;
}

//__________________________________________________________
PHGslMatrix TMutKalmanUtil::cov_vtx_mutoo_to_kalman( double charge, PHGslMatrix cov_mutoo, PHVector p_vect )
{
  double px = p_vect.getX();
  double py = p_vect.getY();
  double pz = p_vect.getZ();
  double pz2 = MUTOO::SQUARE( pz );
  double p = std::sqrt( MUTOO::SQUARE( px ) + MUTOO::SQUARE( py ) + MUTOO::SQUARE( pz ) );
  double p3 = p*p*p;
  
  // transformation matrix 
  // mutoo to kalman matrix is computed as the algebra is easier
  // matrix is then inverted
  PHGslMatrix m(3,3);
  m(0,0) = -charge*px/p3;  // d(c/p)/dpx 
  m(0,1) = -charge*py/p3;  // d(c/p)/dpy
  m(0,2) = -charge*pz/p3;  // d(c/p)/dpz
  m(1,0) = 1/pz; // d(px/pz)/dpx
  m(1,2) = -px/pz2; // d(px/pz)/dpz
  m(2,1) = 1/pz; // d(py/pz)/dpy
  m(2,2) = -py/pz2; // d(py/pz)/dpz 
    
  // returns kalman vtx covariance matrix (3x3)
  // cov_kalman = m*cov_muto*m_t;
  PHGslMatrix m_t( m.transpose() );
  return m*cov_mutoo*m_t; 
}
  
//______________________________________________________________________
PHGslMatrix TMutKalmanUtil::get_covar_kf( const TMutTrkPar &trk_par )
{  

  // retrieve mutoo covariance matrix
  PHGslMatrix cov_mutoo(5, 5); 
  for( int i=0; i<5; i++ )
  for( int j=0; j<5; j++ )
  cov_mutoo( i,j ) = trk_par.get_covar( i, j );
  
  // create transformation matrix
  double px = trk_par.get_px();
  double py = trk_par.get_py();
  double pz = trk_par.get_pz();
  double pz2 = MUTOO::SQUARE( pz );
  double p   = trk_par.get_ptot() ;
  double p3 = p*p*p;
  double charge = trk_par.get_charge();
  
  PHGslMatrix m(5,5);
  m(0,2) = -charge*px/p3;  // d(c/p)/dpx 
  m(0,3) = -charge*py/p3;  // d(c/p)/dpy
  m(0,4) = -charge*pz/p3;  // d(c/p)/dpz
  m(1,2) = 1/pz;  // d(px/pz)/dpx
  m(1,4) = -px/pz2; // d(px/pz)/dpz
  m(2,3) = 1/pz; // d(py/pz)/dpy
  m(2,4) = -py/pz2; // d(py/pz)/dpz 
  m(3,0) = 1; // dx/dx
  m(4,1) = 1; // dy/dy

  // returns kalman PHGslMatrix matrix
  // cov_kalman = m.cov_mutoo.m_t
  PHGslMatrix m_t( m.transpose() );
  return m*cov_mutoo*m_t;
}

//________________________________________________________________
PHGslMatrix TMutKalmanUtil::get_state_vector_kf( const TMutTrkPar& trk_par ) 
{
  PHGslMatrix out( 5, 1 );
  out( 0, 0 ) = trk_par.get_charge()/trk_par.get_ptot();
  out( 1, 0 ) = trk_par.get_px()/trk_par.get_pz();
  out( 2, 0 ) = trk_par.get_py()/trk_par.get_pz();
  out( 3, 0 ) = trk_par.get_x();
  out( 4, 0 ) = trk_par.get_y();
  return out;
}

//________________________________________________________________
void TMutKalmanUtil::print_trk_par_kf( const TMutTrkPar& trk_par )
{
  
  MUTOO::PRINT( std::cout, "TMutKalmanUtil::print_trk_par_kf");
  PHGslMatrix p( get_state_vector_kf( trk_par ) );
  PHGslMatrix c( get_covar_kf( trk_par ) );
  std::cout << "z=" << trk_par.get_z() << "cm, direction=" << ((trk_par.get_pz()<0)?-1.0:1.0) << std::endl;
  std::cout << "state_vector:" << p;
  std::cout << "covariance:" << c;
  MUTOO::PRINT( std::cout, " ** ");
}
