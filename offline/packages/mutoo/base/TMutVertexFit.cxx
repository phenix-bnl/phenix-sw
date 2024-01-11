// $Id: TMutVertexFit.cxx,v 1.6 2013/12/27 13:32:07 slash Exp $

/*!
  \file    TMutVertexFit.C
  \brief   vertex fit from NanoDST tracks and event vertex information
  \author  Hugo Pereira
  \version $Revision: 1.6 $
  \date    $Date: 2013/12/27 13:32:07 $
*/

#include <stdexcept>
#include <cmath>

#include "PHException.h"
#include "TMutVertexFit.h"
#include "TMutKalmanUtil.h"

using namespace std;

//________________________________________________________
TMutVertexFit::TMutVertexFit( void ):
  _has_ext_vtx( false ),
  _vtx_cov(7,7),
  _verbosity( 0 )
{}

//________________________________________________________
void TMutVertexFit::add_vertex( const PHPoint& vertex, const PHPoint& vertex_error )
{

  if( _has_ext_vtx ) {
    cout << "TMutVertexFit::add_vtx - ext vertex already added. canceled.\n";
    return;
  }

  // sets vertex position
  _ext_vtx_par._x.zero();
  _ext_vtx_par._x( 0, 0 ) = vertex.getX();
  _ext_vtx_par._x( 1, 0 ) = vertex.getY();
  _ext_vtx_par._x( 2, 0 ) = vertex.getZ();
  
  /* 
    sets vertex gain matrix
    error on x and y is arbitrary set to 1cm
  */  
  _ext_vtx_par._g.zero();
  _ext_vtx_par._g(0,0) = 1/MUTOO::SQUARE( vertex_error.getX() );
  _ext_vtx_par._g(1,1) = 1/MUTOO::SQUARE( vertex_error.getY() );
  _ext_vtx_par._g(2,2) = 1/MUTOO::SQUARE( vertex_error.getZ() );
  
  // update flag
  _has_ext_vtx = true;
  
  if( _verbosity ) 
  _ext_vtx_par.print();
  
  return;	
}

//________________________________________________________
double TMutVertexFit::get_chisquare( void ) const 
{
  double chisquare = 0;
  for( const_node_iterator node = _node_list.begin(); node != _node_list.end(); node++ ) 
  chisquare += node->_chisquare;
  return chisquare;
}

//________________________________________________________
double TMutVertexFit::get_mass( void ) const
{
  
  if( _node_list.size() < 2 )
  { throw runtime_error( DESCRIPTION( "invalid number of tracks" ) ); }
  
  double e_tot( 0 );
  double px_tot( 0 );
  double py_tot( 0 );
  double pz_tot( 0 );
  
  for( unsigned int index=0; index < _node_list.size(); index++ )
  {
    px_tot += get_px( index );
    py_tot += get_py( index );
    pz_tot += get_pz( index );
    e_tot += sqrt( MUTOO::MASS_MUON_SQUARE + MUTOO::SQUARE( _node_list[index]._ptot ) );
  }
  
  return sqrt( MUTOO::SQUARE( e_tot ) - (
    MUTOO::SQUARE( px_tot ) +
    MUTOO::SQUARE( py_tot ) +
    MUTOO::SQUARE( pz_tot ) ) );
    
}

//________________________________________________________
double TMutVertexFit::get_rapidity( void ) const
{
  
  if( _node_list.size() < 2 )
  { throw runtime_error( DESCRIPTION( "invalid number of tracks" ) ); }
  
  double e_tot( 0 );
  double pz_tot( 0 );
  
  for( unsigned int index=0; index < _node_list.size(); index++ )
  {
    pz_tot += get_pz( index );
    e_tot += sqrt( MUTOO::MASS_MUON_SQUARE + MUTOO::SQUARE( _node_list[index]._ptot ) );
  }
  
  return log( (e_tot + pz_tot)/(e_tot - pz_tot) )/2;
}

//________________________________________________________
void TMutVertexFit::print( ostream& out ) const
{
  MUTOO::PRINT( out, "TMutVertexFit" );
  
  // mass, rapidity
  if( _node_list.size() == 2 )
  {out << " mass: " << get_mass() << ", rapidity: " << get_rapidity() << endl; }
   
  // position and chisquare
  out << " position: (" << get_vtx_x() << "," << get_vtx_y() << "," << get_vtx_z() << ")" << endl; 
  
  out << " chisquare: "<< get_chisquare() << ", trk_count: " << get_trk_count() << ", ndf: " << get_ndf() << endl; 
  
  // input
  out << " input tracks: " << endl;
  for( unsigned int i=0; i < get_trk_count(); i++ )
  {
    out << " track " << i << ", state vector: (" 
      << _node_list[i]._p(0,0) << "," 
      << _node_list[i]._p(1,0) << "," 
      << _node_list[i]._p(2,0) << "," 
      << _node_list[i]._p(3,0) << ")" 
      << endl;
  }
  
  // tracks
  out << " fitted tracks: " << endl;
  for( unsigned int i=0; i < get_trk_count(); i++ )
  {
    out << " track " << i << ", position: (" << get_trk_x(i) << "," <<  get_trk_y(i) << "," << get_trk_z(i) << ")";
    out << " momentum (" << get_px(i) << "," <<  get_py(i) << "," << get_pz(i) << ")" << endl;
  }
  
  MUTOO::PRINT( out, "**" );

}
  
//_________________________________________________________
bool TMutVertexFit::fit( void )
{
  
  // check nodes
  if( !_node_list.size() )
  {
    cout << "TMutVertexFit::fit - no tracks added. Canceled." << endl;
    return false;
  } 
  
  if( _node_list.size() == 1 && !_has_ext_vtx )
  { 
    cout << "TMutVertexFit::fit - only one track and no external vertex. Canceled." << endl;
    return false;
  }
  
  VertexPar vtx_par;
  if( _has_ext_vtx ) vtx_par = _ext_vtx_par;
  
  // max number of iterations is set to 10
  static const unsigned int max_iterations( 10 );
  
  // relative chisquare difference required to stop iterations
  static const double chi_cut( 0.005 );
  
  double old_chisquare = 1e6;
  for( unsigned int iteration = 0; iteration < max_iterations; iteration++ ) {
    vtx_par = iterative_fit( vtx_par, iteration == 0 );
    
    // retrieve total chisquare
    double chisquare = get_chisquare();
    
    // check for convergence
    if( fabs( chisquare - old_chisquare ) < chi_cut*old_chisquare ) break;
    old_chisquare = chisquare;
  }
  
  // calculate the gain matrices for the fitted track parameters
  calculate_gain_matrices( vtx_par );
  
  if( _verbosity )
  for( node_iterator iter = _node_list.begin(); iter != _node_list.end(); iter++ ) {
   	MUTOO::PRINT(cout, "TMutVertexFit::fit_vertex - fitted parameters");
    iter->_trk_par.print();
  }
  
  // update fitted vertex parameters
  _vtx_par = vtx_par;
  
  // update vertex covariance matrix (7x7 parameters: 
  if( _node_list.size() == 2 )
  { fill_vertex_covar(); }
  
  return true;

}

//______________________________________________
TMutVertexFit::VertexPar TMutVertexFit::iterative_fit( const TMutVertexFit::VertexPar & vtx_par, bool first )
{
  // do the linarisation for all nodes
  for( node_iterator node = _node_list.begin(); node != _node_list.end(); node++ ) 
  {
    PHGslMatrix q0( 2, 1);
    
    // define momentum linearization point
    // for first iteration we take the measurement
    if( first ) {
      q0(0,0) = node->_p(0,0);
      q0(1,0) = node->_p(1,0);
    
    // for others, we take result from previous iter
    } else q0 = node->_trk_par._q;
    
    linearize( *node, vtx_par, TrackPar( q0 ) );
  }
  
  VertexPar out;
  
  
  // calculate fitted vertex parameters
  PHGslMatrix left(3,3);
  PHGslMatrix right(3,1);
  
  /*
    position calculations: 
    x = [sum_i( A_i^t.W_i.A_i )]^{-1}.[\sum_i(A^t_i.W.(P_i-C_i))]
    with W_i = G_i[1-B_i(B_i^t.G_i.B_i)^{-1}.B^t_i.G_i]
  */
  
  for( node_iterator node = _node_list.begin(); node != _node_list.end(); node++ ) {
    
    // (b^t.g.b)^{-1}
    node->_bgb_inv = ( PHGslMatrix::get_AtBC( node->_b, node->_g, node->_b ).invert() );
    
    // w matrix = G.[1-B.(Bt.G.B)^{-1}.Bt]
    PHGslMatrix gb( 4, 4 ); 
    gb.unit();
    gb -= ( PHGslMatrix::get_ABCt( node->_b, node->_bgb_inv, node->_b )*node->_g );
    gb = node->_g*gb;
    
    // incremenent left term by a^t.w.a
    left+= PHGslMatrix::get_AtBC( node->_a, gb, node->_a );
    
    // increment right term by a^t.w.(p-c0)
    PHGslMatrix pc( node->_p );
    pc-= node->_c;
    right += PHGslMatrix::get_AtBC(node->_a, gb, pc );
  
  }
  
  // add external vertex contribution to left and right terms
  if( _has_ext_vtx ) {
    
    // external gain matrix G is added to denominator
    left += _ext_vtx_par._g; 
    
    // external G.x is added to numerator
    right += ( _ext_vtx_par._g*_ext_vtx_par._x );  
    
  }
    
  // calculate new vertex position
  out._x = ( left.invert() )*right;
  
  // calculate new momentum (slopes only)
  // q_i = (B^t_i.G_i.B_i)^{-1}.B^t_i.G_i.(P_i-A_i.x-C_i)
  for( node_iterator node = _node_list.begin(); node != _node_list.end(); node++ ) {
    
    PHGslMatrix pca( node->_p );
    pca -= node->_c;
    pca -= ( node->_a*out._x );
    node->_trk_par._q = ( PHGslMatrix::get_ABtC( node->_bgb_inv, node->_b, node->_g )*pca );
  
  }
  
  // calculate chisquare for each node
  for( node_iterator node = _node_list.begin(); node != _node_list.end(); node++ ) {
    PHGslMatrix dp( node->_p );
    dp -= ( node->_a*out._x );
    dp -= ( node->_b*node->_trk_par._q );
    dp -= ( node->_c );
    node->_chisquare = PHGslMatrix::get_AtBC( dp, node->_g, dp )(0,0);	
  }
  
  return out;
}

//______________________________________________
void TMutVertexFit::linearize( TMutVertexFit::Node &node, const TMutVertexFit::VertexPar& vtx_par, const TMutVertexFit::TrackPar& trk_par )
{
  
  // derivatives of state vector wrt x
  node._a.zero();
  node._a(2,0) = 1; node._a(2,2) = -trk_par._q(0,0);
  node._a(3,1) = 1; node._a(3,2) = -trk_par._q(1,0);
  
  // derivatives of state vector wrt q
  node._b.zero();
  node._b(0,0) = 1;
  node._b(1,1) = 1;
  node._b(2,0) = node._z - vtx_par._x(2,0);
  node._b(3,1) = node._z - vtx_par._x(2,0);

  // constant of linearisation
  node._c.zero();
  node._c(2,0) = trk_par._q(0,0)*vtx_par._x(2,0);
  node._c(3,0) = trk_par._q(1,0)*vtx_par._x(2,0);
  
  return;
}

//_________________________________________________________
PHGslMatrix TMutVertexFit::convert_trk_cov( const PHGslMatrix& cov, const PHGslMatrix& momentum ) const
{

  double px( momentum(0,0) );
  double py( momentum(1,0) );
  double pz( momentum(2,0) );
  double pz2( MUTOO::SQUARE( pz ) );
  float p	 = sqrt( MUTOO::SQUARE( px ) + MUTOO::SQUARE( py ) + MUTOO::SQUARE( pz ) ) ;
  float p3 = p*p*p;
  
  PHGslMatrix m(5,5);
  
  // d(1/p)/dpx 
  m(0,2) = -px/p3;	
  
  // d(1/p)/dpy
  m(0,3) = -py/p3;	
  
  // d(1/p)/dpz
  m(0,4) = -pz/p3;
  
  // d(px/pz)/dpx
  m(1,2) = 1/pz;	
  
  // d(px/pz)/dpz
  m(1,4) = -px/pz2;
  
  // d(py/pz)/dpy
  m(2,3) = 1/pz; 
  
  // d(py/pz)/dpz
  m(2,4) = -py/pz2;  
  
  // dx/dx
  m(3,0) = 1; 
  
  // dy/dy
  m(4,1) = 1;   
  
  PHGslMatrix m_t( m.transpose() );
  return m*cov*m_t;

}
    
//______________________________________________
void TMutVertexFit::calculate_gain_matrices( TMutVertexFit::VertexPar &vtx_par )
{
  
  // init vtx_par gain to zero
  vtx_par._g.zero();
  
  for( node_iterator node = _node_list.begin(); node != _node_list.end(); node++ ) {
    
    // calculate qq gain matrix 
    node->_trk_par._g_qq = PHGslMatrix::get_AtBC( node->_b, node->_g, node->_b );
    
    // calculate qx gain matrix
    node->_trk_par._g_qx = PHGslMatrix::get_AtBC( node->_b, node->_g, node->_a );
    
    // increment xx gain matrix
    vtx_par._g += PHGslMatrix::get_AtBC( node->_a, node->_g, node->_a );
    
  }
  
  // increment qx gain matrix with external vertex
  if( _has_ext_vtx ) vtx_par._g += _ext_vtx_par._g;
  
  return;
}


//______________________________________________
void TMutVertexFit::fill_vertex_covar( void )
{
  
  // create covariance matrix (vtx has only 7x7 cov elements: z, px0, py0, pz0, px1, py1, pz1
  int i_node = 0;
  for( node_iterator node = _node_list.begin(); node != _node_list.end(); node++, i_node++ ) 
  {
      
    // first expand momentum qq gain to add c/p cov error from track measurement
    PHGslMatrix g(3,3);
    for( unsigned int i=0; i<3; i++ )
    { g(0,i) = g(i,0) = node->_trk_g( i, 0 ); }
    
    // copy the px/pz and py/pz components from the result of the fit
    for( unsigned int i=0; i<2; i++ )
    {
      for( unsigned int j=0; j<2; j++ )
      { g( i+1, j+1 ) = node->_trk_par._g_qq(i,j); }
    }
    
    // get momentum at vertex
    // transform 'kalman' gain matrix into mutoo covariance matrix
    PHGslMatrix cov(
      TMutKalmanUtil::cov_vtx_kalman_to_mutoo(
        node->_charge,
        g.invert(),
        PHVector( get_px( i_node ), get_py( i_node ), get_pz( i_node ) )
      ) );

    // puts mutoo covariance matrix into vertex object
    for( unsigned int i=0; i<3; i++ )
    for( unsigned int j=0; j<3; j++ )
    _vtx_cov( 1+i_node*3+i, 1+i_node*3+j ) = cov( i,j );
  }

  // sets z covariance error
  _vtx_cov( 0, 0 ) = _vtx_par._g.invert()( 2, 2 );
  
}

//______________________________________________
PHGslMatrix TMutVertexFit::get_trk_cov( unsigned int index ) const
{
  assert( index < _node_list.size() );

  PHGslMatrix g(3,3);
  // first expand momentum qq gain to add c/p cov error from track measurement
  for( unsigned int i=0; i<3; i++ )
    { g(0,i) = g(i,0) =  _node_list[index]._trk_g( i, 0 ); }
  
  // copy the px/pz and py/pz components from the result of the fit
  for( unsigned int i=0; i<2; i++ )
      for( unsigned int j=0; j<2; j++ )
	{ g( i+1, j+1 ) =  _node_list[index]._trk_par._g_qq(i,j); }

  // get momentum at vertex
  // transform 'kalman' gain matrix into mutoo covariance matrix
  PHGslMatrix cov_p(
      TMutKalmanUtil::cov_vtx_kalman_to_mutoo(
        _node_list[index]._charge,
        g.invert(),
        PHVector( get_px( index ), get_py( index ), get_pz( index ) )
      ) );

  // get x,y covariant matrix
  PHGslMatrix cov_g_x(2,2);
  for (int i=0; i<2; i++)
    for (int j=0; j<2; j++)
      cov_g_x(i,j) = _node_list[index]._trk_par._g_qx(i,j);

  PHGslMatrix cov_x(cov_g_x.invert());

  PHGslMatrix cov(5,5);
  for( unsigned int i=0; i<2; i++ )
      for( unsigned int j=0; j<2; j++ )
	cov(i,j) = cov_x(i,j);

  for( unsigned int i=0; i<3; i++ )
      for( unsigned int j=0; j<3; j++ )
	cov(i+2,j+2) = cov_p(i,j);

  return cov;
}
