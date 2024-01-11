#ifndef __TMutVertexFit_h__
#define __TMutVertexFit_h__

// $Id: TMutVertexFit.h,v 1.7 2013/12/26 08:13:29 slash Exp $

/*!
\file    TMutVertexFit.h
\brief  tracks and event vertex linearized fit
\author  Hugo Pereira
\version $Revision: 1.7 $
\date    $Date: 2013/12/26 08:13:29 $
*/

#include <PHGslMatrix.h>
#include <PHPoint.h>
#include <MUTOO.h>

#include <cassert>
#include <iostream>
#include <vector>
#include <cmath>

/*! \ingroup classes */
//! tracks and event vertex linearized fit
/*! 
this is the base class that contains all the maths, but has no interface
to set the input track. These must be set by deriving the class
*/
class TMutVertexFit
{

  public:
  
  //! constructor
  TMutVertexFit( void );
 
  //! destructor
  virtual ~TMutVertexFit( void )
  {}
  
  //! verbosity
  int verbosity( void ) const
  { return _verbosity; }
  
  //! verbosity
  void set_verbosity( int value )
  { _verbosity = value; }
    
  //! add event vertex information
  /*! 
  This method is provided for convenience. 
  Only the vertex z and z error are passed as argument.
  Error on x and y is set at 1cm
  */
  virtual void add_vertex( const double& z_vertex, const double& z_vertex_error )
  { 
   static const double vtx_xy_error( 1 );
    add_vertex( PHPoint( 0, 0, z_vertex ), PHPoint( vtx_xy_error, vtx_xy_error, z_vertex_error ) );
  }
    
  //! add event vertex information
  /*! takes all three (x,y,z) and corresponding (diagonal) errors as argument */
  virtual void add_vertex( const PHPoint& vertex, const PHPoint& vertex_error );
      
  //! fit the vertex
  virtual bool fit( void );
    
  //! retrieve chisquare from nodes
  virtual double get_chisquare( void ) const;

  //! retrieve number of degrees of freedom
  virtual int get_ndf( void ) const
  {
    /* 
    4 measurements/track
    3 measurements for external vertex (if any)
    2 parameters per track
    3 parameters for the fitted vertex position
    */
    return 4*_node_list.size() + ((_has_ext_vtx)?3:0) - 2*_node_list.size() - 3;
  }
  
  //! retrieve chisquare from nodes
  virtual double get_chisquare_pdf( void ) const
  { return get_chisquare()/get_ndf(); }
      
  //! retrieve track momentum along x
  virtual double get_px( unsigned int index ) const
  {
    assert( index < _node_list.size() );
    return _node_list[index]._trk_par._q(0,0)*get_pz( index );
  }
  
  //! retrieve track momentum along y
  virtual double get_py( unsigned int index ) const
  {
    assert( index < _node_list.size() );
    return _node_list[index]._trk_par._q(1,0)*get_pz( index );
  }
  
  //! retrieve track momentum along z
  virtual double get_pz( unsigned int index ) const
  {
    assert( index < _node_list.size() );
    return _node_list[index]._pz_sign*_node_list[index]._ptot/
      std::sqrt( 
        MUTOO::SQUARE( _node_list[index]._trk_par._q(0,0) ) +
        MUTOO::SQUARE( _node_list[index]._trk_par._q(1,0) ) +
        1
      );
  }
  
  //! number of registered tracks
  virtual unsigned int get_trk_count( void ) const
  { return _node_list.size(); }
  
  //! retrieve track position along x
  virtual double get_trk_x( unsigned int index ) const
  {
    assert( index < _node_list.size() );
    return _node_list[index]._p(2,0);
  }
  
  //! retrieve track position along y
  virtual double get_trk_y( unsigned int index ) const
  {
    assert( index < _node_list.size() );
    return _node_list[index]._p(3,0);
  }
  
  //! retrieve track position along z
  virtual double get_trk_z( unsigned int index ) const
  {
    assert( index < _node_list.size() );
    return _node_list[index]._z;
  }
  
  //! retrieve track covariant matrix
  virtual PHGslMatrix get_trk_cov( unsigned int index ) const;

  //! retrieve vertex position along x
  virtual double get_vtx_x( void ) const
  { return _vtx_par._x(0,0); }
  
  //! retrieve vertex position along y
  virtual double get_vtx_y( void ) const
  { return _vtx_par._x(1,0); }
  
  //! retrieve vertex position along z
  virtual double get_vtx_z( void ) const
  { return _vtx_par._x(2,0); }

  //! covariance matrix (7x7)
  virtual double get_vtx_cov( unsigned int i, unsigned int j )
  { return _vtx_cov( i, j ); }
  
  //! retrieve vertex dimuon mass
  virtual double get_mass( void ) const;
 
  //! retrieve vertex dimuon rapidity
  virtual double get_rapidity( void ) const;
 
  //! print vertex
  virtual void print( std::ostream& out = std::cout ) const;
  
  protected:
  
  //!@name vertex fit internal classes
  //@{
  
  //! vertex parameter (x, y, z)
  class VertexPar 
  {
    
    public:
    
    //! empty constructor
    VertexPar( void ): 
      _x( PHGslMatrix( 3, 1 ) ),
      _g( PHGslMatrix( 3, 3 ) ) {}	
    
    //! filled constructor
    VertexPar( const PHGslMatrix &x ): 
      _x( x ),
      _g( PHGslMatrix( 3, 3 ) ) {}	
      
    //! dumps object to stream
    void print( std::ostream& out = std::cout ) { 
      MUTOO::PRINT( out, "TMutVertexFit::VertexPar");
      out << "position: (" << _x(0,0) << "," << _x(1,0) << "," << _x(2,0) << ").\n"; 
      out << "gain:\n" << _g; 
      MUTOO::PRINT( out, "**");
    }
    
    //! vertex position (x, y, z)
    PHGslMatrix _x;	
    
    //! gain matrix (xx correlations)
    PHGslMatrix _g; 
  };
  
  //! track parameters (dxdz, dydz)
  class TrackPar 
  {
    
    public: 
    
    //! empty constructor
    TrackPar( void ): 
      _q( PHGslMatrix( 2, 1 ) ),
      _g_qq( PHGslMatrix( 2, 2 ) ),
      _g_qx( PHGslMatrix( 2, 3 ) )
     {}	

    //! filled constructor
    TrackPar( const PHGslMatrix &q ): 
      _q( q ),
      _g_qq( PHGslMatrix( 2, 2 ) ),
      _g_qx( PHGslMatrix( 2, 3 ) )
      {}	
      
    //! dumps object to stream
    void print( std::ostream& out = std::cout ) { 
      MUTOO::PRINT( out, "TMutVertexFit::TrackPar");
      out << "momentum: (" << _q(0,0) << "," << _q(1,0) << ").\n";
      out << "qq gain: \n" << _g_qq; 
      out << "qx gain: \n" << _g_qx; 
      MUTOO::PRINT( out, "**");
    }
    
    //! slopes at vertex (dxdz, dydz)
    PHGslMatrix _q;	
    
    //! gain matrix (qq correlations)
    PHGslMatrix _g_qq; 
    
    //! gain matrix (qx correlations)
    PHGslMatrix _g_qx; 
    
  };		

  //! input track node
  /*! stores the track parameters and the matrices needed for the fit */
  class Node
  {
    
    public:
    
    //! empty constructor
    Node( void ):
      _p( PHGslMatrix( 4, 1 ) ),
      _g( PHGslMatrix( 4, 4 ) ),
      _charge( 1 ),
      _ptot( 0 ),
      _trk_g( PHGslMatrix( 5, 5 ) ),
      _z( 0 ),
      _chisquare( 0 ),
      _a( PHGslMatrix( 4, 3 ) ),
      _b( PHGslMatrix( 4, 2 ) ),
      _c( PHGslMatrix( 4, 1 ) ),
      _bgb_inv( PHGslMatrix(2,2) ) 
      {}
        
    //! dumps object to stream
    void print( std::ostream& out = std::cout ) { 
      MUTOO::PRINT( out, "TMutVertexFit::Node");
      out << "parameters: (" << _p(0,0) << "," << _p(1,0) << "," << _p(2,0) << "," << _p(3,0) << ").\n"; 
      out << "gain:\n" << _g; 
      MUTOO::PRINT( out, "**");
    }
    
    //! state vector (dxdz, dydz, x, y)
    PHGslMatrix _p; 
    
    //! state vector covariance matrix (dxdz, dydz, x, y)
    PHGslMatrix _g; 

    //! track charge
    int _charge;
    
    //! total momentum (not fitted since magnetic field us 0)
    double _ptot;   
    
    //! initial track covariance matrix (c/p, dxdz, dydz, x, y)
    PHGslMatrix _trk_g;
    
    //! z momentum sign (equivalent to the arm of the track)
    int _pz_sign;
    
    //! z measurement 
    double _z;      
    
    //! chi square increment for this node
    double _chisquare; 
    
    //! derivatives of track state vector wrt vertex position
    PHGslMatrix _a; 
    
    //! derivatives of track state vector wrt momentum at vertex
    PHGslMatrix _b; 
    
    //! const matrix from linearisation.
    PHGslMatrix _c; 
    
    //! track parameter (dxdz, dydz) fitted
    TrackPar _trk_par; 
    
    // following matrices are stored to save calculation time
    //! (b^t.g.b)^{-1}
    PHGslMatrix _bgb_inv; 
  };
  
  //@}
  
  //! add node to the list
  virtual void add_node( const Node& node )
  { _node_list.push_back( node ); }
  
  /*! \brief 
    convert 5x5 covariance matrix (x,y,px,py,pz) into 4x4 
    (px/pz, py/pz, x,y) suited for vertex fit
  */
  virtual PHGslMatrix convert_trk_cov( const PHGslMatrix& cov, const PHGslMatrix &p ) const;
  
  private:
 
  //! iterator over list of nodes
  typedef std::vector< Node >::iterator node_iterator;
  
  //! iterator over list of nodes
  typedef std::vector< Node >::const_iterator const_node_iterator;
  
  //! list of nodes structure to keep track, and kf information
  std::vector< Node > _node_list; 
  
  //! fit the nodes; returns certex parameters
  VertexPar iterative_fit( const VertexPar & vtx_par, bool first );

  /*! 
    \fn void linearize( Node&, const VertexPar& x0, const TrackPar& q0);
    calculate linearisation matrices _a _b and _c associated to node using straight track model to get derivatives
    \par node the node whose matrices are to be calculated
    \par x0 position linearization point
    \par q0 momentum linearization point
  */
  void linearize( Node&, const VertexPar& x0, const TrackPar& q0);
  
  //! calculate the gain matrices for the fitted track parameters
  void calculate_gain_matrices( VertexPar &vtx_par );
  
  //! dimuon vertex covariance matrix
  void fill_vertex_covar( void );
  
  //! true if external vertex has been set
  bool _has_ext_vtx;
  
  //! external vertex information, if any
  VertexPar _ext_vtx_par; 

  //! fitted vertex parameters
  VertexPar _vtx_par;

  //! dimuon vertex covariance matrix
  PHGslMatrix _vtx_cov;
  
  //! module verbosity
  int _verbosity;

};

#endif
