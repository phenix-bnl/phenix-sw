#ifndef TFvtxQuadraticTrackFit_h
#define TFvtxQuadraticTrackFit_h

#include <MUTOO.h>
#include <PHGslMatrix.h>
#include <list>

//! general interface to analytic 2 dimensional straight line fit
class TFvtxQuadraticTrackFit
{
  
  public:
  
  //! constructor
  TFvtxQuadraticTrackFit( void ):
    _verbosity( MUTOO::NONE ),	
    _chisquare( 0 ),
    _state( 6, 1 ),
    _gain( 6, 6 ),
    _z( 0 )
    {}

  //! destructor
  ~TFvtxQuadraticTrackFit( void )
  {}
   
  //! verbosity
  void set_verbosity( const MUTOO::Verbosity verbosity )
  { _verbosity = verbosity; }
  
  //! store necessary matrix for measurement
  class Node
  {
  	
    public:
  	
    //! constructor (take a mutr cluster and a reference z)
    Node( void ):
      _m( 1, 1 ),
      _g( 1, 1 ),
      _h( 1, 6 )
    {};
  			
    //! measurement
    const PHGslMatrix& get_m( void ) const
    { return _m; }
    
    // gain matrix (invert of the covariance matrix)
    const PHGslMatrix& get_g( void ) const
    { return _g; }
    
    //! transfer (projection matrix) from (x,y,z) to measurement coordinates
    const PHGslMatrix& get_h( void ) const
    { return _h; }

  protected:
    
    //! set measurement
    void set_measurement(  const PHGslMatrix& measurement, const PHGslMatrix& measurement_cov )
    { 
      _m = measurement;
      _g = measurement_cov.invert();
    }
    
    //! set h (projection) matrix
    void set_h( const PHGslMatrix& h )
      { _h = h; }
    
  private: 
    
    //! vector of measurement matrices
    PHGslMatrix _m;
    
    //! vector of associated gain matrices
    PHGslMatrix _g;
    
    //! associated transfer matrix
    PHGslMatrix _h;
  
  };
     
  //! shortcut for list of nodes
  typedef std::list< Node > node_list;

  //! add a node to the fit
  void add_node( const Node& node )
  { _nodes.push_back( node ); }
 		
  //! reference z
  void set_z( const double z )
    { _z = z; }
  
  //! reference z
  double get_z( void ) const
    { return _z; }
		
  //! retrieve chisquare
  double get_chisquare( void ) const 
    { return _chisquare; }
  
		//! number of clusters
  int get_n_nodes( void ) const 
    { return _nodes.size(); }
  
  //! degrees of freedom
  int get_ndf( void ) const
    { return get_n_nodes() - 4; }
  
  //! perform the fit. Return true if successful
  bool fit( void );
  
  //! retrieve state vector matrix
  const PHGslMatrix& get_state( void )  const
    { return _state; }
  
  
  //! retrieve gain matrix
  const PHGslMatrix& get_gain( void )  const
    { return _gain; }
  
  //! retrieve nodes
  const node_list& get_nodes( void ) const
    { return _nodes; }

 private:
		
  //! verbosity
  MUTOO::Verbosity _verbosity;
    
  //! road chisquare
  double _chisquare;
  
  //! road parameters (4x1) (state vector)
  PHGslMatrix _state;
  
  //! gain matrix (4x4)
  PHGslMatrix _gain;
  
  //! reference z
  double _z;
  
  //! list of measurement
  node_list _nodes;  
  
};


#endif
