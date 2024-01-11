// $Id: mFvtxKalFitMC.h,v 1.9 2015/06/11 20:59:17 snowball Exp $
#ifndef __mFvtxKalFitMC_h__
#define __mFvtxKalFitMC_h__

/*!
  \file mFvtxKalFitMC.h
  \brief track fit kalman filter module that works on MC tracks
  \author Melynda Brooks
  \version $Revision: 1.9 $
  \date $Date: 2015/06/11 20:59:17 $
*/

#include<FVTXOO.h>
#include<PHTimeServer.h>
#include<TMutKalmanUtil.h>
#include<TFvtxTrkMap.h>
#include<TMutTrkPar.hh>
#include<TFvtxMCHitMap.h>
#include<TMutMCTrkMap.h>

#include<TMutKalmanFilter.h>
#include <TTree.h>
#include <vector>
#include <PHGslRng.h>
#include<list>
#include<boost/array.hpp>

class PHCompositeNode;
class mFvtxKalFitMCPar;
class PHTrackIntegratorKF;
/*! \ingroup modules */
//! single track kalman filter fit. 
/*!
 single track kalman filter fit. 
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mFvtxKalFitMCPar*</td>
<td> Parameter Table </td>
<td> imFvtxable </td>
</tr>
<tr>
<td> TFvtxTrkMap*</td>
<td> Track IOC</td>
<td> Fvtxable </td>
</table>
*/

#include <SvxSnglPisaHit.h>

class mFvtxKalFitMC
{

  public: 

  //! constructor
  mFvtxKalFitMC(); 
  
  //! destructor
  virtual ~mFvtxKalFitMC();

  //! event method
  virtual PHBoolean event(PHCompositeNode*);	
  
  //! output evaluation tree initialisation 
  void init_tree( void );
  
  //! close output evaluation tree
  void finish_evaluation( void );
    
  private:	

  //! Set IOC pointers
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! Loop over tracks
  void track_loop();
  
  //! returns true if track passes basic checks
  bool accept_track( TFvtxTrkMap::const_pointer trk_ptr ) const;
  
  //! initialize kalmanfilter object starting parameters and internal list of nodes from track pointer
  void init( TFvtxTrkMap::const_pointer trk_ptr, TMutKalmanFilter& kalman_filter );
     	
  //! fit all nodes (iterative prediction/filter/smoothing)
  bool fit_nodes( TMutKalmanFilter& kalman_filter );
 		 	
  //! fill_track. Get best parameters for each coord 
  bool fill_track( TFvtxTrkMap::pointer trk_ptr );
 		 	
  //! extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  bool extrapolate_to_vertex( TFvtxTrkMap::pointer trk_ptr );
 		 	
  //! extrapolate track parameters from last gap to z of first MuID gap
  bool extrapolate_to_mutr( TFvtxTrkMap::pointer trk_ptr );
  
  //! fill track with predicted filtered and smoothed residuals for all nodes
  void fill_tree( void );
  
  //! returns r error (copied from mFvtxTrkFit but left private)
  static double get_r_error();
    
  //! sort pointers of TFvtxMCHit from vertex to muid
  struct coord_less_ftor		
  { 
    bool operator() ( const TFvtxMCHitMap::value_type & coord_0, const TFvtxMCHitMap::value_type & coord_1 ) 
    { 
      return ( ( coord_0.get()->get_z() > 0 ) ?
        ( coord_0.get()->get_z() < coord_1.get()->get_z() ):
        ( coord_0.get()->get_z() > coord_1.get()->get_z() ) );
    } 
  };
    
  //! adds r residual calculated from coordinate to track
  void push_r_residual( TFvtxTrkMap::pointer, const TMutTrkPar&, TFvtxMCHitMap::value_type); 
 
  /*! 
    adds w residual calculated from coordinate to track
    returns the corresponding chi_square contribution: (delta_w/w_fit_error)**2
  */
  double push_w_residual( TFvtxTrkMap::pointer, const TMutTrkPar&, TFvtxMCHitMap::value_type); 
  
  //! reset all nodes
  void reset_nodes( void )
  {
    for( node_iterator it = _nodes.begin(); it != _nodes.end(); it++ )
    it->reset_flags();
  }
    
  //! Timer
  PHTimeServer::timer _timer;		

  //! main analysis node
  PHCompositeNode *_top_node;	
  
  //! pointer to mFvtxKalFitMC parameters module
  mFvtxKalFitMCPar* _mod_par;		
  
  //! pointer to TFvtxTrk object map
  TFvtxTrkMap* _trk_map;				


  /*! 
    KalmanFilter node, deriving deriving from TMutKalmanFilter::Node to 
    add constructors from TMuiClusterO
  */
  class KalmanFilterNode: public TMutKalmanFilter::Node
  {
    
    public:

    //! filled constructor using FVTX mc hits
    KalmanFilterNode( const TFvtxMCHitMap::pointer& coord_ptr, const int xyflag,
      const double phismear, const double rsmear);
    
    //! filled constructor using VTX mc hits
    KalmanFilterNode( const SvxSnglPisaHit& svxhit, const int xyflag, const float zsmear, const float phismear );

    //! filled constructor using generix x,y points (for vertex, for instance)
    KalmanFilterNode( const PHPoint* point, const int xyflag );
    
    //! destructor
    virtual ~KalmanFilterNode( void )
    {}
       
    //! reference to associated TFvtxMCHit, if any
    TFvtxMCHitMap::value_type& get_fvtx_mchit( void )
    { return _fvtx_mchit; }
       
    //! reference to associated TFvtxMCHit, if any
    TFvtxMCHitMap::value_type get_fvtx_mchit( void ) const
    { return _fvtx_mchit; }
      
    //! reference to associated VTX mc hit, if any
    SvxSnglPisaHit get_svx_mchit( void )
    { return _svx_mchit; }
      
    private:
    
    //! the fvtx coordinate
    TFvtxMCHitMap::value_type _fvtx_mchit;
    
    //! the vtx MC hit
    SvxSnglPisaHit _svx_mchit;
  };

  //! shortcut to list of nodes
  typedef std::list< KalmanFilterNode > node_list;
  
  //! shortcut to list of nodes
  typedef node_list::iterator node_iterator;
  
  //! shortcut to list of nodes
  typedef node_list::const_iterator const_node_iterator;
  
  //! shortcut to list of nodes
  typedef node_list::reverse_iterator reverse_node_iterator;

  //! list of nodes belonging to a track
  node_list _nodes;	
  
  //! evaluation filename
  std::string _filename;	 
  
  //! output tree, if any
  TTree *_tree;
  
  //! track arm
  unsigned int _arm;
 
  //! node cage
  unsigned int _cage;
 
  //! node station
  unsigned int _station;
  
  //! node sector
  unsigned int _sector;
  
  //! node column
  unsigned int _column;
  
  //! node residual
  boost::array<double,3> _res;
  
  //! corresponding error
  boost::array<double,3> _res_cov;

  //! node contribution to track chisquare
  double _chi_square;
  
  //! Random number generator
  PHGslRng _rng;

  //! Reference to PHTrackIntegratorKF
  PHTrackIntegratorKF *integrator;

};

#endif /* __mFvtxKalFitMC_HH__ */














