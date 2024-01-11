// $Id: mMutKalFitWithSiliReal.h,v 1.11 2015/06/11 20:59:17 snowball Exp $
#ifndef __mMutKalFitWithSiliReal_h__
#define __mMutKalFitWithSiliReal_h__

/*!
  \file		mMutKalFitWithSiliReal.h
  \brief	 track fit kalman filter module, fitting Mut and Silicon
  \author	Melynda Brooks
  \version $Revision: 1.11 $
  \date		$Date: 2015/06/11 20:59:17 $
*/

#include<MUIOO.h>
#include<MUTOO.h>
#include<PHTimeServer.h>
#include<TMutKalmanUtil.h>
#include<TMutTrkMap.h>
#include<TFvtxTrkMap.h>
#include<TMutTrkPar.hh>
#include<TMutGapCoordMap.h>
#include<TMutCoordMap.h>
#include<TFvtxCoordMap.h>
#include<TFvtxSvxClusterMap.h>
#include<TMuiClusterMapO.h>

#include<TMutKalmanFilter.h>

#include "mMutKalFitWithSiliRealPar.h"
#include <TTree.h>

#include<list>
#include<boost/array.hpp>

#include<mFvtxModuleBase.h>

class PHCompositeNode;
class PHTrackIntegratorKF;
/*! \ingroup modules */
//! single track kalman filter fit, fitting silicon along with muon hits. 
/*!
 single track kalman filter fit, fitting silicon along with muon hits. 
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutKalFitWithSiliRealPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> Track IOC</td>
<td> mutable </td>
</table>
*/

#include <SvxSnglPisaHit.h>

class mMutKalFitWithSiliReal : public mFvtxModuleBase
{

  public: 

  //! constructor
  mMutKalFitWithSiliReal(); 
  
  //! destructor
  virtual ~mMutKalFitWithSiliReal();

  //! event method
  virtual PHBoolean event(PHCompositeNode*);	
  
  //! output evaluation tree initialisation 
  void init_tree( void );
  
  //! close output evaluation tree
  void finish_evaluation( void );

  void init(PHCompositeNode*){};
  void init_run(PHCompositeNode*){};
  void end(PHCompositeNode* top_node);
    
  private:	

  //! Set IOC pointers
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! Loop over tracks
  void track_loop();
  
  //! returns true if track passes basic checks
  bool accept_track( TMutTrkMap::const_pointer trk_ptr );
  
  //! returns true if an fvtx track has been found to associate to this muon track
  bool associate_fvtx_trk( TMutTrkMap::pointer trk_ptr );
  
  //! returns true if an fvtx track has been found to associate to this muon track
  bool fvtx_mutr_match( TMutTrkMap::const_pointer trk_ptr, TFvtxTrkMap::const_pointer fvtx_trk_ptr );
 
  double get_fvtx_mutr_match_cut( double mom );
 
  //! initialize kalmanfilter object starting parameters and internal list of nodes from track pointer
  void init( TMutTrkMap::pointer trk_ptr, TFvtxTrkMap::pointer fvtx_trk_ptr, TMutKalmanFilter& kalman_filter );
     	
  //! fit all nodes (iterative prediction/filter/smoothing)
  bool fit_nodes( TMutKalmanFilter& kalman_filter );
 		 	
  //! check_track. Determine fit for this track and how it compares to best so far:
  bool check_track( TMutTrkMap::pointer trk_ptr, TFvtxTrkMap::pointer fvtx_trk_ptr, bool first, TMutTrkPar& );
 		 	
  //! extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  bool extrapolate_to_vertex( TMutTrkPar trk_par, TMutTrkPar& );
 		 	
  //! extrapolate track parameters from last gap to z of first MuID gap
  bool extrapolate_to_muid( TMutTrkMap::pointer trk_ptr );
  
  //! fill track with predicted filtered and smoothed residuals for all nodes
  void fill_tree( void );
  
  //! returns r error (copied from mMutTrkFit but left private)
  static double get_r_error();
    
  //! sort pointers of TMutCoord from vertex to muid
  struct coord_less_ftor		
  { 
    bool operator() ( const TMutCoordMap::value_type & coord_0, const TMutCoordMap::value_type & coord_1 ) 
    { 
      return ( ( coord_0.get()->get_arm() == MUTOO::North ) ?
        ( coord_0.get()->get_mean_z() < coord_1.get()->get_mean_z() ):
        ( coord_0.get()->get_mean_z() > coord_1.get()->get_mean_z() ) );
    } 
  };
    
  //! sort pointers of TFvtxCoord from vertex to muid
  struct fvtx_coord_less_ftor		
  { 
    bool operator() ( const TFvtxCoordMap::value_type & coord_0, const TFvtxCoordMap::value_type & coord_1 ) 
    { 
      return ( ( coord_0.get()->get_arm() == MUTOO::North ) ?
        ( coord_0.get()->get_mean_z() < coord_1.get()->get_mean_z() ):
        ( coord_0.get()->get_mean_z() > coord_1.get()->get_mean_z() ) );
    } 
  };
    
  //! sort pointers of TMuiClusterO from vertex to muid
  struct mui_clus_less_ftor		
  { 
    bool operator() ( const TMuiClusterMapO::value_type & clus_0, const TMuiClusterMapO::value_type & clus_1 ) 
    { 
      return ( ( clus_0.get()->get_arm() == MUIOO::North ) ?
        ( clus_0.get()->get_mean_z() < clus_1.get()->get_mean_z() ):
        ( clus_0.get()->get_mean_z() > clus_1.get()->get_mean_z() ) );
    } 
  };

  //! adds r residual calculated from coordinate to track
  void push_r_residual( TMutTrkMap::pointer, const TMutTrkPar&, TMutCoordMap::value_type); 
 
  /*! 
    adds w residual calculated from coordinate to track
    returns the corresponding chi_square contribution: (delta_w/w_fit_error)**2
  */
  double push_w_residual( TMutTrkMap::pointer, const TMutTrkPar&, TMutCoordMap::value_type); 
  
  //! calculate anode correction for all nodes
  void calculate_anode_corrections( void );
  
  //! reset all nodes
  void reset_nodes( void )
  {
    for( node_iterator it = _nodes.begin(); it != _nodes.end(); it++ )
    it->reset_flags();
  }
  
  /*! \brief
    best associated road for a given track (is based on DG0).
    Is needed to have the muid hits associated to the track.
    The corresponding TMuiClusterO are associated to the track and used in the fit.
    The other roads are dissacitiated.
  */
  void associate_road( TMutTrkMap::pointer ) const;
    
  //! Timer
  PHTimeServer::timer _timer;		

  //! main analysis node
  PHCompositeNode *_top_node;	
  
  //! pointer to mMutKalFitWithSiliReal parameters module
  mMutKalFitWithSiliRealPar* _mod_par;		
  
  //! pointer to tMutTrk object map
  TMutTrkMap* _trk_map;				

  //! pointer to tFvtxTrk object map
  TFvtxTrkMap* _fvtx_trk_map;				

  /*! 
    KalmanFilter node, deriving deriving from TMutKalmanFilter::Node to 
    add constructors from TMutCoord and TMuiClusterO
  */
  
  class KalmanFilterNode: public TMutKalmanFilter::Node
  {
    
    public:

    //! filled constructor using mutr coords
    KalmanFilterNode( const TMutCoordMap::pointer& coord_ptr );

    //! filled constructor using muid clusters
    KalmanFilterNode( const TMuiClusterMapO::pointer& cluster_ptr );
    
    //! filled constructor using FVTX hits
    KalmanFilterNode( const TFvtxCoordMap::pointer& coord_ptr, const int phiflag );

    //! filled constructor using VTX Pisa hits
    KalmanFilterNode( const SvxSnglPisaHit& svxhit, const int xyflga, const float zsmear,
                   const float phismear );

    //! filled constructor using VTX cluster (x-y-z space)
    KalmanFilterNode( const SvxCluster& clus, const int xyflag, const double phi_error,
    const double r_error, double z_error, double dxdz, double dydz );

    //! filled constructor using VTX cluster (r-phi-z space)
    KalmanFilterNode( const SvxCluster& clus, const int phiflag, const double phi_error,
    const double r_error, double z_error, double drdz );

    //! destructor
    virtual ~KalmanFilterNode( void )
    {}
    
    //! true if measurement comes from TMutCoord
    bool has_coord( void ) const
    { return _has_coord; }
    
    //! true if measurement comes from TMuiClusterO
    bool has_mui_cluster( void ) const
    { return _has_mui_cluster; }
    
    //! reference to associated TMutCoord, if any
    TMutCoordMap::value_type& get_coord( void )
    {
      if( !_has_coord ) 
      throw std::logic_error( DESCRIPTION( "no associated coord" ) );
      return _coord;
    }
    
    //! reference to associated TMuiClusterO, if any
    TMuiClusterMapO::value_type& get_mui_cluster( void )
    {
      if( !_has_mui_cluster ) 
      throw std::logic_error( DESCRIPTION( "no associated mui cluster" ) );
      return _mui_cluster;
    }
    
    //! reference to associated TMutCoord, if any
    TMutCoordMap::value_type get_coord( void ) const
    {
      if( !_has_coord ) 
      throw std::logic_error( DESCRIPTION( "no associated coord" ) );
      return _coord;
    }
    
    //! reference to associated TMuiClusterO, if any
    TMuiClusterMapO::value_type get_mui_cluster( void ) const
    {
      if( !_has_mui_cluster ) 
      throw std::logic_error( DESCRIPTION( "no associated mui cluster" ) );
      return _mui_cluster;
    }
    
    //! reference to associated TFvtxCoord, if any
    TFvtxCoordMap::value_type get_fvtx_coord( void ) const
    {
      return _fvtx_coord;
    }
    
    //! reference to associated vtx mc hit, if any
    SvxSnglPisaHit get_svx_mchit( void ) const
    {
      return _svx_mchit;
    }
    
    private:
    
    //! true if coord makes sense
    bool _has_coord;		
    
    //! true if muid cluster makes sense		
    bool _has_mui_cluster;	
    
    //! the coord (measurement)
    TMutCoordMap::value_type _coord;	
    
    //! the mui clusters
    TMuiClusterMapO::value_type _mui_cluster; 

                //! the TFvtxCoord
                TFvtxCoordMap::value_type _fvtx_coord;
    
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
 
  //! output tree_match
  TTree *_tree_match;

  //! init
  bool _init_done;

  //! track arm
  unsigned int _arm;
  
  //! node station
  unsigned int _station;
  
  //! node gap
  unsigned int _gap;
  
  //! node cathode
  unsigned int _cathode;
  
  //! node residual
  boost::array<double,3> _res;
  
  //! corresponding error
  boost::array<double,3> _res_cov;

  //! node contribution to track chisquare
  double _chi_square;

  //! FVTX and extrapolated positions, mut momentum
  double _extrapx;
  double _extrapy;
  double _extrapz;
  double _fvtxx;
  double _fvtxy;
  double _fvtxz;
  double _mut_px;
  double _mut_py;
  double _mut_pz;
  double _z_vertex;

  //! matching distance
  double _distance;

  //! matching angle
  int _match_arm;

  //! matching angle
  double _dangle;

  //! matching dx
  double _match_dx;

  //! matching dy
  double _match_dy;

  //! mc track px
  double _mc_px;

  //! mc track py
  double _mc_py;

  //! mc track pz
  double _mc_pz;

  //! Pointer to PHTrackIntegrator
  PHTrackIntegratorKF *integrator;

};


#endif /* __MMutKalFitWithSiliReal_HH__ */















