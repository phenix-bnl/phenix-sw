#ifndef __mMutKalFit_h__
#define __mMutKalFit_h__

// $Id: mMutKalFit.h,v 1.44 2009/04/27 11:48:42 hpereira Exp $

/*!
  \file    mMutKalFit.h
  \brief   track fit kalman filter module
  \author  Hugo Pereira
  \version $Revision: 1.44 $
  \date    $Date: 2009/04/27 11:48:42 $
*/

#include <MUIOO.h>
#include <MUTOO.h>
#include <PHTimeServer.h>
#include <TMutKalmanUtil.h>
#include <TMutTrkMap.h>
#include <TMutTrkPar.hh>
#include <TMutGapCoordMap.h>
#include <TMutCoordMap.h>
#include <TMuiClusterMapO.h>
#include <TMutKalmanFilter.h>
#include <PHGslMatrix.h>
#include <TTree.h>

#include <list>
#include <string>
#include <boost/array.hpp>

#include "mMutKalFitPar.h"

class PHCompositeNode;

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
<td> const mMutKalFitPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> Track IOC</td>
<td> mutable </td>
</table>
*/

class mMutKalFit
{

  public:

  //! constructor
  mMutKalFit();

  //! destructor
  virtual ~mMutKalFit();

  //! event method
  virtual PHBoolean event(PHCompositeNode*);

  //! output evaluation tree initialisation
  bool init_evaluation( void );

  //! close output evaluation tree
  void finish_evaluation( void );

  private:

  //! Set IOC pointers
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! load vertex 
  void load_ext_vertex( PHCompositeNode* top_node );
  
  //! Loop over tracks
  void track_loop();

  //! returns true if track passes basic checks
  bool accept_track( TMutTrkMap::const_pointer trk_ptr ) const;

  //! initialize kalmanfilter object starting parameters and internal list of nodes from track pointer
  void init( TMutTrkMap::pointer trk_ptr, TMutKalmanFilter& kalman_filter );

  //! fit all nodes (iterative prediction/filter/smoothing)
  /*! return the number of iterations performes, or 0 if the fit fails */
  unsigned int fit_nodes( TMutKalmanFilter& kalman_filter );

  //! fill_track. Get best parameters for each coord
  bool fill_track( TMutTrkMap::pointer trk_ptr );

  //! extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  bool extrapolate_to_vertex( TMutTrkMap::pointer trk_ptr );

  //! extrapolate track parameters from last gap to z of first MuID gap
  bool extrapolate_to_muid( TMutTrkMap::pointer trk_ptr );

  //! fill track with predicted filtered and smoothed residuals for all nodes
  void fill_evaluation_tree( void );

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

  //! pointer to mMutKalFit parameters module
  mMutKalFitPar* _mod_par;

  //! pointer to tMutTrk object map
  TMutTrkMap* _trk_map;

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

    private:

    //! true if coord makes sense
    bool _has_coord;

    //! true if muid cluster makes sense
    bool _has_mui_cluster;

    //! the coord (measurement)
    TMutCoordMap::value_type _coord;

    //! the mui clusters
    TMuiClusterMapO::value_type _mui_cluster;

  };

  //!@name kalman filter nodes
  //@{
  
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

  //@}
  
  //! vertex z
  double _vertex_z;
  
  //!@name evaluation
  //@{

  //! evaluation filename
  std::string _filename;

  //! output tree, if any
  TTree *_residual_tree;

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

  //! vertex extralation tree
  TTree* _vertex_extrapolation_tree;

  //! upstream momentum (i.e. at vertex)
  boost::array<double, 3> _p_up;

  //! upstream total momentum (i.e. at vertex)
  double _p_tot_up;

  //! downstream momentum (i.e. at station 1)
  boost::array<double, 3> _p_down;

  //! downstream total momentum (i.e. at station 1)
  double _p_tot_down;

  //! errors (c/p, px/pz, py/pz, x, y) upstream (i.e. at vertex)
  boost::array<double, 5> _error_up;

  //! errors (c/p, px/pz, py/pz, x, y ) downstream (i.e. at station 1)
  boost::array<double, 5> _error_down;

  //@}

};


#endif /* __MMUTKALFIT_HH__ */
