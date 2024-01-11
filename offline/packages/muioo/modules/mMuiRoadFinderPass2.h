// $Id: mMuiRoadFinderPass2.h,v 1.3 2007/03/26 23:20:14 hugo Exp $
#ifndef __MMUIROADFINDERPASS2_HH__
#define __MMUIROADFINDERPASS2_HH__

#include<PHTimeServer.h>

#include<TMutTrkMap.h>
#include<TMuiRoadMapO.h>
#include<TMuiClusterMapO.h>

#include<mMuiRoadFinderPass2Par.h>
#include<TMutKalmanFilter.h>

#include<TTree.h>
#include<TFile.h>

#include<boost/array.hpp>

/**
 * 2nd pass road-finder; Kalman-style approach to go through MUID clusters
 * after MUTR track has been reconstructed.
 */

/*! \ingroup modules */

/*!
  <br>
  <h2>Analysis Module Interface Specification</h2>
  <table>
  <tr>
  <td> Object </td>
  <td> Description </td>
  <td> Privilege </td>
  </tr>
  <tr>
  <td> const mMuiRoadFinderPass2Par*</td>
  <td> Parameter Table </td>
  <td> immutable </td>
  </tr>

  INSERT ADDITIONAL IOC SPECIFICATIONS HERE

  </table>
*/

class mMuiRoadFinderPass2
{
  public: 

  //! constructor
  mMuiRoadFinderPass2(); 

  //! destructor
  virtual ~mMuiRoadFinderPass2(); 

  //! event method
  PHBoolean event(PHCompositeNode*);

  //! output evaluation tree initialisation 
  bool init_tree( void );
  
  //! close output tree
  void close_tree( void )
  { finish_evaluation(); }
  
  //! close output evaluation tree
  void finish_evaluation( void );

 private:  

  // private methods:
  //! Set IOC pointers
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! Loop over tracks
  void track_loop( void );

  //! check if track is accepted as a starting point to the road fit
  bool accept_track( TMutTrkMap::const_pointer trk_ptr ) const;

  //! initialize kalmanfilter object starting parameters and internal list of nodes from track pointer
  //! and fit all nodes (iterative prediction/filter/smoothing)
  bool init_and_fit_nodes( TMutTrkMap::const_pointer trk_ptr, TMuiRoadMapO::pointer road_ptr, 
         TMutKalmanFilter& kalman_filter );

  //! fill_road - get best parameters 
  bool fill_road( TMuiRoadMapO::pointer road_ptr ) const;

  //! fill tree with info for all nodes
  void fill_tree( TMuiRoadMapO::const_pointer road_ptr );

  //! reset all nodes
  void reset_nodes( void )
  {
    for( node_iterator it = _nodes.begin(); it != _nodes.end(); it++ )
    it->reset_flags();
  }

  //! Timer
  PHTimeServer::timer _timer;

  // Interface pointers
  
  //! module parameters
  const mMuiRoadFinderPass2Par* _mod_par;
  
  //! track map
  TMutTrkMap* _trk_map;
  
  //! road map
  TMuiRoadMapO* _road_map;
  
  //! cluster map
  TMuiClusterMapO* _cluster_map;

  /*! 
    KalmanFilter node, deriving deriving from TMutKalmanFilter::Node to 
    add constructors from TMuiClusterO
  */
  class KalmanFilterNode: public TMutKalmanFilter::Node
  {
    
    public:

    //! filled constructor using muid clusters
    KalmanFilterNode( const TMuiClusterMapO::pointer& cluster_ptr );
    
    //! destructor
    virtual ~KalmanFilterNode( void )
    {}
    
    //! reference to associated TMuiClusterO, if any
    TMuiClusterMapO::value_type& get_mui_cluster( void )
    { return _mui_cluster; }
    
    //! reference to associated TMuiClusterO, if any
    TMuiClusterMapO::value_type get_mui_cluster( void ) const
    { return _mui_cluster; }
    
    private:
    
    //! the mui clusters
    TMuiClusterMapO::value_type _mui_cluster; 
    
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
  
  //! output TFile, if any
  TFile *_file;   
  
  //! output tree, if any
  TTree *_tree;
  
  //! road arm
  unsigned int _arm;    
  
  //! node plane  
  unsigned int _plane;    

  //! node orient
  unsigned int _orient;   
  
  //! node residual
  boost::array<double,3> _res;    
  
  //! corresponding error  
  boost::array<double,3> _res_cov;  
  
  //! measurement cov
  double _meas_cov;   
  
  //! node contribution to total chisquare
  double _chi_square; 
  
  //! road depth
  int _depth;        
  
  //! mc road depth 
  int _depth_mc;      

  //! road tree
  TTree *_road_tree;
  
  //! road number of degrees of freedom
  int _ndf;    
  
  // also added to the tree are the total chi_square and the arm

};

#endif /* __MMUIROADFINDERPASS2_HH__ */






