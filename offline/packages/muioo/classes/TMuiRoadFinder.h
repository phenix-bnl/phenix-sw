//////////////////////////////////////////////////////////////////
//
// Utility class: TMuiRoadFinder
// (based on TMutStubFinder by S. Kelly)
// Description: Encapsulates the road finding algorithm
//              used by the mMuiFindRoad module
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUIROADFINDER_H__
#define __TMUIROADFINDER_H__

#include <iostream>
#include<list>
#include<set>
#include<TDataType.h>
#include<TMuiClusterMapO.h>
#include<TMutFitPar.hh>
#include<TMutTrackUtil.h>
#include<mMuiFindRoadPar.h>
#include<PHPoint.h>
#include<TNtuple.h>
#include<TFile.h>
#include"MUIOO.h"

/*! \ingroup classes */

//! Encapsulates the road finding algorithm 
/*! 

  This class encapsulates the road finding algorithm used by the
  mMuiRoadFinder module.  The algorithm uses an
  adaptive forward search to associate TMuiClusterO objects with a class
  scope Road object.  Upon completion of the first search the list
  of TMuiClusterO can be reversed and the algorithm is run again.
  (w. different seeds)
  Duplicate Roads (roads with the same hit set) are removed from the
  internal list as are "includes", ie roads that have hit or point
  sets that are subsets of other roads.  Running the algorithm twice
  is done to minimize the sensitivity to noise hits added early in the
  search.  The list of class scope road objects held internally by 
  the class is available via an non const member function.  This is
  to allow external modules to splice the internal list.  This is
  quicker than copying but make the class non-const correct.
*/

class TMuiRoadFinder
{
 public:  
  
  class Road;
  typedef std::list<Road> road_list;
  typedef std::pair<float,float> road_window;
  enum WindowEnum { IN_WINDOW, LOW_WINDOW, HIGH_WINDOW };

  TMuiRoadFinder() {}

  /*! 
    Find roads in both arms with no windows 
  */
  void find(TMuiClusterMapO* cluster_map);
  
  /*! 
    Find roads at specified location in specified r, theta window.  If
    no windows are specified the algorithm will find all roads at given
    location.  The theta radians [0-PI], the phi interval is a sub
    interval of the range [0-2PI] in radians.
  */  
  void find(TMuiClusterMapO* cluster_map,
	    UShort_t arm, 
	    bool use_window = false,
	    const road_window& theta_window = std::make_pair(0,0),
	    const road_window& phi_window = std::make_pair(0,0));
  
  /*! 
    Returns a reference to the list of found roads.  This needs to be
    a non-const reference to allow envokers of the algorithm to splice
    elements from this list.  
  */
  road_list& get_road_list() { return _road_list; }
  
  /*! 
    Clears the road list.
  */
  void clear_road_list() { _road_list.clear(); }

  /*! 
    Get a list of coordinates that intersect the given theta/phi window 
  */
  std::list<TMuiClusterMapO::value_type> 
  get_coords_in_window(TMuiClusterMapO* cluster_map,
		       UShort_t arm,
		       const road_window& 
		       theta_window,
		       const road_window& 
		       phi_window);
  //! sets stub evaluation flag
  static void set_do_evaluation( bool value )
    { _do_evaluation = value; }
  
  //! retrieve stub evaluation flag
  static bool get_do_evaluation( void )
    { return _do_evaluation; }  

  //! Fill evaluation ntuple from road finders current state
  void evaluate();

  //! Initialize evaluation ntuple
  static bool initialize_evaluation();  

  //! Write evaluation ntuple
  static void finish_evaluation();
                                                      
  /*! 
    Deluge of info on what the algorithm is doing (DEBUG only) 
  */
  static void set_verbosity(MUIOO::Verbosity value) { 
    _verbosity = value;
    Road::set_verbosity(value);
  }
  
  /*!
    Run the road finding algorithm forwards in absolute z, but
    with reversed seeds, 
    sort and remove duplicates. (Slower but less sensitive to
    inefficiencies)
  */
  static void set_reversed_algo(bool reverse_algo) { 
    _reverse_algo = reverse_algo;
  }
  
  /*! 
    Point project cut applied with road has point.
  */
  static void set_dca_cut(double dca_cut) { 
    Road::set_dca_cut(dca_cut);
  }
  
  /*!
    proximity cut applied when road has no point.
  */
  static void set_prox_cut(double prox_cut) { 
    Road::set_prox_cut(prox_cut);
  }

  /*!
    orientations have different z-values: allow for this in reco.
  */
  static void set_orient_z_dist(double orient_z_dist) { 
    Road::set_orient_z_dist(orient_z_dist);
  }

  /*!
    width of tube; parameter used when checking distances
  */
  static void set_tube_width(double tube_width) { 
    Road::set_tube_width(tube_width);
  }

  //! Accessor for max number of roads before bifurcate error.        
  static UShort_t get_max_n_roads() {
    return _max_n_roads;
  }

  //! Modifier for max number of roads before bifurcate error.                  
  static void set_max_n_roads(UShort_t max) {
    _max_n_roads = max;
  }
  
  /*!
    Minimum hits in total (all planes)
  */
  static void set_min_cluster(UShort_t min_cluster) { 
    _min_cluster = min_cluster;
  }

  /*!
    Maximum number of clusters shared beteween two kept roads 
  */
  static void set_max_cluster_share(UShort_t max_cluster_share) { 
    _max_cluster_share = max_cluster_share;
  }

  /*!
    Minimum points in total (all planes)
  */
  static void set_min_point(UShort_t min_point) { 
    _min_point = min_point;
  }
  
  /*! 
    Class scope road object 
  */
  class Road {
    
  public:    
        
    struct cluster_less_ftor
    {
      bool operator()(TMuiClusterMapO::value_type cluster1, 
		      TMuiClusterMapO::value_type cluster2)
      {
	return (cluster1.get()->get_key().get_obj_key() < 
		cluster2.get()->get_key().get_obj_key()); 
      } 
    };   
    //! shortcut for list of clusters + sorting method    
    typedef std::set<TMuiClusterMapO::value_type, cluster_less_ftor> cluster_list;

    //! shortcut for list of cuts
    typedef std::vector<PHPoint> point_list;

    //! constructor    
    Road(TMuiRoadFinder* finder,
	 const TMuiClusterMapO::pointer cluster_ptr,
	 bool use_window,
	 const road_window& theta_window,
	 const road_window& phi_window) :
      _finder(finder),
      _arm(cluster_ptr->get()->get_arm()),
      _theta_window(theta_window),
      _phi_window(phi_window),
      _has_window(use_window),
      _complete(false),
      _status(true),
      _bifurcate(0)
      {
	_clusters.insert(*cluster_ptr);
      }
    
    //! constructor    
    Road(TMuiRoadFinder* finder, const TMuiClusterMapO::pointer cluster_ptr):
      _finder(finder),
      _arm(cluster_ptr->get()->get_arm()),
      _theta_window(std::make_pair(0,0)),
      _phi_window(std::make_pair(0,0)),
      _has_window(false),
      _complete(false),
      _status(true),
      _bifurcate(0)
      {
	_clusters.insert(*cluster_ptr);
      }
    
    //! retrieve associated fit parameters
    const TMutFitPar & get_fit_par() const  {return _fit_par;}

    //! retrieve number of associated clusters
    size_t get_n_cluster() const {return _clusters.size();}    

    //! retrieve number of associated points
    size_t get_n_point() const {return _points.size();}    
    
    //! add a cluster to the road
    bool add_cluster(TMuiClusterMapO::pointer cluster_ptr);  

    //! bifurcation
    bool bifurcate(TMuiClusterMapO::pointer cluster_ptr);      
    
    UShort_t get_arm() const {return _arm;}
    
    // Non-const so it can be sorted by an external ftor
    //
    cluster_list& get_cluster_list() { return _clusters; }
    point_list& get_point_list() { return _points; }
    road_window get_theta_window() const { return _theta_window; }
    road_window get_phi_window() const { return _phi_window; }
    void set_theta_min(double theta_min) { _theta_window.first = theta_min; }
    void set_theta_max(double theta_max) { _theta_window.second = theta_max; }
    void set_phi_min(double phi_min) { _phi_window.first = phi_min; }
    void set_phi_max(double phi_max) { _phi_window.second = phi_max; }

    // Return the polar radius of the road extrapolated to requested z
    //
    double get_r_at_z(double z) const {
      PHPoint p = TMutTrackUtil::linear_track_model(&_fit_par,z);
      return std::sqrt(MUIOO::SQUARE(p.getX()) + MUIOO::SQUARE(p.getY()));
    }
    
    //! returns angle wrt beam axis
    double get_theta() const {
      double r = std::sqrt(MUIOO::SQUARE(_fit_par.get_x()) +
			   MUIOO::SQUARE(_fit_par.get_y()));
      return atan2(r,std::fabs(_fit_par.get_z()));
    }
    
    //! returns azimuth angle
    double get_phi() const {
      return atan2(_fit_par.get_y(), _fit_par.get_x());
    }
    
    //! set cut on distance of closest approach
    static void set_dca_cut(double dca_cut){
      _dca_cut = dca_cut;
    }
    
    //! set cut on proximity perp. to the tubes
    static void set_prox_cut(double prox_cut){
      _prox_cut = prox_cut;
    }

    //! set value for distance between orientations
    static void set_orient_z_dist(double orient_z_dist) { 
      _orient_z_dist = orient_z_dist;
    }

    //! set value for width of tubes
    static void set_tube_width(double tube_width) { 
      _tube_width = tube_width;
    }

    //! set Road verbosity
    static void set_verbosity(MUIOO::Verbosity value){
      _verbosity = value;
    }

    bool is_complete() const { return _complete; }
    void set_complete(bool complete) { _complete = complete; }
    //! return road status    
    bool get_status() const {return _status; }
    //! defines road status    
    void set_status(bool status) {_status = status;}
    //! bifurcation flag 
    UShort_t get_bifurcate() const {return _bifurcate; }
    void set_bifurcate(UShort_t bifurcate) {_bifurcate = bifurcate;}

    //! returns true if this road falls in theta range
    bool check_theta_window(const road_window& theta_window) const;
    //! returns true if this road falls in phi range
    bool check_phi_window(const road_window& phi_window) const;
    
    //! print road informations
    void print() const { 
      std::cout << "road: " ;
      // clusters
      cluster_list::const_iterator iter = _clusters.begin();
      std::cout << " nclusters " << _clusters.size() << std::endl;
      for(;iter!=_clusters.end();++iter){
	std::cout << " clusterkey " << iter->get()->get_key().get_obj_key() 
		  << std::endl;
	iter->get()->print();
      }
      // points
      point_list::const_iterator piter = _points.begin();
      std::cout << " npoints " << _points.size() << std::endl;
      for(;piter!=_points.end();++piter){
	std::cout << " point " << *piter
		  << std::endl;
	piter->print();
      }
      std::cout << " status " <<  _status << std::endl;
      std::cout << std::endl;
    }

    static void initialize_evaluation();
    static void finish_evaluation();

  private: 
    
    // Private Methods
    //
    PHPoint make_point(const TMuiClusterMapO::pointer) const;
    void update_road(TMuiClusterMapO::pointer);
    void update_road(TMuiClusterMapO::pointer, const PHPoint& point);
    bool test_window(const PHPoint&) const;
    bool check_parallel(const TMuiClusterMapO::pointer) const ;
    PHPoint project(double z) const;
    bool check_proximity(TMuiClusterMapO::pointer cluster_ptr) const;
    bool check_dca(const TMuiClusterMapO::pointer cluster_ptr, const PHPoint&) const;
    bool check_slope() const;
    bool unique_check(TMuiClusterMapO::pointer cluster_ptr) const;
    
    // Pointer to parent algorithm
    //
    TMuiRoadFinder* _finder;
    
    // Locators
    //
    UShort_t _arm;
    
    // Window
    //
    road_window _theta_window;
    road_window _phi_window;
    bool _has_window;
    bool has_window() const { return _has_window; }
    
    // Complete Tag
    //
    bool _complete;
    bool _status;
    UShort_t _bifurcate;

    // Cuts
    //
    static double _dca_cut;
    static double _prox_cut;
    static bool _use_slope_cuts;

    // Offset between orientations in z
    //
    static double _orient_z_dist;

    // Width of tube
    //
    static double _tube_width;
    
    // Road clusters
    //
    cluster_list _clusters;  
    
    // Road points
    //
    point_list _points;  

    // Road parameters
    //
    TMutFitPar _fit_par;      
    
    // Verbosity
    //
    static MUIOO::Verbosity _verbosity;

    // Evaluation
    //
    static bool _evaluation;
    static TFile* _file;
    static TNtuple* _ntuple;
    static boost::array<float,20> _eval_data;

  }; // class Road
  
  struct road_print_ftor
  {
    void operator() (const Road& road) {
      road.print();
    }
  };
  
  struct road_bad_status_ftor
  {
    bool operator()(const Road& road) {
      return !road.get_status();
    }
  };   
  
  //! returns true if associated clusters have same keys strictly
  struct road_equal_ftor;  
  
  //! sort stubs according to associated set of clusters key lexycographic order
  struct road_less_ftor;
  
  struct point_less_ftor
  {
    bool operator()(const PHPoint& point1, const PHPoint& point2)
    {
      double r1 = std::sqrt(MUIOO::SQUARE(point1.getX()) +
			    MUIOO::SQUARE(point1.getY()) +
			    MUIOO::SQUARE(point1.getZ()));
      
      double r2 = std::sqrt(MUIOO::SQUARE(point2.getX()) +
			    MUIOO::SQUARE(point2.getY()) +
			    MUIOO::SQUARE(point2.getZ()));
      
      return r1 < r2;
    }
  };
  
  static WindowEnum check_phi_window(const PHPoint& point, 
				     const TMuiRoadFinder::road_window& 
				     phi_window);
 
  static WindowEnum check_theta_window(const PHPoint& point, 
				       const TMuiRoadFinder::road_window&
				       theta_window);

  void print() const {
    std::cout << "TMuiRoadFinder settings: " << std::endl;
    if (_reverse_algo) std::cout << " reverse algo " << std::endl;
    else std::cout << " no reverse algo " << std::endl;
    if (_do_evaluation) std::cout << " do_evaluation " << std::endl;
    else std::cout << " no do_evaluation " << std::endl;
    if (_evaluation) std::cout << " evaluation " << std::endl;
    else std::cout << " no evaluation " << std::endl;

    if (_verbosity == MUIOO::NONE) std::cout << " verbosity NONE " << std::endl;
    else if (_verbosity == MUIOO::SOME) std::cout << " verbosity SOME " << std::endl;
    else if (_verbosity == MUIOO::ALOT) std::cout << " verbosity ALOT " << std::endl;
    else std::cout << " verbosity UNKNOWN " << std::endl;

    std::cout << " _max_n_roads " <<  _max_n_roads << std::endl
	      << " _min_cluster " <<  _min_cluster << std::endl
	      << " _min_point  " <<  _min_point << std::endl
	      << " _max_cluster_share " <<  _max_cluster_share
	      << std::endl;

    std::cout << "end of TMuiRoadFinder settings: " << std::endl;
  } 

 private:
  
  friend class Road;
  
  bool append_to_road(TMuiClusterMapO::pointer);
  
  TMuiRoadFinder::Road* start_new_road(TMuiClusterMapO::pointer,
				       bool use_window,
				       const road_window& theta_window,
				       const road_window& phi_window);
  void abort_new_road();
  
  void set_complete_tag();
  
  void remove_includes();
  
  void print_roads() const;
  
  //! Storage for road list
  road_list _road_list;
  
  //! Verbosity
  static MUIOO::Verbosity _verbosity;

  //! Use reverse (outer muid to vertex) road searching algorithm 
  static bool _reverse_algo;

  static UShort_t _max_n_roads;
  static UShort_t _min_cluster;
  static UShort_t _min_point;
  static UShort_t _max_cluster_share;

  //! true if evaluation is to be done
  static bool _do_evaluation;

  //! true if evaluation tree/ntuple was done.
  static bool _evaluation;

  //! evaluation TFile
  static TFile* _file;

  //! evaluation ntuple
  static TNtuple* _ntuple;
};

#endif
