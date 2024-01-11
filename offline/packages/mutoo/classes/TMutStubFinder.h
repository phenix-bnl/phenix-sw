// $Id: TMutStubFinder.h,v 1.39 2011/12/24 04:48:21 slash Exp $

/*!
  \file    TMutStubFinder.h
  \brief   Encapsulates the stub finding algorithm used by the mMutFindStub 
	and mMutFindTrack moduleslog file parser based on xml
  \author  S.Kelly
  \version $Revision: 1.39 $
  \date    $Date: 2011/12/24 04:48:21 $
*/

#ifndef __TMUTSTUBFINDER_H__
#define __TMUTSTUBFINDER_H__

#include<list>
#include<set>
#include<TDataType.h>
#include<TMutCoordMap.h>
#include<TMutFitPar.hh>
#include<TMutTrackUtil.h>
#include<PHPoint.h>
#include<TNtuple.h>
#include<MUTOO.h>
#include<PHTimeServer.h>

/*! \ingroup classes */

//! Encapsulates the stub finding algorithm 
/*! 

This class encapsulates the stub finding algorithm used by the
mMutFindStub and mMutFindTrack modules.  The algorithm uses an
adaptive forward search to associate TMutCoord objects with a class
scope Stub object.  Upon completion of the forward search the list
of TMutCoord is reverse sorted and the algorithm is run again.
Duplicate Stubs (stubs with the same hit set) are remove from the
internal list as are "includes", ie stubs that have hit or point
sets that are subsets of other stubs.  Running the algorithm twice
is done to minimize the sensitivity to noise hits added early in the
search.  The list of class scope stub objects held internally by 
the class is available via an non const member function.  This is
to allow external modules to splice the internal list.  This is
quicker than copying but make the class non-const correct.
*/

class TMutStubFinder
{
 public:  

  class Stub;
  
  //! shortcut to access stub_list  
  typedef std::list<Stub> stub_list;
  
  //! shortcut to access stub window limit
  typedef std::pair<float,float> stub_window;
  
  //! stub windowing mode
  enum WindowEnum 
  { IN_WINDOW, LOW_WINDOW, HIGH_WINDOW };
  
  //! constructor
  TMutStubFinder() 
  {}

  //! Find stubs in all stations with no windows 
  void find(TMutCoordMap* coord_map);

  /*! 
    Find stubs at specified location in specified r, theta window.  If
    no windows are specified the algorithm will find all stubs at given
    location.  The theta radians [0-PI] the phi interval is a sub
    interval of the range [0-2PI] in radians.
  */  
  void find(TMutCoordMap* coord_map,
	    unsigned short arm, 
	    unsigned short station, 
	    unsigned short octant, 
	    unsigned short half_octant,
	    bool use_window = false,
	    const stub_window& theta_window = std::make_pair(0,0),
	    const stub_window& phi_window = std::make_pair(0,0));
  
  /*! 
    Returns a reference to the list of found stubs.  This needs to be
    a non-const reference to allow envokers of the algorithm to splice
    elements from this list.  
  */
  stub_list& get_stub_list() 
  { return _master_stub_list; }

  /*! 
    Clears the stub list.
  */
  void clear_stub_list() 
  { _master_stub_list.clear(); }

  /*! Get a list of coordinates that intersect the given theta/phi window */
  std::list<TMutCoordMap::value_type> 
    get_coords_in_window(TMutCoordMap* coord_map,
					 unsigned short arm,
					 unsigned short station,
					 unsigned short octant,
					 unsigned short half_octant,
					 const std::pair<float,float>& theta_window,
					 const std::pair<float,float>& phi_window);

  //! stub evaluation flag
  static void set_do_evaluation( bool value )
  { _do_evaluation = value; }
  
  //! stub evaluation flag
  static bool get_do_evaluation( void )
  { return _do_evaluation; }
  
  //! stub evaluation filename
  static void set_evaluation_filename( const std::string& filename )
  { _evaluation_filename = filename; }
  
  //! stub evaluation filename
  static std::string get_evaluation_filename( void )
  { return _evaluation_filename; }

  //! Fill evaluation ntuple from stub finders current state
  void evaluate(PHCompositeNode* top_node);
  
  //! Initialize evaluation ntuple, returns true if ntuple/TFile is created.
  static bool initialize_evaluation( void );  
  
  //! Write evaluation ntuple
  static void finish_evaluation();
  
  //! set debuging information verbosity level
  static void set_verbosity( MUTOO::Verbosity value ) 
  { 
    _verbosity = value;
    Stub::set_verbosity(value);
  }

  /*!
    Run the stub finding algorithm forwards and backwards in z,
    sort and remove duplicates. (Slower but less sensitive to
    inefficiencies)
  */
  static void set_reverse_algo(bool value) 
  { _reverse_algo = value;}
  
  //! Point project cut applied with stub has point.
  static void set_dca_cut(double dca_cut) 
  { Stub::set_dca_cut(dca_cut); }
  
  //! W proximity cut applied when stub has no point.
  static void set_w_prox_cut(double value) 
  { Stub::set_w_prox_cut(value); }

  //! Accessor for max number of stubs before bifurcate error.
  static unsigned short get_max_n_stubs() 
  { return _max_n_stubs; }

  //! Modifier for max number of stubs before bifurcate error.
  static void set_max_n_stubs(unsigned short value) 
  { _max_n_stubs = value; }

  //! Minimum hits in stations 1
  static void set_min_coord_1(unsigned short value) 
  { _min_coord_1 = value; }

  //! Get minimum hits in stations 1
  static unsigned short get_min_coord_1() 
  { return _min_coord_1; }

  //! Minimum hits in stations 2
  static void set_min_coord_2(unsigned short value) 
  { _min_coord_2 = value;}

  //! Get minimum hits in stations 2
  static unsigned short get_min_coord_2() 
  { return _min_coord_2; }
  
  //! Minimum hits in stations 3
  static void set_min_coord_3(unsigned short value) 
  { _min_coord_3 = value; }

  //! Get minimum hits in stations 3
  static unsigned short get_min_coord_3() 
  { return _min_coord_3; }

  //! Minimum points in stations 1 and 2
  static void set_min_point_12(unsigned short value) 
  { _min_point_12 = value; }

  //! Minimum points in stations 1 and 2
  static unsigned short get_min_point_12( void ) 
  { return _min_point_12; }
  
  //! Minimum points in stations 3
  static unsigned short get_min_point_3( void ) 
  { return _min_point_3; }

  //! print stub finder parameters
  static void print_parameters( std::ostream &out = std::cout );

  //! summary
  static void print_summary( std::ostream& out = std::cout );

  //! Class scope stub object 
  class Stub {
    
    public:    
    //! structure to sort coordinates according to their key
    struct coord_less_ftor
    {
      bool operator()(TMutCoordMap::value_type coord1, TMutCoordMap::value_type coord2)
      { return (coord1.get()->get_key().get_obj_key() < coord2.get()->get_key().get_obj_key()); }
    };   
    
    //! shortcut for list of coordinates + sorting method
    typedef std::set<TMutCoordMap::value_type, coord_less_ftor> coord_list;
    
    //! shortcut for list of cuts
    typedef std::vector<PHPoint> point_list;
    
    //! constructor
    Stub(TMutStubFinder* finder,
	    const TMutCoordMap::pointer coord_ptr,
	    bool use_window,
	    const stub_window& theta_window,
	    const stub_window& phi_window) :
      _finder(finder),
      _arm(coord_ptr->get()->get_arm()),
      _station(coord_ptr->get()->get_station()),
      _octant(coord_ptr->get()->get_octant()),
      _half_octant(coord_ptr->get()->get_half_octant()),
      _theta_window(theta_window),
      _phi_window(phi_window),
      _has_window(use_window),
      _complete(false),
      _status(true),
      _bifurcate(0)
      { _coords.insert(*coord_ptr); }
    
    //! constructor
    Stub(TMutStubFinder* finder, const TMutCoordMap::pointer coord_ptr):
      _finder(finder),
      _arm(coord_ptr->get()->get_arm()),
      _station(coord_ptr->get()->get_station()),
      _octant(coord_ptr->get()->get_octant()),
      _half_octant(coord_ptr->get()->get_half_octant()),
      _theta_window(std::make_pair(0,0)),
      _phi_window(std::make_pair(0,0)),
      _has_window(false),
      _complete(false),
      _status(true),
      _bifurcate(0)
    { _coords.insert(*coord_ptr); }
    
    //! retrieve associated fit parameters
    const TMutFitPar& get_fit_par() const  
    {return _fit_par;}

    //! retrieve number of associated coordinates
    size_t get_n_coord() const 
    {return _coords.size();}

    //! retrieve number of points    
    size_t get_n_point() const 
    {return _points.size();}    

    //! add a coordinate to the stub
    bool add_coord(TMutCoordMap::pointer coord_ptr);
    
    //! biffurcation  
    bool bifurcate(TMutCoordMap::pointer coord_ptr);      

    //! arm location
    unsigned short get_arm() const 
    {return _arm;}
    
    //! station location
    unsigned short get_station() const
    {return _station;}
    
    //! octant location 
    unsigned short get_octant() const 
    {return  _octant;}
    
    //! half octant location
    unsigned short get_half_octant() const 
    {return  _half_octant;}

    //! stub list of coordinates
    coord_list& get_coord_list()
    { return _coords; }

    //! stub list of coordinates
    const coord_list& get_coord_list() const
    { return _coords; }
    
    //! stub list of 2D points
    point_list& get_point_list() 
    { return _points; }
    
    //! theta window
    stub_window get_theta_window() const 
    { return _theta_window; }
    
    //! phi window
    stub_window get_phi_window() const 
    { return _phi_window; }
    
    //! theta window
    void set_theta_min(double theta_min) 
    { _theta_window.first = theta_min; }
    
    //! theta window
    void set_theta_max(double theta_max) 
    { _theta_window.second = theta_max; }
    
    //! phi window
    void set_phi_min(double phi_min) 
    { _phi_window.first = phi_min; }
    
    //! phi window
    void set_phi_max(double phi_max) 
    { _phi_window.second = phi_max; }

    //! Return the polar radius of the stub extrapolated to requested z
    double get_r_at_z(double z) const 
    {
      PHPoint p = TMutTrackUtil::linear_track_model(&_fit_par,z);
      return std::sqrt(
        MUTOO::SQUARE(p.getX()) + 
        MUTOO::SQUARE(p.getY()));
    }

    //! returns angle wrt beam axis
    double get_theta() const 
    {
      double r = std::sqrt(
        MUTOO::SQUARE(_fit_par.get_x()) +
			  MUTOO::SQUARE(_fit_par.get_y()));
      return atan2(r,std::fabs(_fit_par.get_z()));
    }

    //! returns azymuth angle
    double get_phi() const 
    { return atan2(_fit_par.get_y(), _fit_par.get_x()); }

    //! w slope against z
    double get_dwdz() const;
    
    //! _check_gap_coord flag
    static bool get_check_gap_coord( void )
    { return _check_gap_coord; }
    
    //! _check_gap_coord flag
    static void set_check_gap_coord( bool value )
    { _check_gap_coord = value; }
    
    //! cut on distance of closest approach
    static double get_dca_cut( void )
    { return _dca_cut; }
  
    //! cut on distance of closest approach
    static void set_dca_cut(double value)
    { _dca_cut = value; }

    //! cut on w (perp to the strips)
    static double get_w_prox_cut( void )
    { return _w_prox_cut; }
    
    //! cut on w (perp to the strips)
    static void set_w_prox_cut(double value)
    { _w_prox_cut = value; }
 
    //! set Stub verbosity
    static void set_verbosity( MUTOO::Verbosity value )
    { _verbosity = value; }

    //! returns true if stub is completed
    bool is_complete() const 
    { return _complete; }
    
    //! set stub completeness tag
    void set_complete(bool complete) 
    { _complete = complete; }

    //! returns stub status
    bool get_status() const 
    {return _status; }
    
    //! defines stub status
    void set_status(bool status) 
    {_status = status;}
    
    //! bifurcation flag
    unsigned short get_bifurcate() const 
    {return _bifurcate; }
    
    //! bifurcation flag
    void set_bifurcate(unsigned short bifurcate) 
    {_bifurcate = bifurcate;}

    //! returns true if this stub falls in theta range
    bool check_theta_window(const stub_window& theta_window) const;
    
    //! returns true if this stub falls in phi window
    bool check_phi_window(const stub_window& phi_window) const;
    
    //! streamer
    friend std::ostream& operator << (std::ostream& out, const Stub& stub )
    {
      out << "stub: ";
      for( coord_list::const_iterator iter = stub._coords.begin(); iter != stub._coords.end(); ++iter)
      { out << iter->get()->get_key().get_obj_key() << " "; }
      out << "status = " << stub._status << std::endl;
      return out;
    }

    //! initialize Stub evaluation ntuple
    static void initialize_evaluation( void );
    
    //! save Stub evaluation ntuple
    static void finish_evaluation( void );

  private: 

    //! make stub point using new coordinate and stub previous coords
    PHPoint make_point(const TMutCoordMap::pointer) const;
    
    //! add coordinate to a stub
    void update_stub(TMutCoordMap::pointer);
    
    //! add coordinate and point to a stub
    void update_stub(TMutCoordMap::pointer, const PHPoint& point);
    
    //! returns true if point enters stub window
    bool test_window(const PHPoint&) const;
    
    //! check if coordinate adds a new direction to the stub
    bool check_parallel(const TMutCoordMap::pointer) const ;
    
    //! project stub to a given z
    PHPoint project(double z) const;

    /*! 
      check if coord to be added and coord on second cathode form a 
      gap coordinate. returns true if yes or if there is no coordinate
      on other cathode.
    */
    bool check_gap_coord( TMutCoordMap::pointer coord_ptr) const;

    //! check if coordinate to be added is close enough to stub point
    bool check_w_proximity(TMutCoordMap::pointer coord_ptr) const;
    
    //! check distance of closest approach between coordinate and point
    bool check_dca(const TMutCoordMap::pointer coord_ptr, const PHPoint&) const;
    
    //! check stub slope
    bool check_slope() const;
        
    //! check only one coord/gap is in stub
    bool unique_check(TMutCoordMap::pointer coord_ptr) const;

    //! Pointer to parent stubfinder algorithm
    TMutStubFinder* _finder;

    //! arm location
    unsigned short _arm;
    
    //! station location
    unsigned short _station;
    
    //! octant location
    unsigned short _octant;
    
    //! half_octant location
    unsigned short _half_octant;

    //! stub theta window
    stub_window _theta_window;
    
    //! stub phi window
    stub_window _phi_window;
    
    //! set to true when stub windows are set
    bool _has_window;
    
    bool has_window() const 
    { return _has_window; }

    //! Complete Tag
    bool _complete;
    
    //! current stub status
    bool _status;
    
    //! stub bifurcation
    unsigned short _bifurcate;

    //! true if the gap_coord check is to be done
    static bool _check_gap_coord;

    //! cut on point to stub dca
    static double _dca_cut;
    
    //! cut on coord to stub w distance
    static double _w_prox_cut;

    // Stub coordinates
    coord_list _coords;  
    
    //! Stub points
    point_list _points;  

    //! Stub parameters
    TMutFitPar _fit_par;      
    
    //! Verbosity
    static MUTOO::Verbosity _verbosity;

    //! true if stub evaluation is to be done
    static bool _evaluation;
    
    //! stub evaluation ntuple
    static TNtuple* _ntuple;
    
    //! evaluation data
    static boost::array<float,20> _eval_data;
  
  }; 
  
  //! print a stub
  class stub_print_ftor
  {
    public: 
    
    //! constructor
    stub_print_ftor( std::ostream& out = std::cout ):
      _out( out )
      {}
    
    void operator() (const Stub& stub)
    { _out << stub; }
    
    private:
    
    //! stream
    std::ostream& _out;
    
  };
  
  //! return true if stub status is wrong
  struct stub_bad_status_ftor
  {
    bool operator()(const Stub& stub) 
    { return !stub.get_status(); }
  }; 
    
  //! returns true if associated coordinates have same keys strictly
  class stub_equal_ftor
  {
    public:
    
    //! predicate
    bool operator()(const Stub& stub1, const Stub& stub2) const
    {
      const Stub::coord_list& coords1( stub1.get_coord_list() );
      const Stub::coord_list& coords2( stub2.get_coord_list() );
      if( coords1.size() != coords2.size() ) return false;
      
      Stub::coord_list::const_iterator iter1( coords1.begin() );
      Stub::coord_list::const_iterator iter2( coords2.begin() );
      for( ; iter1 != coords1.end() && iter2 != coords2.end(); iter1++, iter2++ )
      {
        if( iter1->get()->get_key().get_obj_key() != iter2->get()->get_key().get_obj_key() )
        { return false; }
      }

      return true;
    }
    
  };
  
  //! returns true if the stub associate coords is equal to the one given in argument
  /*! it is used to avoid creating twice the same stub */
  struct stub_coord_equal_ftor
  {
    //! constructor
    stub_coord_equal_ftor( const TMutCoordMap::value_type& coord ):
      _key( coord.get()->get_key().get_obj_key() )
    {}
    
    //! predicate
    bool operator() (const Stub& stub ) const
    { 
      const Stub::coord_list& coords( stub.get_coord_list() );
      return ( coords.size() == 1 && coords.begin()->get()->get_key().get_obj_key() == _key );
    }
    
    private:
    
    //! predicted key
    PHKey::object_key_type _key;
    
  };
  
  //! sort stubs according to associated set of coordinates key lexycographic order
  class stub_less_ftor
  {
    public:
    
    //! predicate
    bool operator()( const Stub& stub1, const Stub& stub2) const
    {
      const Stub::coord_list& coords1( stub1.get_coord_list() );
      const Stub::coord_list& coords2( stub2.get_coord_list() );
      Stub::coord_list::const_iterator iter1( coords1.begin() );
      Stub::coord_list::const_iterator iter2( coords2.begin() );
      for( ; iter1 != coords1.end() && iter2 != coords2.end(); iter1++, iter2++ )
      {
        if( iter1->get()->get_key().get_obj_key() != iter2->get()->get_key().get_obj_key() )
        { return( iter1->get()->get_key().get_obj_key() < iter2->get()->get_key().get_obj_key() ); }
      }
    
      return ( coords1.size() < coords2.size() );

    }
  };

  //! sort stubs according to size of associated set of coordinates key lexycographic order
  /*! stubs with bigger size come first */
  class stub_size_more_ftor
  {
    public:
    
    //! predicate
    bool operator()( const Stub& stub1, const Stub& stub2) const
    { return ( stub1.get_coord_list().size() > stub2.get_coord_list().size() ); }
    
  };
  
  //! used to reject stubs based on coordinate list
  class stub_include_ftor
  {
    public:
    
    //! constructor
    stub_include_ftor( const Stub& stub ): 
      _coords( stub.get_coord_list() )
    {}
    
    //! predicate
    bool operator() ( const Stub& stub ) const
    {
      const Stub::coord_list& coords( stub.get_coord_list() );
      return ( 
        _coords.size() > coords.size() && 
        std::includes(
          _coords.begin(), _coords.end(),
          coords.begin(), coords.end(),
          Stub::coord_less_ftor() ) );
    }
    
    private:
    
    //! predicted list of coordinates
    const Stub::coord_list& _coords;
    
  };
  
  //! sort point by increasing distance to the beam
  class point_less_ftor
  {
    public:
    
    //! predicate
    bool operator()(const PHPoint& point1, const PHPoint& point2) const
    {
      double r1 = std::sqrt(
          MUTOO::SQUARE(point1.getX()) +
			    MUTOO::SQUARE(point1.getY()) +
			    MUTOO::SQUARE(point1.getZ()));
      
      double r2 = std::sqrt(
          MUTOO::SQUARE(point2.getX()) +
			    MUTOO::SQUARE(point2.getY()) +
			    MUTOO::SQUARE(point2.getZ()));
      
      return r1 < r2;
    }
  };

  //! check that point is in phi window
  static WindowEnum check_phi_window(const PHPoint& point, const TMutStubFinder::stub_window& phi_window);
  
  //! check that point is in theta window
  static WindowEnum check_theta_window(const PHPoint& point, const TMutStubFinder::stub_window& phi_window);
  
 private:
  
  friend class Stub;
  
  // Local storage for coordinate list 
  typedef std::list<TMutCoordMap::value_type> local_coord_list;

  //! try append a coordinate to an existing stub
  bool append_to_stub(TMutCoordMap::pointer);
  
  //! start a new stub
  TMutStubFinder::Stub* start_new_stub(TMutCoordMap::pointer,
				       bool use_window,
				       const stub_window& theta_window,
				       const stub_window& phi_window);
  
  //! abort current stub
  void abort_new_stub();

  //! set that current stub is complete
  void set_complete_tag();

  //! remove stubs which are subset of others
  void remove_includes();

  //! update master stub list
  void update_master_list( void );
  
  //! print stubs
  void print_stubs( std::ostream& out = std::cout ) const
  {

    MUTOO::PRINT( out, "**" );
    std::for_each( _stub_list.begin(), _stub_list.end(), stub_print_ftor( out ));
    MUTOO::PRINT( out,"**");

  }
  
  //! temporary storage for stub list (used in find)
  stub_list _stub_list;  
  
  //! master stub_list
  stub_list _master_stub_list;
  //!@name configuration
  //@{
  
  //! Verbosity
  static MUTOO::Verbosity _verbosity;

  //! use reverse (vertex to muid) stub searching algorithm
  static bool _reverse_algo;
  static unsigned short _max_n_stubs;
  static unsigned short _min_coord_1;
  static unsigned short _min_coord_2;
  static unsigned short _min_coord_3;
  static unsigned short _min_point_12;
  static unsigned short _min_point_3;
  
  //@}
  
  //!@name evaluation
  //@{
  //! true if evaluation is to be done
  static bool _do_evaluation;
  
  //! true if evaluation tree/ntuple was created.
  static bool _evaluation;

  //! evaluation filename
  static std::string _evaluation_filename;
  
  //! evaluation ntuple
  static TNtuple* _ntuple;
  
  //@}
  
  //!@name statistics
  //@{
  
  //! Timer
  static PHTimeServer::timer _timer;

  //! number of stubs after direct algorithm
  static double _n_stubs_direct;
  
  //! number of stubs after reverse algorithm
  static double _n_stubs_reverse;
  
  //! number of stubs after cleaning
  static double _n_stubs_completed;
  
  //@}
  
};

#endif

