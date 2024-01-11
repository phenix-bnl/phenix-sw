#ifndef __FvtxPrimVertex_h__
#define __FvtxPrimVertex_h__

/*!
  \file		FvtxPrimVertex.cxx	
  \ingroup supermodules
  \brief	 Fvtx primary vertex module. 
  Reads TFvtxCompactTrk from DST, fill VtxOut with primary vertex from FVTX
  \author	Cesar Luiz da Silva
  \version $Revision: 1.31 $
  \date		$Date: 2017/07/13 19:24:26 $
*/

// standard
#include <iostream>
#include <string>
#include <vector>
#include <map>

// boost
#define BOOST_NO_HASH
#ifndef __CINT__
#include <boost/shared_ptr.hpp>
#include <boost/bind.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#endif //__CINT__

// root
#include <Minuit2/Minuit2Minimizer.h>

// phenix
#include <PHGeometry.h>
#include <MuonSubsysReco.h>
#include <MUTOO.h>
#include <PHPoint.h>
#include <PHLine.h>
#include <PHTimer.h>
#include <SvxSegment.h>

// fvtx
#include <TFvtxCompactTrk.h>

// Forward declerations
class PHCompositeNode;
//class TFvtxCoordMap;
//class TFvtxCoord;
class TFvtxCompactTrkMap;
class BbcMultipleVtx;

class Tracklet {
 public:

  enum TRACKLET_KIND {FVTX_TRACK,VTX_TRACK,VTXP_STUB};
  enum ARM {EAST=0,WEST=1,NORTH=2,SOUTH=3};
  
  Tracklet(TFvtxCompactTrk* fvtx_track,float max_R,float max_Z,float max_DCA, bool stub);
  Tracklet(SvxSegment* segment,float max_R,float max_Z,float max_DCA);
  virtual ~Tracklet() {}

  void     set_correction_vector(ARM arm, float dx, float dy, float dz) {
    _correction_vector[arm][0] = dx;
    _correction_vector[arm][1] = dy;
    _correction_vector[arm][2] = dz;
  }
  void     print() const;
  int      get_kind() const {return _kind;}
  float    get_phi() const;
  float    get_theta() const;
  PHPoint  get_point() const;
  bool     intersects(Tracklet tracklet) const;
  float    get_dca(PHPoint point) const;
  float    get_dca(Tracklet tracklet) const;
  PHPoint  get_intersection(PHPlane plane) const;
  PHVector get_momentum(PHPoint point) const;
  PHPoint  get_pca(PHPoint point) const; 
  PHPoint  get_pca(Tracklet tracklet) const;
  float    get_fit_weight() const;

  //PHObject* get_parent() const;

 private:
  static float _max_R;
  static float _max_Z;
  static float _max_DCA;
  static float _correction_vector[4][3];
  
  int _kind;
  TFvtxCompactTrk* _fvtx_track;
  SvxSegment* _segment;
}; // class Tracklet

class Intersection {
 public:

  Intersection();
  Intersection(unsigned int index, Tracklet* first, Tracklet* second, float max_sep);
  virtual ~Intersection() {}

  void    print() const;
  unsigned int get_index() const {return _index;}
  PHPoint get_point() const {return _point;}
  void    set_point(PHPoint point) {_point = point;}
  float   get_x() const {return _point.getX();}
  void    set_x(float x) {_point.setX(x);}
  float   get_y() const {return _point.getY();}
  void    set_y(float y) {_point.setY(y);}
  float   get_z() const {return _point.getZ();}
  void    set_z(float z) {_point.setZ(z);}

  float get_reso_x() const {return _max_sep;}
  float get_reso_y() const {return _max_sep;}
  float get_reso_z() const;

  bool is_adjacent(Intersection& intersection) const;

  Tracklet* get_first_tracklet() {return _tracklets.first;}
  const Tracklet* get_first_tracklet() const {return _tracklets.first;}
  Tracklet* get_second_tracklet() {return _tracklets.second;}
  const Tracklet* get_second_tracklet() const {return _tracklets.second;}

  double distance(Intersection& intersection) const;
  double size() {return _size;}
  void   set_size(double size) {_size = size;}
  Intersection merge(Intersection& intersection) const;

 private: 
  static float _max_sep;

  unsigned int _index;
  double _size;
  PHPoint _point;
  std::pair<Tracklet*,Tracklet*> _tracklets;
}; // class Intersection

class Vertex {
 public:

  typedef std::set<Intersection*>::iterator IntersectionIter;
  typedef std::set<Intersection*>::const_iterator IntersectionConstIter;
  typedef std::set<Tracklet*>::iterator TrackletIter;
  typedef std::set<Tracklet*>::const_iterator TrackletConstIter;

  Vertex();
  virtual ~Vertex() {}

  void print() const;
  bool operator< (const Vertex& rhs) const {
    return (get_num_intersections() > rhs.get_num_intersections());
  }

  size_t                 get_num_intersections() const {return _intersections.size();}
  IntersectionConstIter  get_intersections_begin() const {return _intersections.begin();}
  IntersectionConstIter  get_intersections_end() const {return _intersections.end();}
  void                   add_intersection(Intersection* intersection);
  IntersectionIter       get_intersections_begin() {return _intersections.begin();}
  IntersectionIter       get_intersections_end() {return _intersections.end();}

  size_t             get_num_tracklets() const {return _tracklets.size();}
  TrackletConstIter  get_tracklets_begin() const {return _tracklets.begin();}
  TrackletConstIter  get_tracklets_end() const {return _tracklets.end();}
  TrackletIter       get_tracklets_begin() {return _tracklets.begin();}
  TrackletIter       get_tracklets_end() {return _tracklets.end();}

  PHPoint get_ave_point() const;
  PHPoint get_ave_error() const;

  PHPoint get_point() const {return _point;}
  float   get_x() const {return _point.getX();}
  float   get_y() const {return _point.getY();}
  float   get_z() const {return _point.getZ();}
  void    set_point(PHPoint point) {_point = point;}
  void    set_x(double x) {_point.setX(x);}
  void    set_y(double y) {_point.setY(y);}
  void    set_z(double z) {_point.setZ(z);}

  PHPoint get_error() const {return _error;}
  float   get_ex() const {return _error.getX();}
  float   get_ey() const {return _error.getY();}
  float   get_ez() const {return _error.getZ();}
  void    set_error(PHPoint error) {_error = error;}
  void    set_ex(double ex) {_error.setX(ex);}
  void    set_ey(double ey) {_error.setY(ey);}
  void    set_ez(double ez) {_error.setZ(ez);}

 private:
  PHPoint _point;
  PHPoint _error;
  std::set<Intersection*> _intersections;
  std::set<Tracklet*> _tracklets;
}; // class Vertex

/// \ingroup supermodules
/// Fvtx primary vertex module. 
/// Reads TFvtxCompactTrk from DST, fill VtxOut with primary vertex from FVTX
///
class FvtxPrimVertex: public SubsysReco {

 public:
  
  enum SOURCE {Null,Tracks,Segments,PixelStubs};
  enum CLUSTERING {AllInOne,Cesars,Graph,AntiKt};
  enum ARM {EAST=0,WEST=1,NORTH=2,SOUTH=3};
  
  /// constructor
  FvtxPrimVertex();
  virtual ~FvtxPrimVertex();

  /// module initialization
  int Init(PHCompositeNode *topNode);

  /// run initialization
  int InitRun(PHCompositeNode *topNode);
  
  /// event processing
  int process_event(PHCompositeNode *topNode);
  
  /// event reset
  int ResetEvent(PHCompositeNode *topNode);

  /// end of process
  int End(PHCompositeNode *topNode);

  /// switch for pp or hi settings
  void set_pp_flag(bool is_pp, bool user_input = true) {
    // module defaults to pp-settings
    if (!is_pp) {
      set_max_vertexes( 1 ); // write out only one vertex
      set_clustering( FvtxPrimVertex::AllInOne ); // single vertexing
    } else {
      set_max_vertexes( 5 );
      set_clustering( FvtxPrimVertex::Graph );
    }
  }

  /// use vtx data or not
  void set_vtx_usage(bool use_vtx, bool user_input = true) {
    // module defaults to fvtx-only use
    if (use_vtx) {
      set_source( FvtxPrimVertex::Segments, FvtxPrimVertex::Tracks );
    } else {
      set_source( FvtxPrimVertex::Tracks );
    }
  }

  /// set an arm-by-arm correction vector
  void set_correction_vector(ARM arm, float dx, float dy, float dz) {
    _correction_vector[arm][0] = dx;
    _correction_vector[arm][1] = dy;
    _correction_vector[arm][2] = dz;
  }
  
  /// set the range to consider two tracks are crossings
  float get_fvtx_Rres() {return _fvtx_Rres;}
  void set_fvtx_Rres(float a) {_fvtx_Rres = a;}

  void Verbosity(int n) {verbosity = n;}
  void set_verbose(int n) {verbosity = n;}

  void set_max_vertexes(unsigned int max_vertexes) {
    _max_vertexes = max_vertexes;
    _max_vertexes_user_set = true;
    reset_vertex_defaults();
  }

  /// sets the output vertex names and priorities
  void set_vertex_output(unsigned int index = 0,
			 std::string vertex_name = "FVTX",
			 short int vertex_priority = 8) {
    if (index < _max_vertexes) {
      _vertex_names[index] = vertex_name;
      _vertex_priorities[index] = vertex_priority;

			for (unsigned int i = 1; i < _max_vertexes; ++i) {
				if (i == 1) {
					_vertex_names[i] = Form("%s_SECOND",_vertex_names[0].c_str());
				} else {
					_vertex_names[i] = Form("%s_%i",_vertex_names[0].c_str(),i+1);
				}   
			}  

    } else {
      std::cerr << PHWHERE << ":: index out of range." << std::endl;
    }
  }
  
  /// determines if tracks or cluster-pairs are used
  void set_source(SOURCE src1, SOURCE src2 = Null, SOURCE src3 = Null) {
    _sources_user_set = true;
    _sources.clear();
    _sources.insert(src1);
    if (src2 != Null) _sources.insert(src2);
    if (src3 != Null) _sources.insert(src3);
  }

  /// fix the vertex position in x,y to beam spot values
  void set_fixed_xy(float x, float y, float ex = 0.0, float ey = 0.0) {
    _fix_xy = true; 
    _vtx_x = x; 
    _vtx_y = y;
    _vtx_ex = ex;
    _vtx_ey = ey;
  }

  /// use only a subset of tracklets for testing purposes
  void set_arms_in_use(bool north, bool south, bool east, bool west) {
    _use_north_arm = north;
    _use_south_arm = south;
    _use_east_arm = east;
    _use_west_arm = west;
  }

  /// determines if the primary vertex is used by the MuTr tracking
  void set_mutr_use_fvtx(bool b) {_mutr_use_fvtx = b;}

  /// determines how to cluster the intersections into vertex objects
  void
  set_clustering(CLUSTERING clustering)
  {
    _clustering = clustering;
    _clustering_user_set = true;
  }

  void set_bbcz_window(float value) {_bbcz_window = value;}

  /// turns on vertex fitter
  void set_fitter_active(bool make_fit) {_make_fit = make_fit;}

  /// turns on vertex fitter recentering fit to update vertex guess
  void set_fitter_recentering(bool make_recentering_fit) {
    _make_recentering_fit = make_recentering_fit;
  } 

  /// set the regulation sigma values
  void set_fitter_sigma(float sigma_para,
			float sigma_perp,
			float sigma_xy = 0.024,
			float sigma_z  = 0.05625) {
    _fit_sigma_para = sigma_para;
    _fit_sigma_perp = sigma_perp;
    _fit_sigma_xy   = sigma_xy;
    _fit_sigma_z    = sigma_z;
  } 

  /// set the relative weight of FVTX info to VTX info in the fitting
  void set_fitter_fvtx2vtx_weight(float fit_fvtx2vtx_weight, 
				  float fit_fvtx2stub_weight = 0.60) {
    _fit_fvtx2vtx_weight = fit_fvtx2vtx_weight;
    _fit_fvtx2stub_weight = fit_fvtx2stub_weight;
  }

  void set_max_intersections(unsigned int max) {
    _max_intersections = max;
  }
  
  /// run the top-level process event call but don't export the vertexes
  /// to VtxOut (for use in analysis code), code will ignore any pointers
  /// in the exclude list. idea: to provide a way for users to refit vertexes
  /// after identifying heavy flavor constributions in p+p in analysis code
  std::vector<const Vertex*> generate_vertexes(PHCompositeNode* top_node,
					       std::vector<const TFvtxCompactTrk*> exclude_fvtx,
					       std::vector<const SvxSegment*> exclude_vtx);

 private:

  // top-level process event calls
  int make_tracklets(PHCompositeNode* top_node);
  int make_intersections();
  int make_vertexes();
  int fit_vertexes();
  int beam_averaging();
  int export_vertexes(PHCompositeNode* top_node);

  // second-level process event 
  void cesars_vertex_selection(std::set<Tracklet*>& used_tracklets);
  void vertex_selection(std::multimap<int,Intersection>& clustered_intersections,
			std::set<Tracklet*>& used_tracklets);

  bool fit_vertex(Vertex& vertex);
  int find_bbcvtx(float _z, float z_error);
  template <class T>
  int make_graph_clusters(std::vector<T>& objects,
                          std::multimap<int, T>& clusters);
  template <class T>
  int make_antikt_clusters(std::vector<T>& objects,
                           std::multimap<int, T>& clusters, double R = 1.0);

  void reset_vertex_defaults();

  // quality checks
  bool is_good(SvxSegment* segment);
  bool is_good(TFvtxCompactTrk* track);
  bool is_good_stub(TFvtxCompactTrk* track);

  // beam averages
  float get_beam_x() {
    if (_fix_xy) return _vtx_x;
    else if (_beam_sum_counts < 10) return NAN;
    else return _beam_sum_x / _beam_sum_counts;
  }
  float get_beam_y() {
    if (_fix_xy) return _vtx_y;
    else if (_beam_sum_counts < 10) return NAN;
    else return _beam_sum_y / _beam_sum_counts;
  }

  // counters
  int _event_count;
  std::vector<int> _vertex_counts;
  std::vector<int> _fit_exits;

#ifdef __CINT__
  PHTimer* _timer;
#else
  boost::shared_ptr<PHTimer> _timer;
#endif

  // options
  std::set<SOURCE> _sources;
  bool _sources_user_set;
  CLUSTERING _clustering;
  bool _clustering_user_set;
  bool _fix_xy;
  bool _make_fit;
  bool _make_recentering_fit;
  bool _mutr_use_fvtx;
  bool _use_north_arm;
  bool _use_south_arm;
  bool _use_east_arm;
  bool _use_west_arm;

  // settings
  unsigned int _max_vertexes;
  unsigned int _max_intersections;
  bool _max_vertexes_user_set;
  float _vtx_x;
  float _vtx_y;
  float _vtx_ex;
  float _vtx_ey;
  unsigned long _beam_sum_counts;
  double _beam_sum_x;
  double _beam_sum_y;
  float _fvtx_Rres;
  float _chi2_cut;
  float _max_Z;
  float _max_R;
  float _max_R2;
  float _bbcz_window;
  float _fit_sigma_para;
  float _fit_sigma_perp;
  float _fit_sigma_xy;
  float _fit_sigma_z;
  float _fit_fvtx2vtx_weight;
  float _fit_fvtx2stub_weight;
  std::vector<std::string> _vertex_names;
  std::vector<short int> _vertex_priorities;
  float _correction_vector[4][3];
  
  // object storage
  BbcMultipleVtx* _bbc;
  ROOT::Minuit2::Minuit2Minimizer* _min;
  std::vector<const TFvtxCompactTrk*> _exclude_fvtx;
  std::vector<const SvxSegment*> _exclude_vtx;
  std::vector<Tracklet> _tracklets;
  std::vector<Intersection> _intersections;
  std::vector<Vertex> _vertexes;  
}; // end FvtxPrimVertex class definition

/// templated graph-based clustering method implementation
template<class T> 
int FvtxPrimVertex::make_graph_clusters(std::vector<T>& objects,
					std::multimap<int,T>& clusters) {

  // requires bool object1.is_ajacent(object2) definition for creating a link
  typedef boost::adjacency_list <boost::vecS, boost::vecS, boost::undirectedS> Graph;
  
  Graph G;
  
  // loop over all objects and create edges
  for (unsigned int i=0; i<objects.size(); ++i) {
    for (unsigned int j=i+1; j<objects.size(); ++j) {
      if (objects[i].is_adjacent(objects[j])) add_edge(i,j,G);
    }
    add_edge(i,i,G);
  }

  // Find the connections between the vertices of the graph (vertices are 
  // the objects, connections are made when they are adjacent to one another)
  std::vector<int> component(num_vertices(G));
  int nclusters = connected_components(G, &component[0]);
      
  // Loop over the components(vertices) compiling a list of the unique
  // connections (ie clusters).
  std::set<int> comps; // Number of unique components
  for(unsigned int i=0; i<component.size(); ++i) {
    comps.insert(component[i]);
    clusters.insert(std::make_pair(component[i],objects[i]));
  } 

  // return number of clusters
  return nclusters; 
}

/// templated antikt-based clustering method implementation
/// clusters are defined by distance and size via an antikt
/// inspired algorithm
template<class T> 
int FvtxPrimVertex::make_antikt_clusters(std::vector<T>& objects,
					 std::multimap<int,T>& clusters,
					 double R) {

  // requires double object1.distance(object2) definition (think angular distance for jets)
  // requires double object.size() definition (think pt for jets)
  // requires T object1.merge(object2) definition (think 4-vector addition for jets)

  clusters.clear();
  if (objects.size() == 0) return 0;
  if (objects.size() == 1) {
    // return trivial result...
    clusters.insert(std::make_pair(0,objects[0]));
    return 1;
  }

  // storage for merged objects
  std::vector<bool> state;
  state.assign(objects.size(), true);
 
  // construct a container for the ancestry structure
  std::vector<std::vector<int> > ancestry; 
  for (unsigned int i = 0; i < objects.size(); ++i) {
    std::vector<int> parts;
    parts.push_back(i);
    ancestry.push_back(parts);
  }

  int noriginals = objects.size();
  int nclusters = 0;
  int nremaining = state.size();
  while (nremaining > 0) {

    int dij_min_i = -1;
    int dij_min_j = -1;
    double dij_min = DBL_MAX;

    int dib_min_i = -1;
    double dib_min = DBL_MAX;

    for (unsigned int i = 0; i < objects.size(); ++i) {

      if (!state[i]) continue;

      double isize = objects[i].size();
      double dib = isize*isize;
      if (dib <= dib_min) {
	dib_min_i = i;
	dib_min = dib;
      }

      for (unsigned int j = i + 1; j < objects.size(); ++j) {

	if (!state[j]) continue;

	double r = objects[i].distance(objects[j]);
	double jsize = objects[j].size();
	double dij = std::min(isize*isize,jsize*jsize)*r*r/(R*R);

	if (dij <= dij_min) {
	  dij_min_i = i;
	  dij_min_j = j;
	  dij_min = dij;
	}
      }
    }
   
    if (dib_min < dij_min) {

      // final-state has been reached
      for (unsigned int i = 0; i < ancestry[dib_min_i].size(); ++i) {
	clusters.insert(std::make_pair(nclusters, objects[ancestry[dib_min_i][i]]));
      }
      ++nclusters;
      state[dib_min_i] = false;

    } else {

      // merge two least distance objects and remove
      std::vector<int> parts;
      for (unsigned int i = 0; i < ancestry[dij_min_i].size(); ++i) {
	parts.push_back(ancestry[dij_min_i][i]);
      }
      for (unsigned int i = 0; i < ancestry[dij_min_j].size(); ++i) {
	parts.push_back(ancestry[dij_min_j][i]);
      }

      T merged_object = objects[dij_min_i].merge(objects[dij_min_j]);
      state.push_back(true);
      objects.push_back(merged_object);
      ancestry.push_back(parts);

      state[dij_min_i] = false;
      state[dij_min_j] = false;
    }

    nremaining = 0;
    for (unsigned int i = 0; i < state.size(); ++i) {
      if (state[i]) ++nremaining;
    }

  }

  // remove all the added material at the end of objects vector
  objects.resize(noriginals);

  // return number of clusters
  return nclusters; 
}


#endif 
