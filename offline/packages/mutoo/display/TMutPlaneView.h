#ifndef __TMUTPLANEVIEW_HH__
#define __TMUTPLANEVIEW_HH__

#include<TMarker.h>
#include<TCanvas.h>
#include<TPaveLabel.h>
#include<PHPoint.h>
#include<TMutHitMap.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<utility>
#include<vector>
#include<boost/array.hpp>
#include<TLine.h>
#include<TMarker.h>

class MutArm;

/*! \ingroup display */
//!Draws the MUTR strips in a half_octant from "end on" perspective.
/*!
  This perspective maps the locus of point defined by the endpoints
  of the strips in a given cathode plane onto a linear range.
*/
class TMutPlaneView
{
 public:
  
  TMutPlaneView(unsigned short arm, unsigned short octant, int event=-1);
  
  virtual ~TMutPlaneView();
  
  Bool_t event(PHCompositeNode* top_node);
  
  /*! Paint the plane view for the specified half-octant */
  void paint();
  
  /*! Update the canvas */
  void update(){_canvas->Update();}
  
  /*! Get arm */  
  unsigned short get_arm() const { return _arm;}
  
  /*! Get octant */  
  unsigned short get_octant() const { return _octant;}

  /*! Flag that controls stub drawing */
  void set_draw_stubs(bool val) {_draw_stubs = val;}  

  /*! Flag that controls drawing of stub extrapolations */
  void set_draw_stub_ext(bool val) {_draw_stub_ext = val;}  

  /*! Flag that conrols track drawing */
  void set_draw_tracks(bool val) {_draw_tracks = val;}

  /*! Flag that conrols monte-carlo track drawing */
  void set_draw_mc_tracks(bool val) {_draw_mc_tracks = val;}
  
 private:  
  
  typedef std::pair<double,double> coord_pair;
  typedef std::vector< coord_pair > coord_vector;
  typedef std::vector< std::pair<coord_pair,coord_pair> > line_vector;

  // If there ever was a better argument for a tuple I haven't seen it
  //
  typedef std::vector< std::pair< std::pair<coord_pair,coord_pair>, int > > line_flag_vector;

  /*! Clear the graphic primitives in temp storage */
  void clear_temp_storage();
  
  /*! Setup canvas and pad for this view */
  void setup_canvas();
  
  /*! Get IOC pointers */
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  /*! Append the coordinates of specified half octants to coordinate list */
  void get_cathode_coord(); 
  
  /*! Utility, used by get_cathode_coord */
  void get_cathode(unsigned short station, 
		   unsigned short half_octant,
		   unsigned short gap,
		   unsigned short cathode);
  
  /* Clear the current coordinate list */
  void clear_cathode_coord(){_cathode_coord.clear();}
  
  /* Get coordinates of hit strips */
  void get_hits();

  /* Get coordinates stubs */
  void get_stubs();
  
  /* Get coordinates tracks */
  void get_field_on_trk();

  /* Get coordinates tracks */
  void get_trk_windows();
      
  /* Get coordinates monte-carlo tracks */
  void get_mc_trk();
  
  /* Get coordinates of clusters */
  void get_clusters();
  
  /* Get coordinates of gap_coords */
  void get_coord();

  /* Get coordinates of monte carlo hit */
  void get_mc_hit();
  
  /* Get coordinates of gap_coords */
  void get_gap_coord();
  
  /* Get coordinates of hit strips */
  void get_strip_coord(TMutHitMap::const_pointer);
  
  /* Get coordinates of cluster offsets */
  void get_cluster_coord(TMutClusMap::const_pointer);
  
  /*! Append the coordinates of specified frame to coordinate list */
  void get_frame_coord(); 
  
  /*! Utility, used by get_frame_coord */
  void get_frame(unsigned short station);
  
  enum View {VIEW_ZW,VIEW_WZ};
  
  /*! 
    Paint frame associated with specified arm and station.  W axis
    is vertical, z axis is horizontal.
  */
  void paint_frames_wz();
  
  /*! 
    Paint frame associated with specified arm and station.  W axis
    is horizontal, z axis is vertical.
  */
  void paint_frames_zw();
  
  /*! 
    Paint planes associated with specified arm and station.  W axis
    is vertical, z axis is horizontal.
  */
  void paint_planes(View); 
  
  /*! 
    Paint hit strips associated with specified arm and station.  W axis
    is vertical, z axis is horizontal.
  */
  void paint_hits(View);
  
  /*! 
    Paint tracks
  */
  void paint_trks(unsigned short station);

  /*! 
    Paint monte-carlo tracks
  */
  void paint_mc_trk(View);

  /*! 
    Paint stubs
  */
  void paint_stubs(unsigned short station);
  
  /*! 
    Paint peak strip and offset associated with specified arm and station.  
  */
  void paint_clusters(View);
  
  /*! 
    Paint gap coordinates
  */
  void paint_coord(View); 
  
  /*! 
    Paint gap coordinates
  */
  void paint_gap_coord(View); 

  /*! 
    Paint monte carlo hits
  */
  void paint_mc_hit(View); 

  /*! Transform a PHPoint to wz coordinate system */
  
  coord_pair transform_zw(unsigned short station, const PHPoint& point);
  
  /*! Map station onto some reasonable colors */
  unsigned short station_color(unsigned short i_station) const;

  /*! Determine the station closest to given z */
  unsigned short nearest_station(double z) const;

  // IOC
  //
  MutArm* _geometry;
  const TMutHitMap* _hit_map;
  const TMutClusMap* _clus_map;
  const TMutCoordMap* _coord_map;
  const TMutGapCoordMap* _gap_coord_map;
  const TMutStubMap* _stub_map;
  const TMutTrkMap* _trk_map;
  const TMutMCHitMap* _mc_hit_map;
  const TMutMCTrkMap* _mc_trk_map;
  
  unsigned short _arm;
  unsigned short _octant;
  int _event;
  float _sin_x_xp;
  float _cos_x_xp;

  // Canvas/Pad members for this view
  //
  TCanvas* _canvas;
  TPaveLabel* _label;
  TPad* _pad0;
  TPad* _pad1;
  TPad* _pad2;
  TPad* _pad3;
  TPad* _pad4;

  // Root graphics requires we keep the primitives alive 
  // These structs are temporary storage for graphics
  // primitives they are cleared each time paint is called
  // 
  std::vector<TMarker*> _marker_vector;
  std::vector<TLine*> _line_vector;
  boost::array<TPave*,3> _box;

  coord_vector _cathode_coord;           // for drawing cathode strips
  coord_vector _hit_coord;               // for drawing hit strips
  coord_vector _peak_coord;		 // for drawing cluster peaks
  coord_vector _offset_coord;		 // for drawing cluster offsets
  coord_vector _coord;                   // for drawing coordinates
  coord_vector _gap_coord;               // for drawing gap coordinates
  coord_vector _mc_hit;                  // for drawing monte-carlo hits
  coord_vector _mc_trk;                  // for drawing monte-carlo tracks
  coord_vector _trk;                     // for drawing reconstructed tracks

  std::vector<coord_vector> _tracks;
  
  // an array of vectors of frame coordinates (one for each station)
  //
  boost::array<coord_vector,3> _frames;
  boost::array<coord_vector,3> _range;

  boost::array<line_vector,3> _track_windows;
  boost::array<line_flag_vector,3> _stubs;
  boost::array<PHPoint,3> _origin;
  boost::array<PHPoint,3> _last_strip;

  bool _draw_stubs;
  bool _draw_stub_ext;
  bool _draw_tracks;
  bool _draw_mc_tracks;
  bool _is_monte_carlo;
};

struct PVDeleteMarker
{
  void operator() (TMarker* marker){
    delete marker;
  }
};

struct PVDeleteLine
{
  void operator() (TLine* line){
    delete line;
  }
};

#endif

