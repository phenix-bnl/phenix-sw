#ifndef __TMUTOCTANTVIEW_HH__
#define __TMUTOCTANTVIEW_HH__

#include<TLine.h>
#include<TMarker.h>
#include<TCanvas.h>
#include<TPaveLabel.h>
#include<vector>
#include<MUTOO.h>

class TMutCoordMap;
class TMutGapCoordMap;
class TMutStubMap;
class TMutTrkMap;
class PHCompositeNode;

/*! \ingroup display */
/*! Draws the MUTR octants from the looking down the beam pipe perspective. */
class TMutOctantView
{

public:

  TMutOctantView(unsigned short arm = MUTOO::South, int event=-1, int octant=-1, int station=-1);
  virtual ~TMutOctantView(){;}
  void paint();    
  Bool_t event(PHCompositeNode* top_node);
  void set_arm(unsigned short arm ) { _arm = arm; }
  unsigned short get_arm() const { return _arm;}
  int get_octant() const { return _octant; }
  int get_station() const { return _station; }
  void set_draw_stubs(bool draw_stubs) { _draw_stubs = draw_stubs;}

 private:

  void setup_canvas();
  void paint_octants(unsigned short i_station); 
  void get_coord();
  void paint_coord();
  void get_gap_coord();
  void paint_gap_coord();
  void paint_trks();
  void paint_stubs();
  unsigned short station_color(unsigned short i_station) const;
  void clear_temp_storage();

  unsigned short _arm;
  int _event;
  int _octant;
  int _station;

  TMutCoordMap* _coord_map;
  TMutGapCoordMap* _gap_coord_map;
  TMutStubMap* _stub_map;
  TMutTrkMap* _trk_map;

  TCanvas* _canvas;
  TPad* _pad0;
  TPad* _pad1;
  TPaveLabel* _label;  

  typedef std::vector<TLine*> line_vector;			 // for drawing frames
  line_vector _coord;
  line_vector _trks;
  typedef std::vector<TMarker*> marker_vector;
  marker_vector _gap_coord;

  bool _draw_stubs;
  
};

struct OVDeleteMarker
{
  void operator() (TMarker* marker){
    delete marker;
  }
};

struct OVDeleteLine
{
  void operator() (TLine* line){
    delete line;
  }
};

#endif
