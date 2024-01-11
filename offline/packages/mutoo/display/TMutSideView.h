#ifndef __TMUTSIDEVIEW_HH__
#define __TMUTSIDEVIEW_HH__
// $Id: TMutSideView.h,v 1.7 2014/01/26 18:05:32 bbannier Exp $

/*!
   \file TMutSideView.h
   \brief mutoo side view display
   \author Sean Kelly
   \version $Revision: 1.7 $
   \date $Date: 2014/01/26 18:05:32 $
*/

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
class TMutMCHitMap;
class PHCompositeNode;

/*! \ingroup display */
/*! Draws the MUTR from side perspective */
class TMutSideView
{

  public:
  
  TMutSideView(unsigned short arm = MUTOO::South, unsigned short octant=0, int station=-1, int event=-1);

  virtual ~TMutSideView(){;}

  void paint();    

  Bool_t event(PHCompositeNode* top_node);

  unsigned short get_arm() const { return _arm;}
  unsigned short get_octant() const { return _octant;}
  int get_station() const { return _station;}
  
 private:
  
  void setup_canvas();  
  void paint_stations(); 
  void paint_trks(); 
  void paint_stubs(); 
  void paint_gap_coord();
  void paint_mc_hit();
  void get_mc_hit();
  void get_gap_coord();
  void get_trks();
  void clear_temp_storage();
  unsigned short station_color(unsigned short i_station) const;
  
  TCanvas* _canvas;
  TPad* _pad0;
  TPad* _pad1;
  TPaveLabel* _label;

  typedef std::vector<TLine*> line_vector;
  line_vector _frames;
  line_vector _trks;

  typedef std::vector<TMarker*> marker_vector;
  marker_vector _gap_coord;
  marker_vector _mc_hit;

  unsigned short _arm;
  unsigned short _octant;
  int _station;
  int _event;

  TMutGapCoordMap* _gap_coord_map;
  TMutStubMap* _stub_map;
  TMutTrkMap* _trk_map;
  TMutMCHitMap* _mc_hit_map;

  typedef std::pair<double,double> coord_pair;
  typedef std::vector< coord_pair > coord_vector;
  std::vector<coord_vector> _tracks;

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
  
};



#endif

