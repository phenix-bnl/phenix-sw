#ifndef __TMUTHALFOCTANTBIDIMVIEW_HH__
#define __TMUTHALFOCTANTBIDIMVIEW_HH__

#include<TMarker.h>
#include<TCanvas.h>
#include<TPaveLabel.h>
#include<PHFrame.h>
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
#include<map>
#include<vector>
#include<boost/array.hpp>
#include<TLine.h>
#include<TMarker.h>

class TH2S;
class TH2F;
class MutArm;

/*! \ingroup display */
//!Draws 2D histograms from MUTR strips in a half_octant.
/*!
  More details later.
*/
class TMutHalfOctantBidimView
{
 public:
  
  TMutHalfOctantBidimView(unsigned short arm, unsigned short octant, unsigned short halfoctant, unsigned short station, int event=-1);
  
  virtual ~TMutHalfOctantBidimView();
  
  Bool_t event(PHCompositeNode* top_node);
  
  /*! Paint the plane view for the specified half-octant */
  void paint();
  
  /*! Update the canvas */
  void update(){_canvas->Update();}
  
  /*! Get arm */  
  unsigned short get_arm() const { return _arm;}
  
  /*! Get octant */  
  unsigned short get_octant() const { return _octant;}
  
  /*! Get halfoctant */  
  unsigned short get_halfoctant() const { return _halfoctant;}
  
  /*! Get station */  
  unsigned short get_station() const { return _station;}

 protected:
	  
/*   typedef std::map< TMutHitMap::pointer, double > HitSet; */
  typedef std::map< TMutHitMap::const_pointer, double > HitSet;
  typedef std::pair< int, HitSet > Cell;
  typedef std::vector< Cell > Grid;
  
  //! fill grid
  void fill_grid( Grid &grid );
  void fill_grid( Grid &grid, TMutHitMap::const_pointer hit_ptr);
  
  //! fill bidim
  void fill_bidim(Grid &grid);

  //! draw MChit's
  void draw_mchits(void);

  int number_of_orientations(Grid &grid, int grid_index);

  float charge(Grid &grid, int grid_index);
	  
 private:  
  
  /*! Clear the graphic primitives in temp storage */
  void clear_temp_storage();
  
  /*! Setup canvas and pad for this view */
  void setup_canvas();
  
  /*! Get IOC pointers */
  void set_interface_ptrs(PHCompositeNode* top_node);
  
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
  unsigned short _halfoctant;
  unsigned short _station;
  int _event;

  PHFrame _bidimFrame;

  // Canvas/Pad members for this view
  //
  TCanvas* _canvas;
  TPaveLabel* _label;
  TPad* _pad0;
  TPad* _pad1;
  TPad* _pad2;
  TPad* _pad3;
  TH2S* _bidim0;
  TH2S* _bidim1;
  TH2F* _bidim2;

  bool _is_monte_carlo;

};

#endif

