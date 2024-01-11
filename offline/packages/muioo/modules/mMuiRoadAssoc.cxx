/////////////////////////////////////////////////////////////////////////
//
// Utility class: mMuiRoadAssoc:
// Author: Matt Wysocki 
// Date: 2/12/04
// Description: Takes a track and a list of candidate roads and associates
//              the road closest to the extrapolated track.
//              
/////////////////////////////////////////////////////////////////////////

// MUTOO headers
#include <mMuiRoadAssoc.h>
#include <mMuiRoadAssocPar.h>
//INCLUDECHECKER: Removed this line: #include <TMutNode.h>
//INCLUDECHECKER: Removed this line: #include <PHException.h>
#include <PHTimer.h>
#include <TMutTrkMap.h>
#include <TMuiRoadMapO.h>
//INCLUDECHECKER: Removed this line: #include <PHPoint.h>
#include <TMutTrackUtil.h>
//INCLUDECHECKER: Removed this line: #include <MUTOO.h>
#include "MUIOO.h"

// PHENIX headers

/*! \ingroup modules */

// STL/BOOST
#include <iostream>
#include <string>
#include <map>

mMuiRoadAssoc::mMuiRoadAssoc() : _timer("mMuiRoadAssoc")
{
  MUIOO::TRACE("initializing module mMuiRoadAssoc");  
}

// Event method.
PHBoolean mMuiRoadAssoc::event(PHCompositeNode* top_node)
{
  _timer.restart(); 
  
  try { 

    // Reset IOC pointers
    //
    set_interface_ptrs(top_node);

    // convenience typedefs
    //
    typedef std::multimap<UShort_t, TMuiRoadMapO::value_type> road_multimap;
    typedef std::multimap<UShort_t, TMuiRoadMapO::value_type>::iterator  multimap_iterator;
    typedef std::pair<multimap_iterator,multimap_iterator> Range;         

    // Loop over all the tracks....wheeeeeee......
    TMutTrkMap::iterator trk_iter = _trk_map->range();
    while( TMutTrkMap::pointer trk_ptr = trk_iter.next() ) {

      road_multimap roads_to_test;
      TMuiRoadMapO::key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();

      // add each road to multimap and disassociate it from the track:
      while( TMuiRoadMapO::pointer road_ptr = road_iter.next() ) {
	roads_to_test.insert( std::make_pair( road_ptr->get()->get_depth(), road_ptr->get() ));
	PHKey::disassociate(trk_ptr, road_ptr);
      } 

      // NOW TEST EACH ROAD TO SEE WHICH IS CLOSEST...

      // Loop over each depth:
      for(UShort_t depth=2;depth<5;++depth){

	// Loop over roads of given depth:
	Range range = roads_to_test.equal_range(depth); //get begin,end iters to roads of (depth)
	multimap_iterator map_iter = range.first;       // make iter to use, starting at begin

	// start with first road
	double closest_dist = DBL_MAX;
	multimap_iterator closest_iter = map_iter;     // keep iter to closest road

	// now see if any road is closer...
	for(; map_iter!=range.second; ++map_iter){

	  // Set extrapolated point to test the road against...
	  PHPoint road_point = map_iter->second->get_gap0_point();
	  double z0 = (map_iter->second->get_fit_par()).get_z();
	  const TMutTrkPar* trk_par = trk_ptr->get()->get_trk_par_station(MUTOO::Station3);
	  TMutFitPar* lcl_fitpar = new TMutFitPar(trk_par->get_x(),
						  trk_par->get_y(),
						  trk_par->get_z(),
						  ( trk_par->get_px()/trk_par->get_pz() ), // dx/dz
						  ( trk_par->get_py()/trk_par->get_pz() ), // dy/dz
						  trk_par->get_chi_square() );
	  const PHPoint extrap_point = TMutTrackUtil::linear_track_model(lcl_fitpar, z0);
	  delete lcl_fitpar;

	  double dist = distance( extrap_point, road_point );
	  if( dist < closest_dist ){
	    closest_dist = dist;
	    closest_iter = map_iter;
	  }
	}
	if( closest_iter != range.second ) // in case a depth has no roads
	  PHKey::associate(*trk_ptr, closest_iter->second); // for each depth, assoc closet road with track

      } //end depth loop
    } //end track loop

  } catch(std::exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }

  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.print();  
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _trk_map->print();  
  return True;
}

/*! Reset IOC and external interface pointers */
void mMuiRoadAssoc::set_interface_ptrs(PHCompositeNode* top_node){  

  // module runtime parameters
  //
  _mod_par = TMutNode<mMuiRoadAssocPar>::find_node(top_node,"mMuiRoadAssocPar");

  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  
} 

double mMuiRoadAssoc::distance(PHPoint p1, PHPoint p2) {
  double x1 = p1.getX();
  double y1 = p1.getY();
  double z1 = p1.getZ();
  double x2 = p2.getX();
  double y2 = p2.getY();
  double z2 = p2.getZ();

  return std::sqrt( MUIOO::SQUARE(x2-x1)+MUIOO::SQUARE(y2-y1)+MUIOO::SQUARE(z2-z1));
}
