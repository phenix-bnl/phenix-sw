#include<iostream>
#include<map>

#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<PHKeyIterator.h>
#include<TRandom.h>
#include<MUTOO.h>

/*! @ingroup test */

/*! \file testTMutMCHit.cxx 
  \brief exercise TMutMCHitMap
*/

void populate_hit_map(TMutMCHitMap* hit_map);

int main()
{
  
  MUTOO::TRACE("testTMutMCHit");
  try {
    
    TMutMCHitMap* hit_map = new TMutMCHitMap();
    TMutMCTrkMap* trk_map = new TMutMCTrkMap();

    populate_hit_map(hit_map);
    
    Int_t arm=0;
    Int_t station=1;
    Int_t octant=4;
    Int_t half_octant=1;  
    Int_t gap=2;
    
    // insert a new MC track
    //
    TMutMCTrkMap::iterator trk_iter = trk_map->insert_new(0);
    trk_iter->get()->set_x_orig(0.1);
    trk_iter->get()->set_y_orig(-0.2);
    trk_iter->get()->set_x_orig(3.);
    trk_iter->get()->set_px_orig(1.);
    trk_iter->get()->set_py_orig(2.);
    trk_iter->get()->set_pz_orig(3.);
    
    // associate the first hit in each gap with track
    //
    for(int i_gap=0;i_gap<=MUTOO::MAX_GAP;++i_gap){
      TMutMCHitMap::iterator hit_iter = hit_map->get(arm, 
						     station, 
						     octant,
						     half_octant, 
						     i_gap);
      
      if(!hit_iter.at_end()) PHKey::associate(trk_iter.current(),
					      hit_iter.current());
    }

    // print the MC track
    //
    MUTOO::TRACE("Print MC track");
    trk_iter->get()->print();
    
    // read back and print associated hits from cluster
    //
    MUTOO::TRACE("Readback MC hits from track");

    TMutMCHitMap::key_iterator hit_iter = 
      trk_iter->get()->get_associated<TMutMCHit>();

    while(TMutMCHitMap::pointer hit_ptr = hit_iter.next()){
      hit_ptr->get()->print();
    }

  } catch (std::exception& e) {
    std::cout << e.what() << std::endl;
  }
  return 0;
}

void populate_hit_map(TMutMCHitMap* hit_map) {
  // populate the hit container
  // 
  TRandom r;
  enum {ARMS = 1, STATIONS=3, OCTANTS=8, HALF_OCTANTS=2, GAPS=3, CATHODES=2, STRIPS=10};
  for(int i_arm=0; i_arm<ARMS; ++i_arm){
    for(int i_station=0; i_station<STATIONS; ++i_station){
      for(int i_octant=0; i_octant<OCTANTS; ++i_octant){
	for(int i_half_octant=0; i_half_octant<HALF_OCTANTS; ++i_half_octant){
	  for(int i_gap=0; i_gap<GAPS; ++i_gap){
	    hit_map->insert_new(i_arm, 
				i_station, 
				i_octant, 
				i_half_octant, 
				i_gap);
	  }
	}
      }
    }
  }
}





