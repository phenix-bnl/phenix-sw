#include<iostream>
#include<map>

//#include<TMutClus_v1.hh>
//#include<TMutHitMap.h>
#include<TRandom.h>
#include<MUTOO.h>
//#include<TMutClusMap.h>
/*! @ingroup test */

/*! \file testTMutClus.cxx 
\brief exercise TMutClus, TMutHitMap, TMutKeyGen, PHMap, PHKeyIterator PHConstKeyIterator
PHMapIterator, PHConstMapIterator
*/

//void populate_hit_map(TMutHitMap* hit_map);

int main()
{
//    TMutClus();
//    TMutClus_v1();
//    MUTOO::TRACE("testTMutClus");
  
//    try {

//      TMutHitMap* hit_map = new TMutHitMap();
  //  TMutClusMap* clus_map = new TMutClusMap();
//      populate_hit_map(hit_map);
    
//      Int_t arm=0;
//      Int_t station=1;
//      Int_t octant=4;
//      Int_t half_octant=1;  
//      Int_t gap=2;
//      Int_t cathode=1;
    
//      // get an iterator to all hits in station 1, half_octant 14, plane 2
//      //
//      TMutHitMap::iterator iter = hit_map->get(arm, 
//  					     station, 
//  					     octant,
//  					     half_octant, 
//  					     gap,
//  					     cathode);
//      while(TMutHitMap::pointer hit_ptr = iter.next()){
//        hit_ptr->get()->print();
//      }
    
//      // instantiate a cluster object
//      //
//      TMutClusMap::iterator clus_iter  = clus_map->insert_new(arm,
//  							    station,
//  							    octant,
//  							    half_octant,
//  							    gap,
//  							    cathode);
//      if(clus_iter.at_end()) {
//        throw std::runtime_error(DESCRIPTION("TMutClusMapinsertion failed"));
//      }
    
//      clus_iter->get()->set_arm(arm);
//      clus_iter->get()->set_station(station);
//      clus_iter->get()->set_octant(octant);
//      clus_iter->get()->set_half_octant(half_octant);
//      clus_iter->get()->set_gap(gap);
//      clus_iter->get()->set_cathode(cathode);
    
//      // append hits to cluster
//      //
//      MUTOO::TRACE("Adding hits to cluster");
//      iter.reset();
//      while(TMutHitMap::pointer hit_ptr = iter.next()){      
//        //      PHKey::associate(hit_ptr,clus_iter.current());
//        clus_iter.current()->get()->get_key_list().add_key(hit_ptr->get()->get_key());
//      }

//      // read back hits from cluster
//      //
//      MUTOO::TRACE("Reading back hits from cluster");
//      try {
//        TMutHitMap::key_iterator key_iter = clus_iter->get()->get_associated<TMutHit>();
//        while(TMutHitMap::pointer hit_ptr = key_iter.next()){
//  	hit_ptr->get()->print();
//        }
//      } catch (std::exception& e){
//        std::cout << e.what() << std::endl;
//      }
//      // check that the reverse associations were established
//      //
//      MUTOO::TRACE("Reading back clusters from first hit");
//      iter.reset();
//      try {
//        TMutClusMap::key_iterator key_iter = iter->get()->get_associated<TMutClus>();
//        while(TMutClusMap::pointer clus_ptr = key_iter.next()){
//  	clus_ptr->get()->print();
//        }
//      } catch (std::exception& e){
//        std::cout << e.what() << std::endl;
//      }

//      // check disassociate works
//      //
//      MUTOO::TRACE("Remove first hit from cluster");
//      // Iterator over cluster hits
//      //
//      iter.reset();
//      while(TMutHitMap::pointer hit_ptr = iter.next()){
//        PHKey::disassociate(hit_ptr,clus_iter.current());
//        break;
//      }

//      MUTOO::TRACE("Reading back hits from cluster after disassociate");
//      try {
//        TMutHitMap::key_iterator key_iter = clus_iter->get()->get_associated<TMutHit>();
//        while(TMutHitMap::pointer hit_ptr = key_iter.next()){
//  	hit_ptr->get()->print();
//        }
//      } catch (std::exception& e){
//        std::cout << e.what() << std::endl;
//      }
//      // check that reverse dissassociate worked
//      //
//      MUTOO::TRACE("Reading back clusters from first hit after dissociate");
//      iter.reset();
//      try {
//        TMutClusMap::key_iterator key_iter = iter->get()->get_associated<TMutClus>();
//        while(TMutClusMap::pointer clus_ptr = key_iter.next()){
//  	clus_ptr->get()->print();
//        }
//      } catch (std::exception& e){
//        std::cout << e.what() << std::endl;
//      }
    
//    } catch (std::exception& e) {
//      std::cout << e.what() << std::endl;
//    }
  
//    return 0;
}

//void populate_hit_map(TMutHitMap* hit_map) {
  // populate the hit container
  // 
//    TRandom r;
//    enum {ARMS = 1, STATIONS=3, OCTANTS=8, HALF_OCTANTS=2, GAPS=3, CATHODES=2, STRIPS=3};
//    for(int i_arm=0; i_arm<ARMS; ++i_arm){
//      for(int i_station=0; i_station<STATIONS; ++i_station){
//        for(int i_octant=0; i_octant<OCTANTS; ++i_octant){
//  	for(int i_half_octant=0; i_half_octant<HALF_OCTANTS; ++i_half_octant){
//  	  for(int i_gap=0; i_gap<GAPS; ++i_gap){
//  	    for(int i_cathode=0; i_cathode<CATHODES; ++i_cathode){
//  	      for(int i_strip=0; i_strip<STRIPS; ++i_strip){
//  		TMutHitMap::iterator i = hit_map->insert_new(i_arm, 
//  							     i_station, 
//  							     i_octant, 
//  							     i_half_octant, 
//  							     i_gap, 
//  							     i_cathode, 
//  							     i_strip);
//  		i->get()->set_q(r.Gaus());
//  	      }
//  	    }
//  	  }
//  	}
//        }
//      }
//    }
//}





