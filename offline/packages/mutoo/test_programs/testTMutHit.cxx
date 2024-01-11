#include<iostream>
#include<map>

#include"TMutHitMap.h"
#include"TRandom.h"
#include<boost/timer.hpp>

/*! @defgroup test Test Programs
  These small routines are useful for understanding  usage semantics.
*/

/*! @ingroup test */

/*! \file testTMutHit.cxx 
\brief exercise TMutHitMap, TMutKeyGen, PHMap, PHMapIterator, PHConstMapIterator
*/

void populate_hit_map(TMutHitMap* hit_map_ptr);
void fill_hit_map(TMutHitMap* hit_map_ptr);

int main()
{
  TMutHit* foo = new TMutHit();
  int a[10];
  a[11] = 10;
  std::cout << a[11] << std::endl;
  typedef TMutHitMap::iterator iterator;
  typedef TMutHitMap::pointer pointer;
  boost::timer timer;
  
  // populate the hit container
  // 
  TMutHitMap* hit_map_ptr = new TMutHitMap();
  populate_hit_map(hit_map_ptr);
  //  fill_hit_map(hit_map_ptr);

  Int_t arm=0;
  Int_t station=0;
  Int_t octant=0;  
  Int_t half_octant=0;  
  Int_t gap=2;
  Int_t cathode=1;

  // get an iterator to all hits specified cathode
  //
  TMutHitMap::const_iterator iter = hit_map_ptr->get(arm, 
						     station, 
						     octant, 
						     half_octant, 
						     gap, 
						     cathode);

  // next() returns a pointer to the current value and then advances
  // the iterator
  //
  MUTOO::TRACE("method one");
  MUTOO::TRACE("number of hits in plane",iter.count());
  while(TMutHitMap::const_pointer hit_ptr = iter.next()){
    hit_ptr->get();
  }  
  
  // reset() sets iterator to iter.begin(), iter++ increments the iterator
  //    
  MUTOO::TRACE("method two");
  for(iter.reset();!iter.at_end();++iter){
    iter->get();
  }  
  
  MUTOO::TRACE("elapsed time",timer.elapsed());  
  MUTOO::TRACE("test the io functionality");
  //  hit_map_ptr->write_array();
  //  hit_map_ptr->print_array();

  hit_map_ptr->clear();
  return 0;
}

void populate_hit_map(TMutHitMap* hit_map_ptr) {
  // populate the hit container
  // 
  TRandom r;
  enum {ARMS = 1, STATIONS=3, OCTANTS=8, HALF_OCTANTS=2, GAPS=3, CATHODES=2, STRIPS=1024};
  int i_count=0;
  for(int i_arm=0; i_arm<ARMS; ++i_arm){
    for(int i_station=0; i_station<STATIONS; ++i_station){
      for(int i_octant=0; i_octant<OCTANTS; ++i_octant){
	for(int i_half_octant=0; i_half_octant<HALF_OCTANTS; ++i_half_octant){
	  for(int i_gap=0; i_gap<GAPS; ++i_gap){
	    for(int i_cathode=0; i_cathode<CATHODES; ++i_cathode){
	      for(int i_strip=0; i_strip<STRIPS; ++i_strip){
		if(r.Uniform(1) < 0.1) {
		  TMutHitMap::iterator i = hit_map_ptr->insert_new(i_arm, 
								   i_station, 
								   i_octant, 
								   i_half_octant, 
								   i_gap, 
								   i_cathode, 
								   i_strip);
		  i->get()->set_q(r.Gaus());
		}
	      }
	    }
	  }
	}
      }
    }
  }
}

void fill_hit_map(TMutHitMap* hit_map_ptr) {
  // populate the hit container
  // 
  TRandom r;
  enum {ARMS = 1, STATIONS=3, OCTANTS=8, HALF_OCTANTS=2, GAPS=3, CATHODES=2, STRIPS=2};
  int i_count=0;

  Int_t i_arm = 0;
  Int_t i_station = 0;
  Int_t i_octant = 0;
  Int_t i_half_octant = 0;
  Int_t i_gap = 0;
  //  for(int i_octant=0; i_octant<OCTANTS; ++i_octant){
  //    for(int i_half_octant=0; i_half_octant<HALF_OCTANTS; ++i_half_octant){
  for(int i_gap=0; i_gap<GAPS; ++i_gap){
    for(int i_cathode=0; i_cathode<CATHODES; ++i_cathode){
      for(int i_strip=0; i_strip<STRIPS; ++i_strip){
	TMutHitMap::iterator i = hit_map_ptr->insert_new(i_arm, 
							 i_station, 
							 i_octant, 
							 i_half_octant, 
							 i_gap, 
							 i_cathode, 
							 i_strip);
	i->get()->set_q(r.Gaus());
	i->get()->print();
      }
    }
  }
      //  }
      //}
}








