#include <iostream>
#include<map>
#include<algorithm>
#include<PHException.h>
#include<MUTOO.h>
#include <PHMap.h>
/*! \ingroup test */
/*! \file testPHMapManager.cxx 
\brief exercise PHMap, PHMapBase, PHMapManager
*/

int main(){  



  //
  // Developers Note: PHMaps and all derived classes
  // can *only* be heap allocated.  Upon instantiation
  // ownership belongs to PHMapManager. (The user never
  // calls delete on a map pointer)
  //
  MUTOO::TRACE("testPHMapManager");

  typedef PHMap<int,int> MyMap;
  MyMap* map_one = new MyMap();
  MUTOO::TRACE("map one key",map_one->get_map_key());

  MyMap* map_two = new MyMap();
  MUTOO::TRACE("map two key",map_two->get_map_key());
  MUTOO::TRACE("maps under manager control", PHMapManager::size());

  // user the MapFinder to get a pointer to a map via its key
  //
  MUTOO::TRACE("test lookup by key");
  MyMap* map_ptr;
  try {
    map_ptr = MapFinder<MyMap*>::get(map_one->get_map_key());
    MUTOO::TRACE("any_cast successfull",map_ptr->get_map_key());
  } catch (std::exception& e) {
    MUTOO::TRACE(e.what());
  }

  MUTOO::TRACE("test write()");
  PHMapManager::write();

  MUTOO::TRACE("test clear()");
  PHMapManager::clear();
  MUTOO::TRACE("maps under manager control", PHMapManager::size());
}







