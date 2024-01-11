#include <iostream>
#include <PHKeyIterator.h>
#include <PHException.h>
#include <MUTOO.h>
#include <PHConstKeyIterator.h>
#include <PHMap.h>
#include <PHKey.hh>

/*! \ingroup test */
/*! \file testPHKeyIter.cxx 
\brief exercise PHKeyIterator, PHMapIterator, PHKey, PHMap
*/

class TestObject : public PHKey {
public:
  float x;
  PHKeyList key_list;
};

int main(){  

  MUTOO::TRACE("testPHKeyIter");

  typedef PHMap<PHKey::key_type,int> MyMap;

  // instantiate and populate a new map
  //
  MyMap* map_ptr = new MyMap();
  TestObject tmp;
  for(int i=0; i<5; i++){
    Key key(map_ptr->get_map_key(),i);
    map_ptr->insert(key,1000+i);
    tmp.key_list.add_key(key);
  }

    MUTOO::TRACE("check looping with PHKeyIterator::next");
    PHKeyIterator<PHKey::key_type,int> iter(tmp.key_list.get_keys());
    while(MyMap::pointer i = iter.next()){
      std::cout << *(i->get()) << std::endl;
    }  

  MUTOO::TRACE("check looping with PHConstKeyIterator::next");
  PHConstKeyIterator<PHKey::key_type,int> iter2(tmp.key_list.get_keys());
  while(MyMap::const_pointer i = iter2.next()){
    std::cout << *(i->get()) << std::endl;
  }
  
  MUTOO::TRACE("check looping with PHKeyIterator::operator++()");
  for(iter.reset(); iter!=iter.end();++iter){
    std::cout << *(iter->get()) << std::endl;
  }
  
  MUTOO::TRACE("check looping with PHConstKeyIterator::operator++()");
  for(iter2.reset(); iter2!=iter2.end();++iter2){
    std::cout << *(iter2->get()) << std::endl;
  }

}







