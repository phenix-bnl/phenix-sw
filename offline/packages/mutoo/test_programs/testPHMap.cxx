#include <iostream>
#include<map>
#include<algorithm>
#include<PHException.h>
#include<MUTOO.h>
#include <PHMap.h>
/*! \ingroup test */
/*! \file testPHMap.cxx 
\brief exercise PHMap, PHMapIterator
*/

void PRINT(boost::shared_ptr<int> in);

int main(){  

  MUTOO::TRACE("testPHMap");
  
  typedef std::map<int,int> Map;
  typedef PHMap<int,int> MyMap;
  
  // instantiate and populate a PHMap
  // 
  MyMap* map_ptr = new MyMap();
  for(int i=0; i<5; i++){
    map_ptr->insert(i,1000+i);
  }

  MUTOO::TRACE("key find");
  MyMap::iterator iter = map_ptr->find(2);
  std::cout << *(iter->get()) << std::endl;

  MUTOO::TRACE("single element range");
  while(MyMap::pointer i = iter.next()){
    std::cout << *(i->get()) << std::endl;
  }

  MUTOO::TRACE("multiple element range");  
  MyMap::iterator iter2 = map_ptr->find(2,5);
  while(MyMap::pointer i = iter2.next()){
    std::cout << *(i->get()) << std::endl;
  }

  MUTOO::TRACE("boundary conditions");
  // handles requests that are superset of map
  //
  MyMap::iterator iter3 = map_ptr->find(-11,11);
  while(MyMap::pointer i = iter3.next()){
    std::cout << *(i->get()) << std::endl;
  }
  
  MUTOO::TRACE("range()");
  // check range
  //
  MyMap::iterator iter4 = map_ptr->range();
  while(MyMap::pointer i = iter4.next()){
    std::cout << *(i->get()) << std::endl;
  }
  
  MUTOO::TRACE("loop syntax");  
  for( MyMap::iterator iter5 = map_ptr->range(); !iter5.at_end(); ++iter5){
    std::cout << *(iter5->get()) << std::endl;
  }

  MUTOO::TRACE("loop syntax");  
  for( MyMap::iterator iter5 = map_ptr->range(); iter5 != iter5.end(); ++iter5){
    std::cout << *(iter5->get()) << std::endl;
  }

  MUTOO::TRACE("STL forward iterator");  
  MyMap::iterator iter5 = map_ptr->range();
  std::for_each(iter5.begin(),iter5.end(),PRINT);

  MUTOO::TRACE("erase()");
  map_ptr->erase(3);
  for( MyMap::const_iterator iter5 = map_ptr->range(); iter5 != iter5.end(); ++iter5){
    std::cout << *(iter5->get()) << std::endl;
  }

  MUTOO::TRACE("const iterator");
  MyMap::const_iterator iter6 = map_ptr->range();
  //  while(int* i = iter6.next()){  ERROR -- won't compile
  while(const MyMap::const_pointer i = iter6.next()){
    std::cout << *(i->get()) << std::endl;
  }

  MyMap::const_iterator iter7 = map_ptr->find(2,5);
  std::cout << *(iter7->get()) << std::endl;
  // *iter7 = 99; ERROR -- won't compile

  MUTOO::TRACE("check clear()");
  map_ptr->clear();
  if(!map_ptr->empty()) {
    MUTOO::TRACE("clear failed");
  } else {
    MUTOO::TRACE("clear succeded");    
  }

}

void PRINT(boost::shared_ptr<int> in){
  std::cout << *in << std::endl;
}






