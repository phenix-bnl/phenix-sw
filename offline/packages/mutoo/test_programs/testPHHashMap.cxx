#include <iostream>
#include<map>
#include<algorithm>
#include<PHException.h>
#include<MUTOO.h>
#include<PHHashMap.h>
#include<PHCompactHashMap.h>
#include<PHMap.h>
#include<TMutHit.hh>
#include<PHTimer.h>
#include<boost/pool/object_pool.hpp>
/*! \ingroup test */
/*! \file testPHHashMap.cxx 
\brief exercise PHHashMap, PHHashMapIterator
*/

void PRINT(boost::shared_ptr<int> in);

void test_hash();
void test_compact_hash();
void test_map();


int main(){  
  MUTOO::TRACE("testPHHashMap");
  test_compact_hash();
  test_hash();
  test_map();    
}

void PRINT(boost::shared_ptr<int> in){
  std::cout << *in << std::endl;
}
const int NENTRIES=100;
const int NTRIAL=1000;
void test_map(){
  PHTimer map_timer("map timer");

  typedef PHMap<int,TMutHit> MyMap;
  MyMap* map_ptr = new MyMap();
  map_timer.restart();
  for(int itrial = 0; itrial<NTRIAL; ++itrial){
    for(int i=0; i<NENTRIES; i++){
      map_ptr->insert(i,new TMutHit(Key(0,i),1,1,1,1,1,1,i));
    }
    {
      MyMap::iterator iter = map_ptr->find(0,NENTRIES);
      while(MyMap::pointer hit_ptr = iter.next()){
	hit_ptr->get();
      }
    }
    {
      MyMap::iterator iter = map_ptr->range();
      while(MyMap::pointer hit_ptr = iter.next()){
	hit_ptr->get();
      }
    }
    map_ptr->clear();
  }
  map_timer.print();
}

void test_hash(){

  PHTimer hash_timer("hash timer");
  typedef PHHashMap<int,TMutHit> MyHashMap;
  // instantiate and populate a PHHashMap
  // 
  MyHashMap* hash_map_ptr = new MyHashMap();
  //  hash_map_ptr->reserve(0,NENTRIES);
  hash_timer.restart();
  for(int itrial = 0; itrial<NTRIAL; ++itrial){
    for(int i=NENTRIES; i>0; --i){
      hash_map_ptr->insert(0,new TMutHit(Key(0,i),1,1,1,1,1,1,i));
    }
    //    hash_map_ptr->sort();
    {
      MyHashMap::const_iterator iter = hash_map_ptr->range();
      while(MyHashMap::const_pointer hit_ptr = iter.next()){
	hit_ptr->get();
      }
    }
    {
      MyHashMap::iterator iter = hash_map_ptr->range();
      while(MyHashMap::pointer hit_ptr = iter.next()){
	hit_ptr->get();
      }
    }
    hash_map_ptr->clear();
  }
  hash_timer.print();
}

void test_compact_hash(){
  
  PHTimer hash_timer("compact hash timer");
  typedef PHCompactHashMap<int,TMutHit,1> MyHashMap;
  // instantiate and populate a PHHashMap
  // 
  MyHashMap* hash_map_ptr = new MyHashMap();
  //  hash_map_ptr->reserve(0,NENTRIES);
  hash_timer.restart();
  for(int itrial = 0; itrial<NTRIAL; ++itrial){
    for(int i=NENTRIES; i>0; --i){
      hash_map_ptr->insert(0,new TMutHit(Key(0,i),1,1,1,1,1,1,i));
    }
    //    hash_map_ptr->sort();
    {
      MyHashMap::iterator iter = hash_map_ptr->find(0);
      while(MyHashMap::pointer hit_ptr = iter.next()){
	hit_ptr->get();
      }
    }
    {
      MyHashMap::iterator iter = hash_map_ptr->range();
      while(MyHashMap::pointer hit_ptr = iter.next()){
	hit_ptr->get();
      }
    }
    hash_map_ptr->clear();
  }
  hash_timer.print();
}



