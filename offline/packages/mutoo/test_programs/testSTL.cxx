#include<iostream>
#include<stdexcept>
#include<sstream>
#include<map>
#include<algorithm>
#include"PHException.h"
#include"TMutHit.hh"
#include"MUTOO.h"
#include"MUTOO_HASH_MAP.h"
#include<boost/timer.hpp>
void always_throws();
void PRINT(std::pair<int,int>);


/*! @ingroup test */

/*! \file testSTL.cxx 
\brief exercise <map> <hash_map> <boost/timer.hpp>
*/

class dummy {
public:
  void print() { 
    std::cout << "hello world" << std::endl; 
    std::cout << _i << std::endl;
  }
  void set_i(int i){_i = i;}
private:
  int _i;
};

struct print_ftor 
{
  void operator()(const MUTOO::hash_map<int,dummy>::value_type& val) 
  {
    val.second.set_i(99);
    val.second.print();
  }
};

int main()
{    
  MUTOO::TRACE("testSTL");
  print_ftor foo;
  const int NENTRIES = 5000;
  std::map<int,int> map;
  MUTOO::hash_map<int,dummy> hash_map(NENTRIES);
  hash_map[1] = dummy();
//    std::transform(hash_map.begin(),
//  		 hash_map.end(),
//  		 ostream_iterator<int>(std::cout, " "),
//  		 std::select1st<std::hash_map<int,int>::value_type>());
  std::cout << std::endl;
  std::for_each(hash_map.begin(),
  		hash_map.end(), 
  		print_ftor());
		

//  std::for_each(hash_map.begin(),hash_map.end(),select1
//    int  array[NENTRIES];
//    boost::timer timer;
  
//    for(int i=0; i<NENTRIES;++i){
//      map.insert(std::make_pair(i,i));
//    }
//    MUTOO::TRACE("std::map insertion: ",1000*timer.elapsed());

//    timer.restart();
//    for(int i=0; i<NENTRIES;++i){
//      hash_map.insert(std::make_pair(i,i));
//    }
//    MUTOO::TRACE("MUTOO::hash_map insertion: ",1000*timer.elapsed());

//    timer.restart();
//    for(int i=0; i<NENTRIES;++i){
//      array[i] = i;
//    }
//    MUTOO::TRACE("c-style array insertion:",1000*timer.elapsed());

//    timer.restart();
//    for(int i=0; i<NENTRIES;++i){
//      int j = map.find(i)->second;
//    }
//    MUTOO::TRACE("std::map extraction: ",1000*timer.elapsed());

//    timer.restart();
//    for(int i=0; i<NENTRIES;++i){
//      int j = hash_map.find(i)->second;
//    }
//    MUTOO::TRACE("MUTOO::hash_map insertion",1000*timer.elapsed());

//    timer.restart();
//    for(int i=0; i<NENTRIES;++i){
//      for(int j = 0; j<NENTRIES; ++j){ 
//        if(j = NENTRIES/2.0){
//  	int k = array[j];
//  	break;
//        }
//      }
//    }
//    MUTOO::TRACE("c-style array extraction:",1000*timer.elapsed());

}

void always_throws() {
  throw std::out_of_range(DESCRIPTION("out of range"));
}








