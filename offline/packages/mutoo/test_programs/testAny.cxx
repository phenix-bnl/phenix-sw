#include<iostream>
#include<stdexcept>
#include<sstream>
#include<map>
#include<algorithm>
#include"PHException.h"
#include"TMutHit.hh"
#include"MUTOO.h"
#include<boost/timer.hpp>
#include<boost/any.hpp>

/*! @ingroup test */

/*! \file testAny.cxx 
\brief test BOOST any
*/

class MyClassBase {
 public:
  virtual int get() = 0;
};

class MyClass : public MyClassBase {
public:
  MyClass() {_value = 99;}
  int get() { return _value; }
  int constant(int in) { 
    static int a = in;
    std::cout << a << std::endl;
    return in;
  }
private:
  int _value;
};

int main()
{    
  MUTOO::TRACE("test static function scope");
  MyClass foo;
  foo.constant(1);
  foo.constant(2);
  MyClass blah;
  blah.constant(2);
  blah.constant(2);
  MUTOO::TRACE("testAny");
  std::map<int,boost::any> map;
  {
    MyClassBase* base_ptr = new MyClass();
    map.insert(std::make_pair(1,base_ptr));
  }
  boost::any any_ptr = (map.find(1))->second;
  try {
    MyClassBase* base_ptr = boost::any_cast<MyClassBase*>(any_ptr);
    MUTOO::TRACE("any cast to base pointer test",base_ptr->get());
  } catch (std::exception& e) {
    MUTOO::TRACE(e.what());
  }

  // this won't work
  //
  try {
    MyClass* derived_ptr = boost::any_cast<MyClass*>(any_ptr);
    MUTOO::TRACE("any cast to derived pointer test",derived_ptr->get());
  } catch (std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  
  //  MyClass* derived_ptr = boost::any_cast<MyClass*>(any_ptr);
  //  MUTOO::TRACE("any cast test",derived_ptr->get());

}









