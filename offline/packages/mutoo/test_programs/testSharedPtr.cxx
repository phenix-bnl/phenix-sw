#include<boost/smart_ptr.hpp>
#include<PHMap.h>
#include<MUTOO.h>
#include<TObject.h>

/*! \ingroup test */
/*! \file testSharedPtr.cxx 
\brief exercise Boost library shared_ptr
*/

struct TestObject : public TObject {
public:
  TestObject() {;}
  TestObject(int i) : _i(i) {;}
  int get_i() {return _i;}
  void print(std::ostream& os) const {;}
private:
  int _i;
};

using namespace boost;

int main(){  

  MUTOO::TRACE("testSharedPtr");

  shared_ptr<TestObject> ptr(new TestObject(99));
  MUTOO::TRACE("use count",ptr.use_count());
  
  shared_ptr<TestObject> ptr2(ptr);
  MUTOO::TRACE("use count",ptr.use_count());
  
  // this will not compile
  // TestObject* ptr3 = ptr;

  shared_ptr<TestObject> ptr3 = ptr2;
  MUTOO::TRACE("use count",ptr.use_count());
  
  typedef PHMap<int, TestObject > Map;
  Map* map_ptr = new Map();
  
  map_ptr->insert(1,TestObject(100));
  map_ptr->insert(2,TestObject(101));
  
  Map::iterator iter = map_ptr->find(1,2);
  MUTOO::TRACE("get()", iter->get()->get_i());

}








