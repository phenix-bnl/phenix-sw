#include<iostream>
#include<stdexcept>
#include<sstream>
#include<map>
#include"PHException.h"
#include<MUTOO.h>
#include"TMutHit.hh"
#include<boost/timer.hpp>

void always_throws();

/*! @ingroup test */

/*! \file testPHException.cxx 
\brief exercise <stdexcept> DESCRIPTION macro and BOUNDS_CHECK macro
*/

int main()
{    

  MUTOO::TRACE("testPHException");

  // test DESCRIPTION macro
  //
  MUTOO::TRACE("Testing <stdexcept>");
  try { 
    always_throws();
  } catch(std::exception& e) {
    std::cout << e.what() << std::endl;
  }

  // test BOUNDS_CHECK macro
  //
  MUTOO::TRACE("Testing BOUNDS_CHECK macro");
  try { 
    BOUNDS_CHECK(2,3);
  } catch(std::exception& e) {
    std::cout << e.what() << std::endl;
  }

  // test BOUNDS_CHECK in TMutHit accessor
  //
  TMutHit hit;
  MUTOO::TRACE("Testing BOUNDS_CHECK in TMutHit accessor");
  try { 
    hit.get_amu(10);
  } catch(std::exception& e) {
    std::cout << e.what() << std::endl;
  }

}

void always_throws() {
  throw std::out_of_range(DESCRIPTION("out of range"));
}








