#include <sstream>
#include <stdexcept>

/*! \ingroup classes */
/*! \file PHException.h 
MUTOO uses the standard exceptions in <stdexcept> with a macro
DESCRIPTION(message) to append file and line information to the user
supplied message.  The standard exceptions inherit from either
logic_error or runtime_error.  Coding errors such as trying to advance
an iterator beyond its bounds will throw a derivative of logic_error and
are usually not recoverable. Whereas exceptional conditions resulting 
from runtime calculations will throw a derivative of runtime_error and
typically can be handled.  An additional macro BOUNDS_CHECK(value,bound)
is defined specificially for protecting against access violations 
associated with c-style array access.  BOUNDS_CHECK is conditionally
compiled to a null-op if NDEBUG is defined.  We use macros here (and only
here) to take advantage of the ANSI __FILE__ and __LINE__ macros.
*/

/*! \def DESCRIPTION(message)
  Generates a exception string for use as
  a construction argument to a std::exception or
  derivative.  The ANSI __FILE__ and __LINE__
  macros for are used for file location and line 
  number information.
*/
#define DESCRIPTION(message) std::string("Exception: ") + \
std::string(message) + ExceptionSuffix::get(__FILE__,__LINE__)

/*! \def BOUNDS_CHECK(value,bound)
  Throws bounds_check_failed if (value < bound) is false.
*/ 
#ifndef NDEBUG
#define BOUNDS_CHECK(value,bound) if(!(value < bound)) \
throw bounds_check_failed(DESCRIPTION("Attempt to access invalid index: "),value,bound)
#else
#define BOUNDS_CHECK(value,bound)
#endif

#ifndef __PHEXCEPTION_H__
#define __PHEXCEPTION_H__

/*! Bound checked failed.  Attempt to access an invalid array index. */
class bounds_check_failed : public std::out_of_range
{
 public:
  /*! Construct with message, index and bound */
  bounds_check_failed(const std::string& message, int index, int bound) : 
    std::out_of_range(""), 
    _message(message),
    _index(index),
    _bound(bound){;}
  virtual ~bounds_check_failed() throw () {}
  /*! Description */
  virtual const char* what() const throw () {
    std::ostringstream tmp;
    tmp << _message << " index: " << _index << " bound: " << _bound;  
    _msgout = tmp.str();
    return _msgout.c_str();
  }  
 private:
  std::string _message;
  // this string only exists so the thrown message does not go out of scope
  // since it is changed in a const method it has to be declared mutable
  mutable std::string _msgout;
  int _index;
  int _bound;
};
/* Suffix class for appending __FILE__ and __LINE__ macros 
   to description string */
class ExceptionSuffix {
 public:
  static std::string get(std::string file, int i){
    std::ostringstream tmp;
    tmp << i;
    return "  file:" + file + " " + "line:" + tmp.str();
  }
};

#endif



