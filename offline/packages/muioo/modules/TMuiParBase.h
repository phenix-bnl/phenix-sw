#ifndef __TMUIPARBASE_HH__
#define __TMUIPARBASE_HH__

#include <iostream>
#include <PHObject.h>
#include <MUIOO.h>

//! Base class for runtime parameter objects.  
/*! 
  Satisfies PHOOL interface requirements and provides interface
  to run-time parameters that are common to all modules.
*/

class TMuiParBase : public PHObject
{

 public: 

  /*! default constructor */
  TMuiParBase() : 
    _verbosity(MUIOO::NONE){;}
  
  /*! destructor */
  virtual ~TMuiParBase(){;}

  /*! PHOOL inteface requirement */
  virtual void identify(std::ostream& os = std::cout) const {
    os << "TMuiParBasePar";
  }

  /*! PHOOL interface requirement -- null op */
  virtual void Reset(){;}

  /*! PHOOL interface requirement -- always returns 1 */
  virtual int isValid() const {return 1;}

  /*! Verbosity level */
  MUIOO::Verbosity get_verbosity() const {return _verbosity;}   

  /*! Verbosity level */
  void set_verbosity(MUIOO::Verbosity verbosity) {_verbosity = verbosity;}

 protected:  

  MUIOO::Verbosity _verbosity;

};

#endif /* __TMUIPARBASE_HH__ */
