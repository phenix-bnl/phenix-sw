// $Id: MUIGEOM.h,v 1.3 2007/04/05 08:03:25 hpereira Exp $

#ifndef MUIGEOM_h
#define MUIGEOM_h

/*!
  \file MUIGEOM.h
  \brief widely used utility functions and enumerations
  \author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2007/04/05 08:03:25 $
*/

#ifndef __CINT__
#include <gsl/gsl_math.h>
#endif

#include <string>
#include <iostream>

//! widely used utility functions and enumerations
namespace MUIGEOM {

  //! module verbosity
  enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};
  
  //! print a message to cout stream
  inline void TRACE( const std::string& message)
  { std::cout << "TRACE: " << message << std::endl; }

  //! print a message and a value to cout stream
  inline void TRACE( const std::string& message, const float& val )
  { std::cout << "TRACE: " << message << "\t" << val << std::endl; }
  
  //! print a message (formated) to a stream
  void PRINT(std::ostream& os = std::cout, const std::string& message = "");
    
  //! square a number of a given type
#ifndef __CINT__
  template < typename T >
  T SQUARE(const T& x)
  { return x*x; } 
#endif
  
};

#endif
