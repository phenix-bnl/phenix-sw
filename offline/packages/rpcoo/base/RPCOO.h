// $Id: RPCOO.h,v 1.4 2011/07/14 04:29:30 richi Exp $

#ifndef _RPCOO_h_
#define _RPCOO_h_

/*!
	\file RPCOO.h
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
  \version $Revision: 1.4 $
  \date    $Date: 2011/07/14 04:29:30 $
*/

#include <string>
#include <iostream>
#include <TDataType.h>

#ifndef __CINT__
#include "gsl/gsl_math.h"
#endif

//! widely used utility functions and enumerations
namespace RPCOO {

  //! module verbosity level
  enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};
  
  //! number of arms
  const int MAX_ARM = 2;
  
  //! number of stations
  const int MAX_STATION = 3;

  #ifndef __CINT__
  //! convert rads into degrees
  const double RAD_TO_DEG = 180.0/M_PI;
  
  //! convert degrees into radians
  const double DEG_TO_RAD = M_PI/180.0;
  #endif
  
  //! number of arms
  enum { NumberOfArms = 2 };
  
  //! number of stations
  enum { NumberOfStations = 3 };
  
  //! arm enumeration
  enum ArmNumber {South,North};
 
  //! station enumeration
  enum StationNumber {Station1,Station2,Station3};
	
  //! print a message to cout stream
  inline void TRACE( const std::string& message)
  { std::cout << "TRACE: " << message << std::endl; }

  //! print a message and a value to cout stream
  inline void TRACE( const std::string& message, const float& val )
  { std::cout << "TRACE: " << message << "\t" << val << std::endl; }
  
  //! print a message (formated) to a stream
  void PRINT(std::ostream& os = std::cout, const std::string& message = "");
  
  //! square a number of a given type
  template < typename T >
  T SQUARE(const T& x)
  { return x*x; } 

  //! return sign of a number: 0; +1 or -1
  template < typename T >
  int SIGN( const T& value ) 
  {
    if( !value ) return 0;
    return (value > 0) ? 1:-1;
  }

	//! print a message to cout stream
	inline void TRACE( const std::string& message, const Verbosity verb)
	  { if(verb<=SOME) { std::cout << "TRACE v: " << message << std::endl; } }

	//! print a message and a value to cout stream
	inline void TRACE( const std::string& message, const float& val , const Verbosity verb)
	  { if(verb<=SOME) { std::cout << "TRACE v: " << message << "\t" << val << std::endl; } }

};
#endif
