#ifndef __MUIOO_H__
#define __MUIOO_H__

#include<string>
#include<iostream>
#include<TDataType.h>

// Hide anything remotely complicated from CINT
//
#ifndef __CINT__
#include<boost/tuple/tuple.hpp>
#include<gsl/gsl_math.h>
#endif

/*!
  encapsulate constants used in class invariant tests and
  the trace functions. 
  enumerations
*/
namespace MUIOO {


  //! module verbosity
  enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};

  // constants 
  const int MAX_ARM = 2;
  const int MAX_PLANE = 5;
  const int MAX_PANEL = 6;
  const int MAX_ORIENTATION = 2;
  const int MAX_TWOPACK_PANEL = 64;
  const short kROCsPerFEM=20;
  const short kWordsPerROC=6;
  const short kChannelsPerWord=16;
  const short kFEMsTotal=4;
  const short kROCsTotal =kFEMsTotal*kROCsPerFEM;
  const short kWordsTotal =kROCsTotal*kWordsPerROC;
  const short kChannelsTotal =kWordsTotal*kChannelsPerWord;
  const short kWordsPerFEM =kROCsPerFEM*kWordsPerROC;
  
  const int kLL1_NSymsetWords = 5;

#ifndef __CINT__
  // Constants not in gsl_constants
  //
  const double RAD_TO_DEG = 180.0/M_PI;
  const double DEG_TO_RAD = M_PI/180.0;
  const double MASS_MUON  = 0.105658388214;
  const double MASS_MUON_SQUARE = 0.011163695;

#endif

  // Nicki's enumerations brought inside MUIOO namespace also
  //
  const int NumberOfArms = 2;
  enum ArmNumber {South,North};

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
  
  // locator typedefs
  //
#ifndef __CINT__

  /*! Define a name for a boost tuple for (arm, plane, panel, orienation, index) */
  typedef boost::tuple<const UShort_t, 
                       const UShort_t, 
                       const UShort_t, 
                       const UShort_t, 
                       const UShort_t> object_locator;

  /*! Define a name for a boost tuple for (arm, plane, panel, orienation) */

  typedef boost::tuple<const UShort_t, 
                       const UShort_t, 
                       const UShort_t, 
                       const UShort_t> panel_orient_locator;

  /*! Define a name for a boost tuple for (arm, plane, panel) */

  typedef boost::tuple<const UShort_t, 
                       const UShort_t, 
                       const UShort_t> panel_locator;


  /*! Define a name for a boost tuple for (arm, plane) */

  typedef boost::tuple<const UShort_t, 
                       const UShort_t> plane_locator;

#endif

	//! print a message to cout stream
	inline void TRACE( const std::string& message, const Verbosity verb)
	  { if(verb<=SOME) { std::cout << "TRACE v: " << message << std::endl; } }

	//! print a message and a value to cout stream
	inline void TRACE( const std::string& message, const float& val , const Verbosity verb)
	  { if(verb<=SOME) { std::cout << "TRACE v: " << message << "\t" << val << std::endl; } }


};

#endif


