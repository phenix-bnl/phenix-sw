// $Id: MUTGEOM.h,v 1.16 2008/07/30 10:46:42 hpereira Exp $

#ifndef MUTGEOM_h
#define MUTGEOM_h

/*!
  \file MUTGEOM.h
  \brief widely used utility functions and enumerations
  \author H. Pereira Da Costa
  \version $Revision: 1.16 $
  \date		$Date: 2008/07/30 10:46:42 $
*/

#ifndef __CINT__
#include <gsl/gsl_math.h>
#endif

#include <string>
#include <iostream>

//! widely used utility functions and enumerations
namespace MUTGEOM 
{

  #ifndef __CINT__
  
  //! rad to degree conversion
  const double RAD_TO_DEG = 180.0/M_PI;
  
  //! degree to rad conversion
  const double DEG_TO_RAD = M_PI/180.0;	
  #endif

  //! module verbosity level
  enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};

  //! number of arms
  enum { NumberOfArms = 2 };
  
  //! arm number
  enum ArmNumber {South,North};
  
  //! number of stations
  enum { NumberOfStations = 3 };
  
  //! station number
  enum StationNumber {Station1,Station2,Station3};
  
  //! number of octants
  enum { NumberOfOctants = 8 };
  
  //! octant number
  enum OctantNumber {Octant1,Octant2,Octant3,Octant4,Octant5,Octant6,Octant7,Octant8};
  
  //! number of half octants
  enum { NumberOfHalfOctants = 2 };
  
  //! half octant number
  enum HalfOctantNumber {HalfOctant1,HalfOctant2};
  
  //! number of gaps
  enum { NumberOfGaps = 3 };
  
  //! gap number
  enum GapNumber {Gap1,Gap2,Gap3};
  
  //! number of planes/gap
  enum { NumberOfPlanes = 3 };
  
  //! plane number
  enum PlaneNumber {Cathode1,Wire,Cathode2};
  
  //! number of cathode planes
  enum { NumberOfCathodePlanes = 2 };
  
  //! cathode planes
  enum CathodePlaneNumber {Cathode_1,Cathode_2};
  
  //! anode cards
  enum{ NumberOfAnodeCards = 10 };
  
  //! print a message to cout stream
  inline void TRACE( const std::string& message)
  { std::cout << "TRACE: " << message << std::endl; }

  //! print a message and a value to cout stream
  inline void TRACE( const std::string& message, const float& val )
  { std::cout << "TRACE: " << message << "\t" << val << std::endl; }
  
  //! print a message (formated) to a stream
  void PRINT(std::ostream& os = std::cout, const std::string& message = "");
    
  #ifndef __CINT__

  //! square a number of a given type
  template < typename T >
  T SQUARE(const T& x)
  { return x*x; } 
  
  #endif

};

#endif
