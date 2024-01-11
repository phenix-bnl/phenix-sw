// $Id: RPCGEOM.h,v 1.7 2014/11/05 23:53:00 pinkenbu Exp $

#ifndef _RPCGEOM_h_
#define _RPCGEOM_h_

/*!
	\file RPCGEOM.h
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
	\version $Revision: 1.7 $
	\date		$Date: 2014/11/05 23:53:00 $
*/

#include<TObject.h>

#include<iostream>
#include<string>

//! widely used utility functions and enumerations
class RPCGEOM : public TObject
{
 public:
	
	//! module verbosity level
	enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};
 
	//! out of range number
	static const int OUTOFRANGE;

	//! convert rads into degrees
	static const Float_t RAD_TO_DEG;
	
	//! convert degrees into radians
	static const Float_t DEG_TO_RAD;
	
	//! number of arms
	static const int NumberOfArms = 2;
	
	//! number of stations
	static const int NumberOfStations = 3;
	
	//!number of R bounds RPC 1
	static const int NumberofRBoundsSt1 = 5;

	//!number of R bounds RPC 2
        static const int NumberofRBoundsSt2 = 5;

	//!number of R bounds RPC 3
	static const int NumberofRBoundsSt3 = 4;

	//! arm enumeration
	enum ArmNumber {South,North};
 
	//! station enumeration
	enum StationNumber {Station1,Station2,Station3};

	//actual geometry

	//z-position of stations
	static const double station_z[NumberOfArms][NumberOfStations];
	
	//R Segmentation (in RPC local y coord)--North and South are taken to be symmetric
	static const double R_bound[NumberOfStations][NumberofRBoundsSt1];
  
	//strip widths
	//	static const double strip_size[NumberOfStations][NumberofRBoundsSt1-1];
	static const double strip_size[NumberOfStations][5-1];

	//! print a message to cout stream
	static void TRACE( const std::string& message)
	{ std::cout << "TRACE: " << message << std::endl; }	
 
	//! print a message and a value to cout stream
	static void TRACE( const std::string& message, const float& val )
	{ std::cout << "TRACE: " << message << "\t" << val << std::endl; }

	//! print a message (formated) to a stream
	static void PRINT(std::ostream& os = std::cout, const std::string& message = "");
	
#ifndef __CINT__
	
	//! square a number of a given type
	template < typename T >
	static T SQUARE(const T& x)
	{ return x*x; } 

	//! return sign of a number: 0; +1 or -1
	template < typename T >
	static int SIGN( const T& value ) 
	{
		if( !value ) return 0;
		return (value > 0) ? 1:-1;
	}

#endif
	
	ClassDef( RPCGEOM,1 );
	
};
#endif
