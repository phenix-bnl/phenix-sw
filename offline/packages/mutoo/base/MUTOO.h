// $Id: MUTOO.h,v 1.8 2019/04/16 02:04:08 slash Exp $
#ifndef __MUTOO_H__
#define __MUTOO_H__

/*!
	\file MUTOO.h
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
	\version $Revision: 1.8 $
	\date $Date: 2019/04/16 02:04:08 $
*/

#include <string>
#include <iostream>
#include <sstream>
#include <TDataType.h>
#include <TMath.h>

// Hide anything remotely complicated from CINT
#ifndef __CINT__
#include <gsl/gsl_math.h>
#include <boost/tuple/tuple.hpp>
#endif

//! widely used utility functions and enumerations
namespace MUTOO {

	// encapsulate constants used in class invariant tests and
	// the trace functions. 

	//! module verbosity level
	enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};
	
	//! number of arms
	const int MAX_ARM = 2;
	
	//! number of stations
	const int MAX_STATION = 3;
	
	//! number of octants
	const int MAX_OCTANT = 8;
	
	//! number of half octants per octant
	const int MAX_HALF_OCTANT = 2;
	
	//! min number of gap/station (there are 3 gaps in station1 and 2; 2 gaps in station 3)
	const int MAX_GAP = 3;
	
	//! number of cathodes par gap
	const int MAX_CATHODE = 2;
	
	//! max number of strips/half octant
	const int MAX_STRIP = 200;
	
	//! ???
	const int NCATHODE_PLANE = 386;

	//! particule pid for (positive) positrons in pisa
	const int PISA_PID_EPOS = 2;
	
	//! particule pid for (negative) electrons in pisa
	const int PISA_PID_ENEG = 3;

	//! particule pid for positive muons in pisa
	const int PISA_PID_MUPOS = 5;
	
	//! particule pid for negative muons in pisa
	const int PISA_PID_MUNEG = 6;

	//! particule pid for positive pions in pisa
	const int PISA_PID_PIPOS = 8;
	
	//! particule pid for negative pions in pisa
	const int PISA_PID_PINEG = 9;

	//! particule pid for positive kaons in pisa
	const int PISA_PID_KPOS = 11;
	
	//! particule pid for negative kaons in pisa
	const int PISA_PID_KNEG = 12;

	//! particule pid for positive protons in pisa
	const int PISA_PID_PPOS = 1;
	
	//! particule pid for negative protons in pisa
	const int PISA_PID_PNEG = -1;
	
	//! muon mass
	const double MASS_MUON	= 0.105658388214;
	
	//! muon mass square
	const double MASS_MUON_SQUARE = 0.011163695;

	//! convert rads into degrees
	const double RAD_TO_DEG = TMath::RadToDeg();

	//! convert degrees into radians
	const double DEG_TO_RAD = TMath::DegToRad();

	// Nicki's enumerations brought inside MUTOO namespace
	
	//! number of arms
	const int NumberOfArms = MAX_ARM;
	
	//! arm enumeration
	enum ArmNumber {South,North};
	
	//! number of stations
	const int NumberOfStations = MAX_STATION;
	
	//! station enumeration
	enum StationNumber {Station1,Station2,Station3};
	
	//! number of octants
	const int NumberOfOctants = MAX_OCTANT;
	
	//! octant enumeration
	enum OctantNumber {Octant1,Octant2,Octant3,Octant4,Octant5,Octant6,Octant7,Octant8};
	
	//! number of half octants per octant
	const int NumberOfHalfOctants = MAX_HALF_OCTANT;
	
	//! half octant enumeration
	enum HalfOctantNumber {HalfOctant1,HalfOctant2};
	
	//! number of gaps (max)
	const int NumberOfGaps = MAX_GAP;
	
	//! gap enumeration
	enum GapNumber {Gap1,Gap2,Gap3};
	
	//! number of planes (cathode/anode/cathode)
	const int NumberOfPlanes = 3;
	
	//! plane enumeration
	enum PlaneNumber {Cathode1,Wire,Cathode2};
	
	//! number of cathode plane
	const int NumberOfCathodePlanes = MAX_CATHODE;
	
	//! cathode plane enumeration
	enum CathodePlaneNumber {Cathode_1,Cathode_2};
	
	//! print a message to cout stream
	inline void TRACE( const std::string& message)
	{ std::cout << "TRACE: " << message << std::endl; }

	//! print a message and a value to cout stream
	inline void TRACE( const std::string& message, const float& val )
	{ std::cout << "TRACE: " << message << "\t" << val << std::endl; }
	
	long int global_index(int arm, int station, int gap, 
			      int plane, int octant, int half_octant);

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
	
	// locator typedefs
	#ifndef __CINT__

	/*! Define a name for a boost tuple for (arm, station, octant, half_octant, gap, cathode, index) */
	typedef boost::tuple<
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short,
			const unsigned short> object_locator;

	/*! Define a name for a boost tuple for (arm, station, octant, half_octant, gap, cathode) */
	typedef boost::tuple<
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short> cathode_locator;

	/*! Define a name for a boost tuple for (arm, station, octant, half_octant, gap) */
	typedef boost::tuple<
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short> gap_locator;


	/*! Define a name for a boost tuple for (arm, station, octant, half_octant) */
	typedef boost::tuple<
			const unsigned short, 
			const unsigned short, 
			const unsigned short, 
			const unsigned short> half_octant_locator;

#endif

	//! print a message to cout stream
	inline void TRACE( const std::string& message, const Verbosity verb)
	  { if(verb<=SOME) { std::cout << "TRACE v: " << message << std::endl; } }

	//! print a message and a value to cout stream
	inline void TRACE( const std::string& message, const float& val , const Verbosity verb)
	  { if(verb<=SOME) { std::cout << "TRACE v: " << message << "\t" << val << std::endl; } }

}

#endif


