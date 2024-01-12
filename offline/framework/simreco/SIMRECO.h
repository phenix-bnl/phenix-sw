// $Id: SIMRECO.h,v 1.1 2008/10/18 13:22:17 hpereira Exp $
#ifndef __SIMRECO_H__
#define __SIMRECO_H__

/*!
	\file SIMRECO.h
	\brief widely used utility functions and enumerations
	\author H. Pereira Da Costa
	\version $Revision: 1.1 $
	\date $Date: 2008/10/18 13:22:17 $
*/

#include<string>
#include<iostream>
#include<sstream>
#include<TDataType.h>

// Hide anything remotely complicated from CINT
#ifndef __CINT__
#include <gsl/gsl_math.h>
#include<boost/tuple/tuple.hpp>
#endif

//! widely used utility functions and enumerations
namespace SIMRECO {

	// encapsulate constants used in class invariant tests and
	// the trace functions. 

	//! module verbosity level
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
	
}

#endif


