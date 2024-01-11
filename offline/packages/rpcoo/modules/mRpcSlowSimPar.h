// $Id: mRpcSlowSimPar.h,v 1.4 2014/01/26 18:01:24 bbannier Exp $
#ifndef __mRpcSlowSimPar_h__
#define __mRpcSlowSimPar_h__

/*!
	\file		mRpcSlowSimPar.h
	\brief	 Runtime parameter object for mRpcSlowSim analysis module
	\author	H. Pereira Da Costa
	\version $Revision: 1.4 $
	\date		$Date: 2014/01/26 18:01:24 $
*/

#include <PHObject.h>
#include <PHException.h>
#include <boost/array.hpp>
#include <iostream>

#include "TRpcParBase.h"
#include "RPCOO.h"

/*! @ingroup modules */
//! Runtime parameter object for mRpcSlowSim analysis module
class mRpcSlowSimPar : public TRpcParBase
{
	
 public: 

	/*! default constructor */
	mRpcSlowSimPar() 
	{} 
	
	/*! destructor */
	~mRpcSlowSimPar()
	{}
	
	//! PHOOL inteface requirement
	void identify(std::ostream& os = std::cout) const 
	{ os << "mRpcSlowSimPar";}
	
	//! number of parameters
	enum { n_acceptance_parameters = RPCOO::NumberOfArms*RPCOO::NumberOfStations };

	//! \brief return unique index for arm/station
	static int get_acceptance_index( int arm, int station )
	{ return station + RPCOO::NumberOfStations*arm; }

	//! printing (insert values of all parameters here
	void print(std::ostream &out = std::cout) const
	{ 
		RPCOO::PRINT( out, "mRpcSlowSimPar" );
		RPCOO::PRINT( out, "**" );
	}
};

#endif
