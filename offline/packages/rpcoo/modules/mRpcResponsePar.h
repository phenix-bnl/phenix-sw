// $Id: mRpcResponsePar.h,v 1.2 2008/08/28 00:53:56 kempel Exp $
#ifndef __mRpcResponsePar_h__
#define __mRpcResponsePar_h__

/*!
  \file    mRpcResponsePar.h
  \brief   Runtime parameter object for mRpcResponse analysis module
  \author  H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:53:56 $
*/

#include <PHObject.h>
#include <PHException.h>
#include <iostream>
#include <boost/array.hpp>

#include "TRpcParBase.h"
#include "RPCOO.h"

/*! @ingroup modules */
//! Runtime parameter object for mRpcResponse analysis module
class mRpcResponsePar : public TRpcParBase
{
  
	public: 

  /*! default constructor */
  mRpcResponsePar() 
  { _plane_efficiency.assign(1); } 
  
  /*! destructor */
  ~mRpcResponsePar()
	{}
  
  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const 
  { os << "mRpcResponsePar";}

	//! plane efficiency
	double get_plane_efficiency( int arm, int station ) const
	{ return _plane_efficiency[ get_index( arm, station ) ]; }
	
	//! plane efficiency
	void set_plane_efficiency( int arm, int station, double value )
	{ _plane_efficiency[ get_index( arm, station ) ] = value; }
	
	//! printing (insert values of all parameters here
	void print( std::ostream &out = std::cout ) const
	{ 
		RPCOO::PRINT( out, "mRpcResponsePar" );
		for( int arm=0; arm < RPCOO::NumberOfArms; arm++ )
		for( int station=0; station < RPCOO::NumberOfStations; station++ )
		out 
						<< "_plane_efficiency[" << arm << "," << station << "] : "
						<< get_plane_efficiency( arm, station )
						<< std::endl;
		RPCOO::PRINT( out, "**" );
	}
	
	private:  

	//! get unique index from arm/station
	static int get_index( int arm, int station )
	{ return station + RPCOO::NumberOfStations*arm; }
			
	//! plane efficiency
	boost::array< double, RPCOO::NumberOfArms*RPCOO::NumberOfStations > _plane_efficiency;
		 
};

#endif










