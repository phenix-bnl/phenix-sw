// $Id: RpcArm.cxx,v 1.4 2008/08/28 00:48:26 kempel Exp $

/*!
	\file RpcArm.cxx
	\brief RPC Arm geometry
	Initialize and provide access to RPC stations
	\author Hugo Pereira da costa
	\version $Revision: 1.4 $
	\date    $Date: 2008/08/28 00:48:26 $
*/

#include "RpcArm.h"
//INCLUDECHECKER: Removed this line: #include "RPCGEOM.h"

#include <sstream>
#include <stdexcept>

using namespace std;

//_____________________________________________________
RpcStation* RpcArm::get_station( unsigned int station )
{
	
	switch( station )
	{
		case RPCGEOM::Station1: 
			return station1();
			break;
			
		case RPCGEOM::Station2:
			return station2();
			break;
			
		case RPCGEOM::Station3:
			return station3();
			break;
		
		default:
		{				
			ostringstream what;
			what << "RpcArm::get_station - invalid index: " << station;
			throw runtime_error( what.str() );
		}
		break;
	}
	
	return 0;
			
}
