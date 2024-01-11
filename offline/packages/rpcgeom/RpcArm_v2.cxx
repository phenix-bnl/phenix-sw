// $Id: RpcArm_v2.cxx,v 1.1 2011/11/30 17:22:31 richi Exp $

/*!
	\file RpcArm_v2.cxx
	\brief RPC Arm geometry
	Initialize and provide access to RPC stations
	\author Richard Hollis (UCR - rhollis@ucr.edu)
	\version $Revision: 1.1 $
	\date    $Date: 2011/11/30 17:22:31 $
*/

#include "RpcArm_v2.h"
//INCLUDECHECKER: Removed this line: #include "RPCGEOM.h"

#include <sstream>
#include <stdexcept>

using namespace std;

//_____________________________________________________
RpcStation* RpcArm_v2::get_station( unsigned int station )
{
	
  switch( station )
    {
    case RPCFINALGEOM::Station1: 
      return (RpcStation_v2 *) station1();
      break;
      
    case RPCFINALGEOM::Station2:
      return (RpcStation_v2 *) station2();
      break;
      
    case RPCFINALGEOM::Station3:
      return (RpcStation_v2 *) station3();
      break;
      
    default:
      {				
	ostringstream what;
	what << "RpcArm_v2::get_station - invalid index: " << station;
	throw runtime_error( what.str() );
      }
      break;
    }
  
  return 0;
  
}
