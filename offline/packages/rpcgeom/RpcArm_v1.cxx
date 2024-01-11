// $Id: RpcArm_v1.cxx,v 1.1 2010/06/27 13:25:52 richi Exp $

/*!
	\file RpcArm_v1.cxx
	\brief RPC Arm geometry
	Initialize and provide access to RPC stations
	\author Richard Hollis (UCR - rhollis@ucr.edu)
	\version $Revision: 1.1 $
	\date    $Date: 2010/06/27 13:25:52 $
*/

#include "RpcArm_v1.h"
//INCLUDECHECKER: Removed this line: #include "RPCGEOM.h"

#include <sstream>
#include <stdexcept>

using namespace std;

//_____________________________________________________
RpcStation* RpcArm_v1::get_station( unsigned int station )
{
	
  switch( station )
    {
    case RPCFULLGEOM::Station1: 
      return (RpcStation_v1 *) station1();
      break;
      
    case RPCFULLGEOM::Station2:
      return (RpcStation_v1 *) station2();
      break;
      
    case RPCFULLGEOM::Station3:
      return (RpcStation_v1 *) station3();
      break;
      
    default:
      {				
	ostringstream what;
	what << "RpcArm_v1::get_station - invalid index: " << station;
	throw runtime_error( what.str() );
      }
      break;
    }
  
  return 0;
  
}
