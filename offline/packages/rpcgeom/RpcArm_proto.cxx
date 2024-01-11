// $Id: RpcArm_proto.cxx,v 1.1 2009/04/03 18:40:39 richi Exp $

/*!
	\file RpcArm_proto.cxx
	\brief RPC Arm geometry
	Initialize and provide access to RPC stations
	\author Richard Hollis (UCR - rhollis@ucr.edu)
	\version $Revision: 1.1 $
	\date    $Date: 2009/04/03 18:40:39 $
*/

#include "RpcArm_proto.h"
//INCLUDECHECKER: Removed this line: #include "RPCGEOM.h"

#include <sstream>
#include <stdexcept>

using namespace std;

//_____________________________________________________
RpcStation* RpcArm_proto::get_station( unsigned int station )
{
	
  switch( station )
    {
    case RPCPROTOGEOM::Station1: 
      return (RpcStation_proto *) station1();
      break;
      
    case RPCPROTOGEOM::Station2:
      return (RpcStation_proto *) station2();
      break;
      
    case RPCPROTOGEOM::Station3:
      return (RpcStation_proto *) station3();
      break;
      
    default:
      {				
	ostringstream what;
	what << "RpcArm_proto::get_station - invalid index: " << station;
	throw runtime_error( what.str() );
      }
      break;
    }
  
  return 0;
  
}
