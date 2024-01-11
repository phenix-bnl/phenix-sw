/*!
	\file TRpcRoad.cxx
	\brief The RPC Track object 
	\author R. S. Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2010/09/09 20:36:20 $
*/

#include "TRpcRoad.h"
#include "TRpcCoordMap.h"
#include <PHConstKeyIterator.h>

#include <TMuiRoadO.h>
#include <TMuiRoadO_v1.h>
#include <TMuiRoadO_v2.h>

#include <RpcGeom.h>
#include <float.h>

ClassImp(TRpcRoad)

using namespace std;

//__________________________________________________  
TRpcRoad::TRpcRoad()
{
}

//__________________________________________________  
TRpcRoad::TRpcRoad(const Key& key, UShort_t arm, UShort_t index)
{
  
}

//__________________________________________________  
void TRpcRoad::print( std::ostream &os ) const
{
  //MUIOO::PRINT(os,GetName());
}
