/*!
	\file TRpcMuoTrk.cxx
	\brief The RPC Track object 
	\author Richard Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/04/03 18:47:20 $
*/

#include "TRpcMuoTrk.h"
#include "TRpcCoordMap.h"
#include <PHConstKeyIterator.h>

#include <RpcGeom.h>
#include <float.h>

ClassImp(TRpcMuoTrk)

using namespace std;

//_________________________________________
/*UShort_t TRpcMuoTrk::get_hit_pattern() const
{

  UShort_t out( 0 );
  TRpcCoordMap::const_key_iterator coord_iter = get_associated<TRpcCoord>();
  while( TRpcCoordMap::const_pointer coord_ptr = coord_iter.next() )
  {
    
    // calculate plane index from the coordinate
    UShort_t index = coord_ptr->get()->get_station();
    
    // update the pattern
    out |= ( 1 << index );
  }
  
  return out;

}
*/
	
