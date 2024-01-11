// $Id: TRpcTrk.cxx,v 1.3 2008/08/28 00:50:24 kempel Exp $

/*!
	\file TRpcTrk.cxx
	\brief The RPC Track object 
	\author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2008/08/28 00:50:24 $
*/

#include "TRpcTrk.h"
#include "TRpcCoordMap.h"
#include <PHConstKeyIterator.h>

#include <RpcGeom.h>
#include <float.h>

ClassImp(TRpcTrk)

using namespace std;

//_________________________________________________________________________
const TMutTrkPar* TRpcTrk::get_trk_par_station( UShort_t station ) const
{
  
  // check there are some track parameters in the list
  const trk_par_list* local_list( get_trk_par_list() );
  if( !local_list ) return get_trk_par();
	
	// get station z for this track
	double z_st( RpcGeom::get_arm( get_arm() )->get_station( station )->get_z() );

  // loop ovet the track_par_list and search closest track parameters
  double min_delta_z = DBL_MAX;
  const TMutTrkPar* trk_par_ptr=0;

  // Loop over trk par list and TMutTrkPar with z reference closest to station z
  for( trk_par_list::const_iterator trk_iter = local_list->begin();trk_iter != local_list->end();++trk_iter){
    double delta_z = std::fabs(z_st-trk_iter->get_z());    
    if(delta_z < min_delta_z) {
      min_delta_z = delta_z;
      trk_par_ptr = &(*trk_iter);
    }
  }
  
  // If TMutTrkPar* is still null then return the reference track pars
  return (trk_par_ptr) ? trk_par_ptr : get_trk_par();
}

//___________________________________________________
size_t TRpcTrk::get_n_coord() const
{ return get_associated<TRpcCoord>().count(); }

//___________________________________________________
size_t TRpcTrk::get_ndf() const
{ return 2*get_n_coord() - 5; }

//_________________________________________
UShort_t TRpcTrk::get_hit_pattern() const
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

	
