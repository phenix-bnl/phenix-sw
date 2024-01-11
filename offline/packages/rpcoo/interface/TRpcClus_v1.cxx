// $Id: TRpcClus_v1.cxx,v 1.2 2008/08/28 00:50:18 kempel Exp $

/*!
   \file    TRpcClus_v1.cxx
   \brief   Interface Object Class : TRpcClus
   \author  H.Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2008/08/28 00:50:18 $
*/

#include "RPCOO.h"
#include "TRpcClus_v1.h"
#include "TRpcHitMap.h"

ClassImp(TRpcClus_v1)

using namespace std;

//____________________________________________
TRpcClus_v1::TRpcClus_v1( void ):
		_arm(0),
		_station(0),
		_octant(0),
		_halfoctant(0),
		_rseg(0),
		_index(0),
		_chi_square(0),
		_status(0)
{}

//____________________________________________
TRpcClus_v1::TRpcClus_v1( const Key& key, UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t index ):
		TRpcClus( key ),
		_arm( arm ),
		_station( station ),
		_octant(octant),
		_halfoctant(halfoctant),
		_rseg(rseg),
		_index( index ),
		_chi_square( 0 ),
		_status( 0 )
{}
	
//____________________________________________
TRpcClus_v1::TRpcClus_v1( const TRpcClus& ref ):
		TRpcClus( ref ),
		_arm( ref.get_arm() ),
		_station( ref.get_station() ),
		_octant( ref.get_octant() ),
		_halfoctant( ref.get_half_octant() ),
		_rseg( ref.get_rseg() ),
		_index( ref.get_index() ),
		_chi_square( ref.get_chi_square() ),
		_status( ref.get_status() )
{

	// copy centroid list
	centroid_list centroids( ref.get_centroid_list() );
	for( centroid_iterator iter = centroids.begin(); iter!=centroids.end(); iter++ )
	if( *iter ) _centroid_list.push_back( new centroid_value_type( **iter ) );
	
}
	
//____________________________________________
TRpcClus_v1::TRpcClus_v1( const TRpcClus* ptr ):
		TRpcClus( *ptr ),
		_arm( ptr->get_arm() ),
		_station( ptr->get_station() ),
		_octant( ptr->get_octant() ),
		_halfoctant( ptr->get_half_octant() ),
		_rseg( ptr->get_rseg() ),
		_index( ptr->get_index() ),
		_chi_square( ptr->get_chi_square() ),
		_status( ptr->get_status() )
{

	// copy centroid list
	centroid_list centroids( ptr->get_centroid_list() );
	for( centroid_iterator iter = centroids.begin(); iter!=centroids.end(); iter++ )
	if( *iter ) _centroid_list.push_back( new centroid_value_type( **iter ) );
	
}
	
//____________________________________________
void TRpcClus_v1::print( ostream& os ) const
{
	
  RPCOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _halfoctant
     << "  rseg: " << _rseg
     << "  index: " << _index << endl;

  // dump the cluster width
  os << " cluster width: " << get_n_hits() << endl; 
	
  RPCOO::PRINT(os, "**" );

  // dump associated hits
  os << " hits associated with this cluster: "; 
  TRpcHitMap::const_key_iterator hit_iter = get_associated<TRpcHit>();
  while(TRpcHitMap::const_pointer hit_ptr = hit_iter.next())
  os << hit_ptr->get()->get_strip() << ":" << hit_ptr->get()->get_q() << ":" << hit_ptr->get()->get_q_error() << " ";
  os << endl;
	
  // dump centroid fit data
  for( 
    centroid_list::const_iterator iter = _centroid_list.begin();
    iter!=_centroid_list.end();
    ++iter) 
  if( *iter ) (*iter)->print(os);

}
