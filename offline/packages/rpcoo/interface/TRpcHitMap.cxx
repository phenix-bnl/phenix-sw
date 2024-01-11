// $Id: TRpcHitMap.cxx,v 1.3 2009/09/24 07:53:35 hpereira Exp $

/*!
	\file TRpcHitMap.cxx
	\brief Container for TRpcHit objects
	\author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2009/09/24 07:53:35 $
*/

#include<TRpcHitMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_____________________________________________________
TRpcHitMap::iterator TRpcHitMap::insert_new(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t strip )
{
  
  // get the key for the new cluster
  TRpcKeyGen::key_type key = TRpcKeyGen::get_key(arm, station, octant, halfoctant, rseg, strip);						 
  
	// full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert( full_key, new value_imp_type(full_key, arm, station, octant, halfoctant, rseg, strip ) );
  
  // okay not so efficient 
  return find(full_key);
}

//_____________________________________________________
TRpcHitMap::iterator TRpcHitMap::insert_clone(const TRpcHitMap::pointer hit_ptr)
{
  // Full key is object key from to be cloned object and map key from this map.
  Key full_key( get_map_key(), hit_ptr->get()->get_key().get_obj_key() );
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(hit_ptr->get()));
  
  // Cloned object still has the wrong map key so we  we fix that.
  TRpcHitMap::iterator hit_iter = find(full_key);
  hit_iter->get()->set_key(full_key);
  
  // Return the cloned object
  return hit_iter;
}

//_____________________________________________________
/*! 
Get an iterator to a specific hit strip. If the strip is not
in the map the iterator's current value will be equal to end() 
and at_end() will return true.	It is *never* safe to attempt
to dereference the iterator returned by this method without 
checking.
*/
TRpcHitMap::iterator TRpcHitMap::get(UShort_t arm, 
						 UShort_t station, 
						 UShort_t octant, 
						 UShort_t half_octant, 
						 UShort_t rseg,
						 UShort_t strip)
{

  TRpcKeyGen::key_type key = TRpcKeyGen::get_key(arm, 
    station, 
    octant, 
    half_octant, 
    rseg, 
    strip);	
  Key full_key(get_map_key(),key);
  
  return find(full_key);

}

//_____________________________________________________
TRpcHitMap::iterator TRpcHitMap::get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg )
{
  // key range associated with this strip
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station, octant, halfoctant, rseg );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHitMap::const_iterator TRpcHitMap::get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg ) const  
{
  // key range associated with this strip
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station, octant, halfoctant, rseg );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHitMap::iterator TRpcHitMap::get(UShort_t arm, UShort_t station )
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHitMap::const_iterator TRpcHitMap::get(UShort_t arm, UShort_t station ) const  
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHitMap::iterator TRpcHitMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHitMap::const_iterator TRpcHitMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
