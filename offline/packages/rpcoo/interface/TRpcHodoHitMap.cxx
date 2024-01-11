
/*!
	\file TRpcHodoHitMap.cxx
	\brief Container for TRpcHodoHit objects
	\author R.S.Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/02/13 02:53:07 $
*/

#include<TRpcHodoHitMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_____________________________________________________
TRpcHodoHitMap::iterator TRpcHodoHitMap::insert_new(UShort_t arm, UShort_t station, UShort_t strip )
{
  
  // get the key for the new cluster
  TRpcKeyGen::key_type key = TRpcKeyGen::get_key(arm, station, 0, 0, 0, strip);						 
  
  // full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert( full_key, new value_imp_type(full_key, arm, station, strip ) );
  
  // okay not so efficient 
  return find(full_key);
}

//_____________________________________________________
TRpcHodoHitMap::iterator TRpcHodoHitMap::insert_clone(const TRpcHodoHitMap::pointer hit_ptr)
{
  // Full key is object key from to be cloned object and map key from this map.
  Key full_key( get_map_key(), hit_ptr->get()->get_key().get_obj_key() );
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(hit_ptr->get()));
  
  // Cloned object still has the wrong map key so we  we fix that.
  TRpcHodoHitMap::iterator hit_iter = find(full_key);
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
TRpcHodoHitMap::iterator TRpcHodoHitMap::get(UShort_t arm, 
					     UShort_t station, 
					     UShort_t strip)
{

  //We lazily use the key generated from RPC, but assume that the octant, halfoct, and radseg are 0
  TRpcKeyGen::key_type key = TRpcKeyGen::get_key(arm, 
    station, 
    0, 
    0, 
    0, 
    strip);	
  Key full_key(get_map_key(),key);
  
  return find(full_key);

}

//_____________________________________________________
TRpcHodoHitMap::iterator TRpcHodoHitMap::get(UShort_t arm, UShort_t station )
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHodoHitMap::const_iterator TRpcHodoHitMap::get(UShort_t arm, UShort_t station ) const  
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHodoHitMap::iterator TRpcHodoHitMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcHodoHitMap::const_iterator TRpcHodoHitMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
