/*!
	\file TRpcMuoTrkMap.cxx
	\brief Container for TRpcMuoTrk objects
	\author Richard Hollis (UCR)
  \version $Revision: 1.1 $
  \date    $Date: 2012/04/03 18:47:21 $
*/

#include<TRpcMuoTrkMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_____________________________________________________
TRpcMuoTrkMap::iterator TRpcMuoTrkMap::insert_new(UShort_t arm )
{
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  TRpcKeyGen::key_type key = TRpcKeyGen::get_key(arm, index);						 
  
	// full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert( full_key, new value_imp_type(full_key, arm, index ) );
  
  // okay not so efficient 
  return find(full_key);
}

//_____________________________________________________
TRpcMuoTrkMap::iterator TRpcMuoTrkMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcMuoTrkMap::const_iterator TRpcMuoTrkMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
