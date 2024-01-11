/*!
	\file TRpcRoadMap.cxx
	\brief Container for TRpcRoad objects
	\author R. S. Hollis (rhollis@ucr.edu)
  \version $Revision: 1.1 $
  \date    today
*/

#include<TRpcRoadMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

TRpcRoadMap::TRpcRoadMap() : _count(0){;}

TRpcRoadMap::TRpcRoadMap(PHKey::map_key_type map_key) : PHMap<PHKey::key_type, TRpcRoad, TRpcRoad_v1>(map_key), _count(0){;}


//_____________________________________________________
TRpcRoadMap::iterator TRpcRoadMap::insert_new(UShort_t arm )
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
TRpcRoadMap::iterator TRpcRoadMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcRoadMap::const_iterator TRpcRoadMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
