// $Id: TRpcClusMap.cxx,v 1.2 2008/08/28 00:50:17 kempel Exp $

/*!
	\file TRpcClusMap.cxx
	\brief Container for TRpcClus objects

	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:17 $
*/

#include<TRpcClusMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_____________________________________________________
TRpcClusMap::iterator TRpcClusMap::insert_new(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg )
{
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  TRpcKeyGen::key_type key = TRpcKeyGen::get_key(arm, station, octant, halfoctant, rseg, index);						 
  
	// full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert( full_key, new value_imp_type(full_key, arm, station, octant, halfoctant, rseg, index ) );
  
  // okay not so efficient 
  return find(full_key);
}

//_____________________________________________________
TRpcClusMap::iterator TRpcClusMap::get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg )
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station, octant, halfoctant, rseg);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcClusMap::const_iterator TRpcClusMap::get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg ) const  
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station, octant, halfoctant, rseg );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcClusMap::iterator TRpcClusMap::get(UShort_t arm, UShort_t station )
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcClusMap::const_iterator TRpcClusMap::get(UShort_t arm, UShort_t station ) const  
{
  // key range associated with this station
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm, station );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcClusMap::iterator TRpcClusMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRpcClusMap::const_iterator TRpcClusMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRpcKeyGen::key_range range = TRpcKeyGen::get_key_range(arm);        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
