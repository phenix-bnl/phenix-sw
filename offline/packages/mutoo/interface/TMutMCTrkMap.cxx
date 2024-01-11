#include<TMutMCTrkMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMutMCTrkMap::TMutMCTrkMap() : _count(0){;}

TMutMCTrkMap::TMutMCTrkMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TMutMCTrk, TMutMCTrk_v5 >(map_key),
  _count(0){;}

TMutMCTrkMap::iterator 
TMutMCTrkMap::insert_new(UShort_t arm)
{
  // count the number of clusters to determine the new index
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  UShort_t index = find(lower,upper).count();
  
  // get the key for the new cluster
  //
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm,   
						 index);
  // full key
  //
  Key full_key(get_map_key(),key);

  // insert cluster
  //
  insert(full_key, new TMutMCTrk_v5(full_key, arm, index));
  
  // okay not so efficient 
  //
  return find(full_key);
}


TMutMCTrkMap::iterator 
TMutMCTrkMap::get(UShort_t arm)
{
  // key range associated with this plane
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);
							  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMutMCTrkMap::const_iterator 
TMutMCTrkMap::get(UShort_t arm) const 
{
  // key range associated with this gap
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}













