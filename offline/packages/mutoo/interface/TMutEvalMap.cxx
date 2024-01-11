#include<TMutEvalMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMutEvalMap::TMutEvalMap() :
  _count(0){;}


TMutEvalMap::TMutEvalMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type,TMutEval,TMutEval_v1>(map_key),
  _count(0){;}

TMutEvalMap::iterator TMutEvalMap::insert_new(UShort_t arm,
					      UShort_t octant)
{

  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  //
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm,
						 octant,
						 index);
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert cluster
  //
  insert(full_key, new TMutEval_v1(full_key,
				arm,
				octant,
				index));
  
  // okay not so efficient 
  //
  return find(full_key);
}


TMutEvalMap::iterator 
TMutEvalMap::get(UShort_t arm,
		UShort_t octant)
{
  // key range associated with this plane
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,octant);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMutEvalMap::const_iterator 
TMutEvalMap::get(UShort_t arm,
		UShort_t octant) const
{
  // key range associated with this gap
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,octant);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMutEvalMap::iterator 
TMutEvalMap::get(UShort_t arm)		
{
  // key range associated with this plane
  //
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,0,0);
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1,0,0);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}


TMutEvalMap::const_iterator 
TMutEvalMap::get(UShort_t arm) const		
{
  // key range associated with this plane
  //
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,0,0);
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1,0,0);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}






