#include<TMuiPseudoBLTMapO.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMuiPseudoBLTMapO::TMuiPseudoBLTMapO() : _count(0){;}


TMuiPseudoBLTMapO::TMuiPseudoBLTMapO(PHKey::map_key_type map_key) : PHMap<PHKey::key_type, TMuiPseudoBLTO, TMuiPseudoBLTO_v1>(map_key), _count(0){;}


TMuiPseudoBLTMapO::iterator TMuiPseudoBLTMapO::insert_new(UShort_t arm)
{  
  // Get the new index from the roll count;
  //
  UShort_t index = get_roll_count();
  
  // get the key for the new TMuiPseudoBLT object
  //
  TMuiKeyGen::key_type key = TMuiKeyGen::get_key(arm,
						 index);
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert cluster
  //
  insert(full_key, new value_imp_type(full_key,
				      arm,
				      index));
  
  // okay not so efficient 
  //
  return find(full_key);
}


TMuiPseudoBLTMapO::iterator 
TMuiPseudoBLTMapO::get(UShort_t arm)		
{
  // key range associated with this arm
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMuiPseudoBLTMapO::const_iterator 
TMuiPseudoBLTMapO::get(UShort_t arm) const		
{
  // key range associated with this arm
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
