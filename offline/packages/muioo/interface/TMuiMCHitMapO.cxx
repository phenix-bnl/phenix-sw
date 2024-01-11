#include<TMuiMCHitMapO.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMuiMCHitMapO::TMuiMCHitMapO() : _count(0) {;}


TMuiMCHitMapO::TMuiMCHitMapO(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TMuiMCHitO, TMuiMCHitO_v1>(map_key), _count(0){;}


TMuiMCHitMapO::iterator TMuiMCHitMapO::insert_new(UShort_t arm,
						  UShort_t plane)
{
  
  UShort_t index = get_roll_count();
  
  // get the key for new object
  //
  TMuiKeyGen::key_type key = TMuiKeyGen::get_key(arm,
                                                 plane,
                                                 index);
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert 
  //
  insert(full_key, new value_imp_type(full_key,
                                      arm,
                                      plane,
				      index));
  
  // okay not so efficient 
  //
  return find(full_key);
}


TMuiMCHitMapO::iterator 
TMuiMCHitMapO::get(UShort_t arm)		
{
  // key range associated with this plane
  //
  TMuiKeyGen::key_type lower_key = TMuiKeyGen::get_key(arm,0);
  TMuiKeyGen::key_type upper_key = TMuiKeyGen::get_key(arm+1,0)-1;
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}


TMuiMCHitMapO::const_iterator 
TMuiMCHitMapO::get(UShort_t arm) const		
{
  // key range associated with this plane
  //
  TMuiKeyGen::key_type lower_key = TMuiKeyGen::get_key(arm,0);
  TMuiKeyGen::key_type upper_key = TMuiKeyGen::get_key(arm+1,0)-1;

  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMuiMCHitMapO::iterator
TMuiMCHitMapO::get(UShort_t arm,
		   UShort_t plane)
{
  // key range associated with this plane
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm,
							  plane);
  

  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);

}

TMuiMCHitMapO::const_iterator
TMuiMCHitMapO::get(UShort_t arm,
		   UShort_t plane) const
{
  // key range associated with this plane
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm,
							  plane);
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}









