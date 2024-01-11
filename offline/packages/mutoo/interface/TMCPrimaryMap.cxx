#include<TMCPrimaryMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

TMCPrimaryMap::TMCPrimaryMap() : _count(0){;}

TMCPrimaryMap::TMCPrimaryMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TMCPrimary, TMCPrimary_v3 >(map_key),
  _count(0){;}

TMCPrimaryMap::iterator 
TMCPrimaryMap::insert_new()
{
  UShort_t index = get_roll_count();
  
  // full key
  //
  Key full_key(get_map_key(),index);

  // insert cluster
  //
  insert(full_key, new TMCPrimary_v3(full_key, index));
  
  // okay not so efficient 
  //
  return find(full_key);
}
















