// $Id: TMutVtxMap.cxx,v 1.6 2011/12/29 20:19:31 slash Exp $
#include<TMutVtxMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMutVtxMap::TMutVtxMap() : _count(0){;}

//_________________________________________________________
TMutVtxMap::TMutVtxMap(PHKey::map_key_type map_key) :
  PHMap<PHKey::key_type, TMutVtx, TMutVtx_v2 >(map_key),
  _count(0){;}

//_________________________________________________________
TMutVtxMap::iterator
TMutVtxMap::insert_new(UShort_t arm)
{

  UShort_t index = get_roll_count();

  // get the key for the new cluster
  //
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm,
						 index);
  // full key
  //
  Key full_key(get_map_key(),key);

  // insert cluster
  //
  insert(full_key, new TMutVtx_v2(full_key, arm, index));

  // okay not so efficient
  //
  return find(full_key);
}


//_________________________________________________________
TMutVtxMap::iterator
TMutVtxMap::get(UShort_t arm)
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


//_________________________________________________________
TMutVtxMap::const_iterator
TMutVtxMap::get(UShort_t arm) const
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
