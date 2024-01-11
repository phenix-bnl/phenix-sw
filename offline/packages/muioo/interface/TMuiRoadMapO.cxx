#include<TMuiRoadMapO.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMuiRoadMapO::TMuiRoadMapO() : _count(0){;}


TMuiRoadMapO::TMuiRoadMapO(PHKey::map_key_type map_key) : PHMap<PHKey::key_type, TMuiRoadO, TMuiRoadO_v2>(map_key), _count(0){;}


TMuiRoadMapO::iterator TMuiRoadMapO::insert_new(UShort_t arm)
{  
  // Get the new index from the roll count;
  //
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
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

TMuiRoadMapO::iterator TMuiRoadMapO::insert_clone(const TMuiRoadMapO::pointer road_ptr)
{
  // Full key is object key from to be cloned object and
  // map key from this map.
  //
  Key full_key(get_map_key(),road_ptr->get()->get_key().get_obj_key());
  
  // Construct cloned from input hit
  //
  insert(full_key, new value_imp_type(road_ptr->get()));
  
  // Cloned object still has the wrong map key so we
  // we fix that.
  //
  TMuiRoadMapO::iterator road_iter = find(full_key);
  road_iter->get()->set_key(full_key);
  
  // Return the cloned object
  //
  return road_iter;
}


TMuiRoadMapO::iterator 
TMuiRoadMapO::get(UShort_t arm)		
{
  // key range associated with this plane
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMuiRoadMapO::const_iterator 
TMuiRoadMapO::get(UShort_t arm) const		
{
  // key range associated with this plane
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
