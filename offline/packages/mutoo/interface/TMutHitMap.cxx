#include<TMutHitMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


//_____________________________________________________
TMutHitMap::TMutHitMap() 
{}

//_____________________________________________________
TMutHitMap::TMutHitMap(PHKey::map_key_type map_key) : 
		PHMap<PHKey::key_type, TMutHit, TMutHit_v2>(map_key)
{}

//_____________________________________________________
TMutHitMap::iterator TMutHitMap::insert_new(UShort_t arm, 
							UShort_t station, 
							UShort_t octant, 
							UShort_t half_octant, 
							UShort_t gap, 
							UShort_t cathode, 
							UShort_t strip)
{
  
	TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, 
						 station, 
						 octant, 
						 half_octant, 
						 gap, 
						 cathode,
						 strip);	
	Key full_key(get_map_key(),key);

	insert(full_key, new value_imp_type(full_key,
							arm,
							station,
							octant,
							half_octant,
							gap,
							cathode,
							strip));
            
	return find(full_key);
}

//_____________________________________________________
TMutHitMap::iterator TMutHitMap::insert_clone(const TMutHitMap::pointer hit_ptr)
{
	// Full key is object key from to be cloned object and map key from this map.
	Key full_key(get_map_key(),hit_ptr->get()->get_key().get_obj_key());
	
	// Construct cloned from input hit
	insert(full_key, new value_imp_type(hit_ptr->get()));
	
	// Cloned object still has the wrong map key so we	we fix that.
	TMutHitMap::iterator hit_iter = find(full_key);
	hit_iter->get()->set_key(full_key);
	
	// Return the cloned object
	return hit_iter;
}


//_____________________________________________________
TMutHitMap::iterator TMutHitMap::get(const MUTOO::cathode_locator& location)
{
	// access to tuple elements is via get<[index]>()
	return get(location.get<0>(),
			 location.get<1>(),
			 location.get<2>(),
			 location.get<3>(),
			 location.get<4>(),
			 location.get<5>());
}

//_____________________________________________________
/*! 
Get an iterator to a specific hit strip. If the strip is not
in the map the iterator's current value will be equal to end() 
and at_end() will return true.	It is *never* safe to attempt
to dereference the iterator returned by this method without 
checking.
*/
TMutHitMap::iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station, 
						 UShort_t octant, 
						 UShort_t half_octant, 
						 UShort_t gap,
						 UShort_t cathode,
						 UShort_t strip)
{
	TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, 
						 station, 
						 octant, 
						 half_octant, 
						 gap, 
						 cathode,
						 strip);	
	Key full_key(get_map_key(),key);
	return find(full_key);
}

//_____________________________________________________
//! Get an iterator to all hits in given plane 
TMutHitMap::iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station, 
						 UShort_t octant, 
						 UShort_t half_octant, 
						 UShort_t gap,
						 UShort_t cathode)
{
	// key range associated with this plane
	TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
								station, 
								octant, 
								half_octant, 
								gap, 
								cathode);
	// return the iterator with specified range
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);

	return find(lower,upper);
}

//_____________________________________________________
/*! Get an iterator to all TMutHit objects in given plane.
	The plane is specified by the input boost tuple
*/
TMutHitMap::const_iterator TMutHitMap::get(const MUTOO::cathode_locator& location) const
{
	// access to tuple elements is via get<[index]>()
	return get(location.get<0>(),
			 location.get<1>(),
			 location.get<2>(),
			 location.get<3>(),
			 location.get<4>(),
			 location.get<5>());
}

//_____________________________________________________
//! Get a const_iterator to all hits in given plane 
TMutHitMap::const_iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station, 
						 UShort_t octant, 
						 UShort_t half_octant, 
						 UShort_t gap, 
						 UShort_t cathode) const
{
	// key range associated with this plane
	TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
								station, 
								octant, 
								half_octant, 
								gap, 
								cathode);
	// return the iterator with specified range
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);
	
	// return the iterator with specified range
	return find(lower,upper);
}

//_____________________________________________________
//! Get an iterator to all hits in a given station
TMutHitMap::iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station)
{

	TMutKeyGen::key_type lower_obj_key = TMutKeyGen::get_key(arm, 
								 station, 
								 0, 
								 0, 
								 0, 
								 0,
								 0);	
	
	TMutKeyGen::key_type upper_obj_key = TMutKeyGen::get_key(arm, 
								 station+1, 
								 0, 
								 0, 
								 0, 
								 0,
								 0)-1;	
							 
	// return the iterator with specified range
	Key lower(get_map_key(),lower_obj_key);
	Key upper(get_map_key(),upper_obj_key);
	
	// return the iterator with specified range
	return find(lower,upper);
}

//_____________________________________________________
//! Get an iterator to all hits in a given station
TMutHitMap::const_iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station) const
{

	TMutKeyGen::key_type lower_obj_key = TMutKeyGen::get_key(arm, 
								 station, 
								 0, 
								 0, 
								 0, 
								 0,
								 0);	
	
	TMutKeyGen::key_type upper_obj_key = TMutKeyGen::get_key(arm, 
								 station+1, 
								 0, 
								 0, 
								 0, 
								 0,
								 0)-1;	
								 
	// return the iterator with specified range
	Key lower(get_map_key(),lower_obj_key);
	Key upper(get_map_key(),upper_obj_key);
	
	// return the iterator with specified range
	return find(lower,upper);
}

//_____________________________________________________
//! Get an iterator to all hits in a given octant
TMutHitMap::iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station,
						 UShort_t octant)
{

// 	TMutKeyGen::key_type lower_obj_key = TMutKeyGen::get_key(arm, 
// 								 station, 
// 								 octant, 
// 								 0, 
// 								 0, 
// 								 0,
// 								 0);	
// 	
// 	TMutKeyGen::key_type upper_obj_key = TMutKeyGen::get_key(arm, 
// 								 station, 
// 								 octant+1, 
// 								 0, 
// 								 0, 
// 								 0,
// 								 0)-1;	
// 							 
// 	// return the iterator with specified range
// 	Key lower(get_map_key(),lower_obj_key);
// 	Key upper(get_map_key(),upper_obj_key);
	
	// key range associated with this octant
	TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
								station, 
								octant);
	
	// return the iterator with specified range
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);

	// return the iterator with specified range
	return find(lower,upper);
}

//_____________________________________________________
//! Get an iterator to all hits in a given octant
TMutHitMap::const_iterator TMutHitMap::get(UShort_t arm, 
						 UShort_t station,
						 UShort_t octant) const
{

// 	TMutKeyGen::key_type lower_obj_key = TMutKeyGen::get_key(arm, 
// 								 station, 
// 								 octant, 
// 								 0, 
// 								 0, 
// 								 0,
// 								 0);	
// 	
// 	TMutKeyGen::key_type upper_obj_key = TMutKeyGen::get_key(arm, 
// 								 station, 
// 								 octant+1, 
// 								 0, 
// 								 0, 
// 								 0,
// 								 0)-1;	
// 	// return the iterator with specified range
// 	Key lower(get_map_key(),lower_obj_key);
// 	Key upper(get_map_key(),upper_obj_key);
								 
	// key range associated with this octant
	TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
								station, 
								octant);
	
	// return the iterator with specified range
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);
	
	// return the iterator with specified range
	return find(lower,upper);
}

//_____________________________________________________
TMutHitMap::iterator TMutHitMap::get(UShort_t arm)		
{

// 	// key range associated with this plane
// 	TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm, 0, 0, 0, 0, 0, 0);	
// 	TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1, 0, 0, 0, 0, 0, 0);	
// 
// 	// return the iterator with specified range
// 	Key lower(get_map_key(),lower_key);
// 	Key upper(get_map_key(),upper_key);

	// key range associated with this arm
	TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

	// return the iterator with specified range
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);

	return find(lower,upper);
}

//_____________________________________________________
TMutHitMap::const_iterator TMutHitMap::get(UShort_t arm) const		
{

// 	// key range associated with this plane
// 	TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm, 0, 0, 0, 0, 0, 0);	
// 	TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1, 0, 0, 0, 0, 0, 0);	
// 
// 	// return the iterator with specified range
// 	Key lower(get_map_key(),lower_key);
// 	Key upper(get_map_key(),upper_key);

	// key range associated with this arm
	TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

	// return the iterator with specified range
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);

	return find(lower,upper);
}
