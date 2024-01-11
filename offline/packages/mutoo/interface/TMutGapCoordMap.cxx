#include<TMutGapCoordMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMutGapCoordMap::TMutGapCoordMap() : 
  _count(0) {;}

TMutGapCoordMap::TMutGapCoordMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TMutGapCoord, TMutGapCoord_v2>(map_key),
  _count(0){;}


TMutGapCoordMap::iterator TMutGapCoordMap::insert_new(UShort_t arm, 
						      UShort_t station, 
						      UShort_t octant, 
						      UShort_t half_octant, 
						      UShort_t gap)
  
  
  
{

  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  //
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, 
						 station, 
						 octant, 
						 half_octant, 
						 gap,
						 index);						 
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert cluster
  //
  insert(full_key, new TMutGapCoord_v2(full_key,
				       arm,
				       station,
				       octant,
				       half_octant,
				       gap,
				       index));
  
  // okay not so efficient 
  //
  return find(full_key);
}


TMutGapCoordMap::iterator TMutGapCoordMap::get(UShort_t arm, 
					       UShort_t station, 
					       UShort_t octant, 
					       UShort_t half_octant, 
					       UShort_t gap)
				       
{
  // key range associated with this plane
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
							  station, 
							  octant, 
							  half_octant, 
							  gap);
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

/*! Get an iterator to all TMutGapCoord objects in given half_octant */
TMutGapCoordMap::iterator TMutGapCoordMap::get(UShort_t arm, 
					       UShort_t station, 
					       UShort_t octant, 
					       UShort_t half_octant)
{
  // key range associated with this plane
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
							  station, 
							  octant, 
							  half_octant);
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

/*! Get an iterator to all TMutGapCoord objects in given station */
TMutGapCoordMap::iterator TMutGapCoordMap::get(UShort_t arm, 
					       UShort_t station)
{
  // key range associated with this plane
  //
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm, 
						       station, 
						       0, 
						       0, 
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm, 
						       station+1, 
						       0, 
						       0, 
						       0,
						       0);
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

/*! 
Get an iterator to all TMutGapCoord objects in given station 
*/
TMutGapCoordMap::const_iterator TMutGapCoordMap::get(UShort_t arm, 
						     UShort_t station) const
{
  // key range associated with this plane
  //
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm, 
						       station, 
						       0, 
						       0, 
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm, 
						       station+1, 
						       0, 
						       0, 
						       0,
						       0);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

/*! 
  Get an iterator to all TMutClus objects in given plane.
  The plane is specified by the input boost tuple
*/
TMutGapCoordMap::iterator 
TMutGapCoordMap::get(const MUTOO::gap_locator& location)
{
  // access to tuple elements is via get<[index]>()
  //
  return get(location.get<0>(),
	     location.get<1>(),
	     location.get<2>(),
	     location.get<3>(),
	     location.get<4>());
	     
}

/*! Get a const_iterator to all hits in given gap */
TMutGapCoordMap::const_iterator TMutGapCoordMap::get(UShort_t arm, 
					       UShort_t station, 
					       UShort_t octant, 
					       UShort_t half_octant, 
					       UShort_t gap) const  
					       
{
  // key range associated with this gap
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,        
							  station,    
							  octant,     
							  half_octant,
							  gap);        
							  
							  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

/*! Get a const_iterator to all hits in given half_octant */
TMutGapCoordMap::const_iterator TMutGapCoordMap::get(UShort_t arm, 
						     UShort_t station, 
						     UShort_t octant, 
						     UShort_t half_octant) const
  
{
  // key range associated with this gap
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,        
							  station,    
							  octant,     
							  half_octant);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

/*! 
  Get an iterator to all TMutClus objects in given plane.
  The plane is specified by the input boost tuple
*/
TMutGapCoordMap::const_iterator 
TMutGapCoordMap::get(const MUTOO::gap_locator& location) const
{
  // access to tuple elements is via get<[index]>()
  //
  return get(location.get<0>(),
	     location.get<1>(),
	     location.get<2>(),
	     location.get<3>(),
	     location.get<4>());
}










