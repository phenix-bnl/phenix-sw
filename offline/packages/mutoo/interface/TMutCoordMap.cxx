#include<TMutCoordMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

/*! Default contructor */
TMutCoordMap::TMutCoordMap() : 
  _count(0) {;}

/*! Construct with map key */
TMutCoordMap::TMutCoordMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TMutCoord, TMutCoord_v3>(map_key), 
  _count(0){;}


/*! Create a new TMutCoord object.  Returns an iterator to 
  the newly created object */

TMutCoordMap::iterator TMutCoordMap::insert_new(UShort_t arm, 
						UShort_t station, 
						UShort_t octant, 
						UShort_t half_octant, 
						UShort_t gap,
						UShort_t cathode)
  
  
{

  UShort_t index = get_roll_count();
  
  // get the key for the new coord
  //
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, 
						 station, 
						 octant, 
						 half_octant, 
						 gap,
						 cathode,
						 index);						 
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert coordinate
  //
  insert(full_key, new TMutCoord_v3(full_key,
				    arm,
				    station,
				    octant,
				    half_octant,
				    gap,
				    cathode,
				    index));
  
  // okay not so efficient 
  //
  return find(full_key);
}

/*! 
  Create a new TMutCoord object at specified location. The location
  is specified by a boost tuple. Returns an iterator to the newly created 
  object 
*/
TMutCoordMap::iterator 
TMutCoordMap::insert_new(const MUTOO::cathode_locator& location)
  
{
  // access to tuple elements is via get<[index]>()
  //
  return insert_new(location.get<0>(),
		    location.get<1>(),
		    location.get<2>(),
		    location.get<3>(),
		    location.get<4>(),
		    location.get<5>());
}

/*! Get an iterator to all TMutCoord objects in given gap */
TMutCoordMap::iterator TMutCoordMap::get(UShort_t arm, 
					 UShort_t station, 
					 UShort_t octant, 
					 UShort_t half_octant, 
					 UShort_t gap,
					 UShort_t cathode)
  
{
  // key range associated with this plane
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
							  station, 
							  octant, 
							  half_octant, 
							  gap,
							  cathode);
  
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
TMutCoordMap::iterator 
TMutCoordMap::get(const MUTOO::cathode_locator& location)
{
  // access to tuple elements is via get<[index]>()
  //
  return get(location.get<0>(),
	     location.get<1>(),
	     location.get<2>(),
	     location.get<3>(),
	     location.get<4>(),
	     location.get<5>());
}

/*! Get a const_iterator to all hits in given gap */
TMutCoordMap::const_iterator TMutCoordMap::get(UShort_t arm, 
					       UShort_t station, 
					       UShort_t octant, 
					       UShort_t half_octant, 
					       UShort_t gap,
					       UShort_t cathode) const  
{
  // key range associated with this gap
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,        
							  station,    
							  octant,     
							  half_octant,
							  gap,
							  cathode);        
  
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
TMutCoordMap::const_iterator 
TMutCoordMap::get(const MUTOO::cathode_locator& location) const
{
  // access to tuple elements is via get<[index]>()
  //
  return get(location.get<0>(),
	     location.get<1>(),
	     location.get<2>(),
	     location.get<3>(),
	     location.get<4>(),
	     location.get<5>());
}

TMutCoordMap::const_iterator TMutCoordMap::get(UShort_t arm, 
					       UShort_t station, 
					       UShort_t octant) const  
  
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       0,
						       0,
						       0,
						       0);        
  
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant+1,     
						       0,
						       0,
						       0,
						       0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMutCoordMap::iterator TMutCoordMap::get(UShort_t arm, 
					 UShort_t station, 
					 UShort_t octant)   
					       
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       0,
						       0,
						       0,
						       0);        
  
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant+1,     
						       0,
						       0,
						       0,
						       0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}
TMutCoordMap::const_iterator TMutCoordMap::get(UShort_t arm, 
					       UShort_t station, 
					       UShort_t octant,
					       UShort_t half_octant) const  
  
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       half_octant,
						       0,
						       0,
						       0);        
  
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       half_octant+1,
						       0,
						       0,
						       0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMutCoordMap::iterator TMutCoordMap::get(UShort_t arm, 
					 UShort_t station, 
					 UShort_t octant,
					 UShort_t half_octant)   
					       
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       half_octant,
						       0,
						       0,
						       0);        
  
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       half_octant+1,
						       0,
						       0,
						       0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}












