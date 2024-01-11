// $Id: TMutClusMap.cxx,v 1.13 2011/12/29 20:19:29 slash Exp $

/*!
  \file TMutClusMap.cxx
  \brief Interface Object Container : TMutClusMap
  \author S.Kelly
  \version $Revision: 1.13 $
  \date $Date: 2011/12/29 20:19:29 $
*/

#include<TMutClusMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//___________________________________________________________
TMutClusMap::TMutClusMap() :
  _count(0)
{}

//___________________________________________________________
TMutClusMap::TMutClusMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TMutClus, TMutClus_v3>(map_key), 
  _count(0)
{}

//___________________________________________________________
TMutClusMap::iterator TMutClusMap::insert_new(const UShort_t& arm, 
					      const UShort_t& station, 
					      const UShort_t& octant, 
					      const UShort_t& half_octant, 
					      const UShort_t& gap,
					      const UShort_t& cathode)	
{

  // Get the new index from the roll count;
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, 
						 station, 
						 octant, 
						 half_octant, 
						 gap,
						 cathode,
						 index);						 
  
  // full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert(full_key, new value_imp_type(full_key,
				      arm,
				      station,
				      octant,
				      half_octant,
				      gap,
				      cathode,
				      index));
  
  // okay not so efficient 
  return find(full_key);
}

//___________________________________________________________
TMutClusMap::iterator 
TMutClusMap::insert_new(const MUTOO::cathode_locator& location)
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


//___________________________________________________________
TMutClusMap::iterator TMutClusMap::get(const UShort_t& arm, 
				       const UShort_t& station, 
				       const UShort_t& octant, 
				       const UShort_t& half_octant, 
				       const UShort_t& gap,
				       const UShort_t& cathode)
  
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

//___________________________________________________________
TMutClusMap::iterator 
TMutClusMap::get(const MUTOO::cathode_locator& location)
{
  
  // access to tuple elements is via get<[index]>()
  return get(location.get<0>(),
	     location.get<1>(),
	     location.get<2>(),
	     location.get<3>(),
	     location.get<4>(),
	     location.get<5>());
}

//___________________________________________________________
TMutClusMap::const_iterator TMutClusMap::get(const UShort_t& arm, 
					     const UShort_t& station, 
					     const UShort_t& octant, 
					     const UShort_t& half_octant, 
					     const UShort_t& gap,
					     const UShort_t& cathode) const	
{

  // key range associated with this gap
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

//___________________________________________________________
TMutClusMap::const_iterator 
TMutClusMap::get(const MUTOO::cathode_locator& location) const
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


//___________________________________________________________
TMutClusMap::iterator TMutClusMap::get(const UShort_t& arm, 
				       const UShort_t& station,
				       const UShort_t& octant)
{
  
  // key range associated with this gap
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, station, octant );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);

}

//___________________________________________________________
TMutClusMap::const_iterator TMutClusMap::get(const UShort_t& arm, 
					     const UShort_t& station,
					     const UShort_t& octant ) const
{

  // key range associated with this gap
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, station, octant );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);

}

//___________________________________________________________
TMutClusMap::iterator TMutClusMap::get(const UShort_t& arm, 
				       const UShort_t& station)
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
  //
  Key lower(get_map_key(),lower_obj_key);
  Key upper(get_map_key(),upper_obj_key);
	
  // return the iterator with specified range
  return find(lower,upper);
}

//___________________________________________________________
TMutClusMap::const_iterator TMutClusMap::get(const UShort_t& arm, 
					     const UShort_t& station) const
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
  //
  Key lower(get_map_key(),lower_obj_key);
  Key upper(get_map_key(),upper_obj_key);
  
  // return the iterator with specified range
  //
  return find(lower,upper);
}











