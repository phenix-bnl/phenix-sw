// $Id: TMutMCHitMap.cxx,v 1.10 2011/12/29 20:19:30 slash Exp $

/*!
  \file    TMutMCHitMap.cxx
  \brief   Container for MUTR TMutMCHit objects
  \author  S. Kelly
  \version $Revision: 1.10 $
  \date    $Date: 2011/12/29 20:19:30 $
*/

#include<TMutMCHitMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMutMCHitMap::TMutMCHitMap() :
	_count(0){;}


TMutMCHitMap::TMutMCHitMap(PHKey::map_key_type map_key) : 
	PHMap<PHKey::key_type,TMutMCHit, TMutMCHit_v2>(map_key),
	_count(0){;}

TMutMCHitMap::iterator TMutMCHitMap::insert_new(
	const UShort_t& arm, 
	const UShort_t& station, 
	const UShort_t& octant, 
	const UShort_t& half_octant, 
	const UShort_t& gap)
	
	
{
	UShort_t index = get_roll_count();
	
	// get the key for the new cluster
	TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, 
		 station, 
		 octant, 
		 half_octant, 
		 gap, 
		 index);						 

	// full key
	Key full_key(get_map_key(),key);
	
	// insert cluster
	insert(full_key, new TMutMCHit_v2(full_key,arm,station,octant,half_octant,gap,index));
	
	// okay not so efficient 
	return find(full_key);

}


TMutMCHitMap::iterator TMutMCHitMap::get(
	const UShort_t& arm, 
	const UShort_t& station, 
	const UShort_t& octant, 
	const UShort_t& half_octant, 
	const UShort_t& gap)
	
{
	// key range associated with this plane
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

TMutMCHitMap::iterator 
TMutMCHitMap::get(const MUTOO::gap_locator& location)
{
	// access to tuple elements is via get<[index]>()
	//
	return get(location.get<0>(),
		location.get<1>(),
		location.get<2>(),
		location.get<3>(),
		location.get<4>());
}

TMutMCHitMap::const_iterator TMutMCHitMap::get(
	const UShort_t& arm, 
	const UShort_t& station, 
	const UShort_t& octant, 
	const UShort_t& half_octant, 
	const UShort_t& gap) const	
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

TMutMCHitMap::const_iterator 
TMutMCHitMap::get(const MUTOO::gap_locator& location) const
{
	// access to tuple elements is via get<[index]>()
	//
	return get(location.get<0>(),
		location.get<1>(),
		location.get<2>(),
		location.get<3>(),
		location.get<4>());
}
