// $Id: TFvtxMCHitMap.cxx,v 1.7 2011/12/01 04:16:20 slash Exp $

/*!
  \file    TFvtxMCHitMap.cxx
  \brief   Container for MUTR TFvtxMCHit objects
  \author  S. Kelly
  \version $Revision: 1.7 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

#include "TFvtxMCHitMap.h"

//___________________________________________________________________
TFvtxMCHitMap::TFvtxMCHitMap() :
	_count(0)
{}

//___________________________________________________________________
TFvtxMCHitMap::TFvtxMCHitMap(PHKey::map_key_type map_key) : 
	PHMap<PHKey::key_type,TFvtxMCHit, TFvtxMCHit_v1>(map_key),
	_count(0)
{}

//___________________________________________________________________
TFvtxMCHitMap::iterator TFvtxMCHitMap::insert_new(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	)
{
	unsigned short index = get_roll_count();
	
	// get the key for the new cluster
	TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key(
		arm, 
                cage,
		station, 
		sector, 
		column, 
		index);						 

	// full key
	Key full_key(get_map_key(),key);
	
	// insert cluster
	insert(full_key, new TFvtxMCHit_v1(
			full_key,
			arm,
                        cage,
			station,
			sector,
			column,
			index));
	
	// okay not so efficient 
	return find(full_key);

}

//__________________________________________________________
TFvtxMCHitMap::iterator TFvtxMCHitMap::get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
) {
	// key range associated with this plane
	TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(
			arm, 
                        cage,
			station, 
			sector, 
			column );		
	
	// return the iterator with specified range
	//
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);
	return find(lower,upper);
}

//__________________________________________________________
TFvtxMCHitMap::const_iterator TFvtxMCHitMap::get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
		) const
{
	// key range associated with this gap
	//
	TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range( 
			arm, 
                        cage,
			station, 
			sector, 
			column );		
	
	// return the iterator with specified range
	//
	Key lower(get_map_key(),range.first);
	Key upper(get_map_key(),range.second);
	return find(lower,upper);
}

TFvtxMCHitMap::iterator 
TFvtxMCHitMap::get(
		   const unsigned short& arm,
                   const unsigned short& cage,
		   const unsigned short& station,
		   const unsigned short& sector
		   ) 
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(
							    arm, 
                                                            cage,
							    station,
							    sector
							    );
	
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::const_iterator 
TFvtxMCHitMap::get(
		   const unsigned short& arm,
                   const unsigned short& cage,
		   const unsigned short& station,
		   const unsigned short& sector
		   ) const
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(
							    arm, 
                                                            cage,
							    station,
							    sector
							    );
	
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::iterator 
TFvtxMCHitMap::get(const unsigned short& arm,
                   const unsigned short& cage,
                   const unsigned short& station) 
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,cage,station);
	
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::const_iterator 
TFvtxMCHitMap::get(const unsigned short& arm, const unsigned short& cage, const unsigned short& station) const
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,cage,station);
	
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::iterator
TFvtxMCHitMap::get(const unsigned short& arm,
                   const unsigned short& cage)
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,cage);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::const_iterator
TFvtxMCHitMap::get(const unsigned short& arm, const unsigned short& cage) const
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,cage);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::iterator 
TFvtxMCHitMap::get(const unsigned short& arm) 
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm);
	
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

TFvtxMCHitMap::const_iterator 
TFvtxMCHitMap::get(const unsigned short& arm) const
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm);
	
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
