// $Id: TFvtxHitMap.cxx,v 1.8 2011/12/01 04:16:20 slash Exp $

/*!
\file TFvtxHitMap.cxx
\brief Container for Fvtx TFvtxHit objects
\author S. Kelly
\version $Revision: 1.8 $
\date $Date: 2011/12/01 04:16:20 $
*/

#include "TFvtxHitMap.h"

//___________________________________________________________________
TFvtxHitMap::TFvtxHitMap()
{}

//___________________________________________________________________
TFvtxHitMap::TFvtxHitMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type,TFvtxHit, TFvtxHit_v1>(map_key)
{}

//___________________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::insert_new(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station,
  const unsigned short& sector,
  const unsigned short& column,
  const unsigned short& strip
  )
{
  
  // get the key for the new cluster
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key(
  arm, 
  cage,
  station, 
    sector, 
    column, 
    strip);						 
  
  // full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert(full_key, new TFvtxHit_v1(
    full_key,
    arm,
    cage,
    station,
    sector,
    column,
    strip));
  
  // okay not so efficient 
  return find(full_key);
  
}

//_____________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::insert_clone(const TFvtxHitMap::pointer hit_ptr)
{
  // Full key is object key from to be cloned object and map key from this map.
  Key full_key(get_map_key(),hit_ptr->get()->get_key().get_obj_key());
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(hit_ptr->get()));
  
  // Cloned object still has the wrong map key so we	we fix that.
  TFvtxHitMap::iterator hit_iter = find(full_key);
  hit_iter->get()->set_key(full_key);
  
  // Return the cloned object
  return hit_iter;
}

//__________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station,
  const unsigned short& sector,
  const unsigned short& column,
  const unsigned short& strip
  ) 
{
  // key range associated with this plane
  PHKey::object_key_type key = TFvtxKeyGen::get_key( 
    arm, 
    cage,
    station, 
    sector, 
    column,
    strip );		
  
  // return the iterator with specified range
  Key lower(get_map_key(),key);
  return find(lower,lower);
}

//__________________________________________________________
TFvtxHitMap::const_iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station,
  const unsigned short& sector,
  const unsigned short& column,
  const unsigned short& strip
  ) const
{
  // key range associated with this plane
  PHKey::object_key_type key = TFvtxKeyGen::get_key( 
    arm, 
    cage,
    station, 
    sector, 
    column,
    strip );		
  
  // return the iterator with specified range
  Key lower(get_map_key(),key);
  return find(lower,lower);
}
//__________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station,
  const unsigned short& sector,
  const unsigned short& column
  ) 
{
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
TFvtxHitMap::const_iterator TFvtxHitMap::get(
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

//__________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::get(
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
    sector );		
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::const_iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station,
  const unsigned short& sector
  ) const
{
  // key range associated with this gap
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range( 
    arm, 
    cage,
    station, 
    sector );		
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station
  ) 
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(
    arm, 
    cage,
    station);		
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::const_iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station
  ) const
{
  // key range associated with this gap
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range( 
    arm, 
    cage,
    station);		
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage
  )
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(
    arm,
    cage);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::const_iterator TFvtxHitMap::get(
  const unsigned short& arm,
  const unsigned short& cage
  ) const
{
  // key range associated with this gap
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(
    arm,
    cage);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::iterator TFvtxHitMap::get( const unsigned short& arm ) 
{
  // key range associated with this plane
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range( arm );		
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
TFvtxHitMap::const_iterator TFvtxHitMap::get( const unsigned short& arm ) const
{
  // key range associated with this gap
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm);		
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
