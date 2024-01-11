// $Id: TMutTrkMap.cxx,v 1.12 2011/12/29 20:19:31 slash Exp $  

/*!
  \file    TFvtxTrkMap.cxx
  \brief   Interface Object Container Class : TFvtxTrkMap
  \author  M. Brooks
  \version $Revision: 1.12 $
  \date    $Date: 2011/12/29 20:19:31 $
*/

#include <iostream>
#include <PHKeyIterator.h>
#include <PHConstKeyIterator.h>

#include "TMutTrkMap.h"

using namespace std;

//______________________________________________   
TMutTrkMap::iterator TMutTrkMap::insert_new(
  const UShort_t& arm,
  const UShort_t& octant)
{
  
  UShort_t index = get_roll_count( arm, octant );
  
  // get the key for the new track
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm,
    octant,
    index);
  
  // full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert(full_key, new TMutTrk_v4(full_key,
    arm,
    octant,
    index));
  
  return find(full_key);
}


//______________________________________________   
TMutTrkMap::iterator TMutTrkMap::insert_clone(const TMutTrkMap::pointer trk_ptr)
{
  
  // we generate a new key for the cloned object
  UShort_t index = get_roll_count(
    trk_ptr->get()->get_arm(),
    trk_ptr->get()->get_octant());
  
  TMutKeyGen::key_type key = TMutKeyGen::get_key(
    trk_ptr->get()->get_arm(),
    trk_ptr->get()->get_octant(),
    index);
    
  // full key
  Key full_key(get_map_key(),key);
  
  // Construct cloned from input track
  value_imp_type *new_trk_ptr(  new value_imp_type(trk_ptr->get()) );
  new_trk_ptr->set_key( full_key );
  insert(full_key, new_trk_ptr );
  
  // Return the cloned object
  return find(full_key);

}

//______________________________________________   
TMutTrkMap::iterator TMutTrkMap::get(const UShort_t& arm, const UShort_t& octant)
{

  // key range associated with this plane
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,octant);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


//______________________________________________   
TMutTrkMap::const_iterator TMutTrkMap::get(const UShort_t& arm, const UShort_t& octant) const
{
  // key range associated with this gap
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm,octant);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//______________________________________________    
TMutTrkMap::iterator TMutTrkMap::get(const UShort_t& arm)		
{
  // key range associated with this plane
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,0,0);
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1,0,0);

  // return the iterator with specified range
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

//______________________________________________    
TMutTrkMap::const_iterator TMutTrkMap::get(const UShort_t& arm) const		
{

  // key range associated with this plane
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,0,0);
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1,0,0);

  // return the iterator with specified range
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);

  return find(lower,upper);
}

//______________________________________________    
void TMutTrkMap::write_array( void )		
{
  
  // some printout
  static bool first( true );
  if( first && !_write_ghost_tracks )
  {
    cout << "TMutTrkMap::write_array - _write_ghost_tracks is false. " << endl;
    cout << "TMutTrkMap::write_array - Ghost tracks are not written to output node" << endl;
    first = false;
  }
  
  // clear io
  get_io_utility().clear();

  // do nothing if map is empty
  if( empty() ) return;

  // put tracks into _io_utility
  // skip ghost tracks if requested
  iterator iter = range();
  while(pointer ptr = iter.next())
  { if(  _write_ghost_tracks || !ptr->get()->get_ghost() ) get_io_utility().insert(ptr->get()); }
  
  return;
  
}
