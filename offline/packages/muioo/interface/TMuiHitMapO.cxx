// $Id: TMuiHitMapO.cxx,v 1.3 2007/07/03 23:10:21 hpereira Exp $

/*!
   \file    TMuiHitMapO.cxx
   \brief   Container for MUID TMuiHitO objects
   \author  Jason Newby/Chun Zhang/David Silvermyr/Hugo Pereira
   \version $Revision: 1.3 $
   \date    $Date: 2007/07/03 23:10:21 $
*/



#include<TMuiHitMapO.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


//____________________________________________
TMuiHitMapO::iterator TMuiHitMapO::insert_new(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel,
  const UShort_t& orientation,
  const UShort_t& twopack
  )
{
  // Get the new index from the roll count;
  UShort_t index = get_roll_count( arm, plane, panel, orientation, twopack );
  
  // get the key for the new hit
  TMuiKeyGen::key_type key = TMuiKeyGen::get_key(
    arm,
    plane,
    panel,
    orientation,
    twopack,
    index);
  
  // full key
  Key full_key(get_map_key(),key);
  
  // insert hit
  insert(full_key, new value_imp_type(
    full_key,
    arm,
    plane,
    panel,
    orientation,
    twopack,                                   
    index));
  
  // okay not so efficient 
  return find(full_key);
}

//____________________________________________
TMuiHitMapO::iterator TMuiHitMapO::insert_clone(const TMuiHitMapO::pointer hit_ptr)
{
  
  // we generate a new key for the cloned object
  UShort_t index = get_roll_count( 
    hit_ptr->get()->get_arm(),
    hit_ptr->get()->get_plane(),
    hit_ptr->get()->get_panel(),
    hit_ptr->get()->get_orientation(),
    hit_ptr->get()->get_twopack()
    );
  
  TMuiKeyGen::key_type key = TMuiKeyGen::get_key(
    hit_ptr->get()->get_arm(),
    hit_ptr->get()->get_plane(),
    hit_ptr->get()->get_panel(),
    hit_ptr->get()->get_orientation(),
    hit_ptr->get()->get_twopack(),
    index );
    
  // full key
  Key full_key(get_map_key(),key);
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(hit_ptr->get()));
  
  // Cloned object still has the old key. update
  TMuiHitMapO::iterator hit_iter = find(full_key);
  hit_iter->get()->set_key(full_key);
  
  // Return the cloned object
  return hit_iter;
}

//____________________________________________
TMuiHitMapO::iterator TMuiHitMapO::get(const UShort_t& arm)		
{
  // Range 
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//____________________________________________
TMuiHitMapO::const_iterator TMuiHitMapO::get(const UShort_t& arm) const		
{
  // Range 
  //
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//____________________________________________
TMuiHitMapO::iterator TMuiHitMapO::get(const UShort_t& arm, const UShort_t& plane)
{
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm, plane);
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


//____________________________________________
TMuiHitMapO::const_iterator TMuiHitMapO::get(const UShort_t& arm, const UShort_t& plane ) const
{
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range( arm, plane );
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  
  return find(lower,upper);
	
}

//____________________________________________
TMuiHitMapO::iterator TMuiHitMapO::get(
		const UShort_t& arm,
		const UShort_t& plane,
		const UShort_t& panel,
		const UShort_t& orientation)
{
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm,
    plane,
    panel,
    orientation);
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


//____________________________________________
TMuiHitMapO::const_iterator TMuiHitMapO::get(
		const UShort_t& arm,
		const UShort_t& plane,
		const UShort_t& panel,
		const UShort_t& orientation) const
{
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm,
							  plane,
							  panel,
							  orientation);
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  
  return find(lower,upper);
}

//______________________________________________________
TMuiHitMapO::iterator
TMuiHitMapO::get(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel,
  const UShort_t& orientation,
  const UShort_t& twopack)
{
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range( 
    arm, 
    plane, 
    panel, 
    orientation, 
    twopack);
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  
  return find(lower,upper);
}

//______________________________________________________
TMuiHitMapO::const_iterator
TMuiHitMapO::get(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel,
  const UShort_t& orientation,
  const UShort_t& twopack) const
{
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range( 
    arm, 
    plane, 
    panel, 
    orientation, 
    twopack);
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  
  return find(lower,upper);
}








