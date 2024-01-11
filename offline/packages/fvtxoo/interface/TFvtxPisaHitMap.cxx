// $Id: TFvtxPisaHitMap.cxx,v 1.2 2011/12/01 04:16:20 slash Exp $  

/*!
  \file    TFvtxPisaHitMap.cxx
  \brief   Interface Object Container Class : TFvtxPisaHitMap
  \author  M. Brooks
  \version $Revision: 1.2 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include "TFvtxPisaHitMap.h"

//______________________________________________ 
TFvtxPisaHitMap::TFvtxPisaHitMap():
_count(0)
{}

//______________________________________________
TFvtxPisaHitMap::TFvtxPisaHitMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TFvtxPisaHit, TFvtxPisaHit_v1>(map_key),
  _count(0)
{}

//______________________________________________
TFvtxPisaHitMap::iterator TFvtxPisaHitMap::insert_new( void )
{
  unsigned short index = get_roll_count();
  
  // get the key for the new track
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key(index);

  // full key
  Key full_key(get_map_key(),key);
  
  // insert track
  insert(full_key, new value_imp_type(full_key, index));
  
  // okay not so efficient 
  return find(full_key);
}

//______________________________________________
TFvtxPisaHitMap::iterator TFvtxPisaHitMap::insert_clone(const TFvtxPisaHitMap::pointer hit_ptr)
{
  
  // we generate a new key for the cloned object
  unsigned short index = get_roll_count();
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key( index );
    
  // full key
  Key full_key(get_map_key(),key);
  
  // Construct cloned from input track
  value_imp_type *new_hit_ptr( new value_imp_type(hit_ptr->get()) );
  new_hit_ptr->set_key( full_key );
  insert(full_key, new_hit_ptr );
  
  // Return the cloned object
  return find(full_key);

}
