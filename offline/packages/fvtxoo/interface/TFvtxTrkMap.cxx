// $Id: TFvtxTrkMap.cxx,v 1.3 2011/12/01 04:16:21 slash Exp $  

/*!
  \file    TFvtxTrkMap.cxx
  \brief   Interface Object Container Class : TFvtxTrkMap
  \author  M. Brooks
  \version $Revision: 1.3 $
  \date    $Date: 2011/12/01 04:16:21 $
*/

#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include "TFvtxTrkMap.h"

//______________________________________________ 
TFvtxTrkMap::TFvtxTrkMap():
_count(0)
{}

//______________________________________________
TFvtxTrkMap::TFvtxTrkMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TFvtxTrk, TFvtxTrk_v1>(map_key),
  _count(0)
{}

//______________________________________________
TFvtxTrkMap::iterator TFvtxTrkMap::insert_new( unsigned short arm )
{
  unsigned short index = get_roll_count();
  
  // get the key for the new track
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key( arm, index );

  // full key
  Key full_key(get_map_key(),key);
  
  // insert track
  insert(full_key, new value_imp_type(full_key, arm, index));
  
  // okay not so efficient 
  return find(full_key);
  
}

//______________________________________________
TFvtxTrkMap::iterator TFvtxTrkMap::insert_clone(const TFvtxTrkMap::pointer trk_ptr)
{
  
  // we generate a new key for the cloned object
  unsigned short index = get_roll_count();
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key( trk_ptr->get()->get_arm(), index);
    
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
TFvtxTrkMap::iterator TFvtxTrkMap::get(unsigned short arm)		
{

  // key range associated with this plane
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,0,0);
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,0,0);

  // return the iterator with specified range
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

//______________________________________________
TFvtxTrkMap::const_iterator TFvtxTrkMap::get(unsigned short arm) const		
{
  
  // key range associated with this plane
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,0,0);
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,0,0);
  
  // return the iterator with specified range
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);

}
