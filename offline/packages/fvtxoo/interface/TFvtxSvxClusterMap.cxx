// $Id: TFvtxSvxClusterMap.cxx,v 1.3 2012/10/01 04:28:01 keyaaron Exp $  

/*!
  \file    TFvtxSvxClusterMap.cxx
  \brief   Interface Object Container Class : TFvtxSvxClusterMap
  \author  M. Brooks
  \version $Revision: 1.3 $
  \date    $Date: 2012/10/01 04:28:01 $
*/

#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include "TFvtxSvxClusterMap.h"

//______________________________________________ 
TFvtxSvxClusterMap::TFvtxSvxClusterMap():
  _count(0)
{}

//______________________________________________
TFvtxSvxClusterMap::TFvtxSvxClusterMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TFvtxSvxCluster, TFvtxSvxCluster_v4>(map_key),
  _count(0)
{}

//______________________________________________
TFvtxSvxClusterMap::iterator
TFvtxSvxClusterMap::insert_new( void )
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
TFvtxSvxClusterMap::iterator
TFvtxSvxClusterMap::insert_clone(const TFvtxSvxClusterMap::pointer hit_ptr)
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
