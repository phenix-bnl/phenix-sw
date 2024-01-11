// $Id: TRxnpRawScintMap.cxx,v 1.5 2007/03/08 23:11:19 zhangc Exp $

/*!
  \file TRxnpRawScintMap.cxx
  \brief Container for TRxnpRawScint objects
  \author Chun Zhang
  \version $Revision: 1.5 $
  \date    $Date: 2007/03/08 23:11:19 $
*/

#include <iostream>
#include "TRxnpRawScintMap.h"
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

using namespace std;

//_____________________________________________________
TRxnpRawScintMap::iterator TRxnpRawScintMap::insert_new(UShort_t arm, UShort_t ring, UShort_t scint )
{
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  TRxnpKeyGen::key_type key = TRxnpKeyGen::get_key(arm, ring, scint, index);						 
  
	// full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert( full_key, new value_imp_type(full_key, arm, ring, scint ) );
  
  // okay not so efficient 
  return find(full_key);
}

//_____________________________________________________
TRxnpRawScintMap::iterator TRxnpRawScintMap::insert_clone(const TRxnpRawScintMap::pointer hit_ptr)
{
  // Full key is object key from to be cloned object and map key from this map.
  Key full_key( get_map_key(), hit_ptr->get()->get_key().get_obj_key() );
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(hit_ptr->get()));
  
  // Cloned object still has the wrong map key so we  we fix that.
  TRxnpRawScintMap::iterator hit_iter = find(full_key);
  hit_iter->get()->set_key(full_key);
  
  // Return the cloned object
  return hit_iter;
}

//_____________________________________________________
TRxnpRawScintMap::iterator TRxnpRawScintMap::get(UShort_t arm, UShort_t ring, UShort_t scint )
{
  // key range associated with this scint
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring, scint );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawScintMap::const_iterator TRxnpRawScintMap::get(UShort_t arm, UShort_t ring, UShort_t scint ) const  
{
  // key range associated with this scint
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring, scint );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawScintMap::iterator TRxnpRawScintMap::get(UShort_t arm, UShort_t ring )
{
  // key range associated with this ring
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawScintMap::const_iterator TRxnpRawScintMap::get(UShort_t arm, UShort_t ring ) const  
{
  // key range associated with this ring
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawScintMap::iterator TRxnpRawScintMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawScintMap::const_iterator TRxnpRawScintMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
void TRxnpRawScintMap::print(std::ostream& os) const
{
  TRxnpRawScintMap::const_iterator iter = this->range();
  os<<"**************************************************************" << std::endl;
  os<<"*           Begin to dump TRxnpRawScintMap contents          *" << std::endl;
  os<<"**************************************************************" << std::endl;
  while(TRxnpRawScintMap::const_pointer ptr = iter.next())
    {
      ptr->get()->print(os);
    }
}
