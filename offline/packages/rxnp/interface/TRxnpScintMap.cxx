// $Id: TRxnpScintMap.cxx,v 1.3 2006/12/08 20:21:36 zhangc Exp $

/*!
  \file TRxnpScintMap.cxx
  \brief Container for TRxnpScint objects
  \author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2006/12/08 20:21:36 $
*/

#include<TRxnpScintMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_____________________________________________________
TRxnpScintMap::iterator TRxnpScintMap::insert_new(UShort_t arm, UShort_t ring, UShort_t scint )
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
TRxnpScintMap::iterator TRxnpScintMap::insert_clone(const TRxnpScintMap::pointer hit_ptr)
{
  // Full key is object key from to be cloned object and map key from this map.
  Key full_key( get_map_key(), hit_ptr->get()->get_key().get_obj_key() );
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(hit_ptr->get()));
  
  // Cloned object still has the wrong map key so we  we fix that.
  TRxnpScintMap::iterator hit_iter = find(full_key);
  hit_iter->get()->set_key(full_key);
  
  // Return the cloned object
  return hit_iter;
}

//_____________________________________________________
TRxnpScintMap::iterator TRxnpScintMap::get(UShort_t arm, UShort_t ring, UShort_t scint )
{
  // key range associated with this scint
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring, scint );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpScintMap::const_iterator TRxnpScintMap::get(UShort_t arm, UShort_t ring, UShort_t scint ) const  
{
  // key range associated with this scint
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring, scint );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpScintMap::iterator TRxnpScintMap::get(UShort_t arm, UShort_t ring )
{
  // key range associated with this ring
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpScintMap::const_iterator TRxnpScintMap::get(UShort_t arm, UShort_t ring ) const  
{
  // key range associated with this ring
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpScintMap::iterator TRxnpScintMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpScintMap::const_iterator TRxnpScintMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//__________________________________________________________
void TRxnpScintMap::print(std::ostream& os) const
{
  TRxnpScintMap::const_iterator iter = this->range();
  os<<"**************************************************************" << std::endl;
  os<<"*           Begin to dump TRxnpScintMap contents          *" << std::endl;
  os<<"**************************************************************" << std::endl;
  while(TRxnpScintMap::const_pointer ptr = iter.next())
    {
      ptr->get()->print(os);
    }
}
