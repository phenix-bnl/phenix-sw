// $Id: TRxnpRawXangMap.cxx,v 1.2 2006/12/08 20:21:35 zhangc Exp $

/*!
  \file TRxnpRawXangMap.cxx
  \brief Container for TRxnpRawXang objects
  \author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2006/12/08 20:21:35 $
*/

#include<TRxnpRawXangMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_____________________________________________________
TRxnpRawXangMap::iterator TRxnpRawXangMap::insert_new(UShort_t arm, UShort_t ring)
{
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  TRxnpKeyGen::key_type key = TRxnpKeyGen::get_key(arm, ring, index);						 
  
	// full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert( full_key, new value_imp_type(full_key, arm, ring) );
  
  // okay not so efficient 
  return find(full_key);
}

//_____________________________________________________
TRxnpRawXangMap::iterator TRxnpRawXangMap::insert_clone(const TRxnpRawXangMap::pointer ptr)
{
  // Full key is object key from to be cloned object and map key from this map.
  Key full_key( get_map_key(), ptr->get()->get_key().get_obj_key() );
  
  // Construct cloned from input hit
  insert(full_key, new value_imp_type(ptr->get()));
  
  // Cloned object still has the wrong map key so we  we fix that.
  TRxnpRawXangMap::iterator hit_iter = find(full_key);
  hit_iter->get()->set_key(full_key);
  
  // Return the cloned object
  return hit_iter;
}

//_____________________________________________________
TRxnpRawXangMap::iterator TRxnpRawXangMap::get(UShort_t arm, UShort_t ring )
{
  // key range associated with this ring
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawXangMap::const_iterator TRxnpRawXangMap::get(UShort_t arm, UShort_t ring ) const  
{
  // key range associated with this ring
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm, ring );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawXangMap::iterator TRxnpRawXangMap::get(UShort_t arm )
{
  // key range associated with this arm
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm);
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//_____________________________________________________
TRxnpRawXangMap::const_iterator TRxnpRawXangMap::get(UShort_t arm ) const  
{
  // key range associated with this arm
  TRxnpKeyGen::key_range range = TRxnpKeyGen::get_key_range(arm );        
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//___________________________________________________________________
void TRxnpRawXangMap::print(std::ostream& os) const
{
  TRxnpRawXangMap::const_iterator iter = this->range();
  os<<"**************************************************************" << std::endl;
  os<<"*           Begin to dump TRxnpRawXangMap contents          *" << std::endl;
  os<<"**************************************************************" << std::endl;
  while(TRxnpRawXangMap::const_pointer ptr = iter.next())
    {
      ptr->get()->print(os);
    }
}
