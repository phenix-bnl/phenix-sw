#include<TMutStubMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TMutStubMap::TMutStubMap() : _count(0)
{;}


TMutStubMap::TMutStubMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type,TMutStub, TMutStub_v1>(map_key),
  _count(0)
{;}

TMutStubMap::iterator TMutStubMap::insert_new(UShort_t arm,
					      UShort_t station,
					      UShort_t octant,
					      UShort_t hoct)
{
  UShort_t index = get_roll_count();
  
  // get the key for the new cluster
  //
  TMutKeyGen::key_type key = TMutKeyGen::get_key(arm,
						 station,
						 octant,
						 hoct,
						 index);
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert TMutStub
  //
  insert(full_key, new TMutStub_v1(full_key,
				   arm,
				   station,
				   octant,
				   hoct,
				   index));
  
  // okay not so efficient 
  //
  return find(full_key);
}




TMutStubMap::iterator 
TMutStubMap::get(UShort_t arm,
		 UShort_t station,
		 UShort_t octant) 
  
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant+1,     
						       0,
						       0)-1;
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMutStubMap::const_iterator 
TMutStubMap::get(UShort_t arm,
		 UShort_t station,
		 UShort_t octant) const

{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant,     
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station,    
						       octant+1,     
						       0,
						       0)-1;
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMutStubMap::iterator 
TMutStubMap::get(UShort_t arm,
		 UShort_t station,
		 UShort_t octant,
		 UShort_t hoct)	
{
  // key range associated with this half octant
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
                                                          station, 
                                                          octant, 
                                                          hoct); 
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMutStubMap::const_iterator 
TMutStubMap::get(UShort_t arm,
		 UShort_t station,
		 UShort_t octant,
		 UShort_t hoct) const		
{
  // key range associated with this half octant
  //
  TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm, 
                                                          station, 
                                                          octant, 
                                                          hoct); 
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TMutStubMap::iterator 
TMutStubMap::get(UShort_t arm,
		 UShort_t station)
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       0,     
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station+1,    
						       0,     
						       0,
						       0)-1;
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}


TMutStubMap::const_iterator 
TMutStubMap::get(UShort_t arm,
		 UShort_t station) const
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       station,    
						       0,     
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm,        
						       station+1,    
						       0,     
						       0,
						       0)-1;
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMutStubMap::iterator 
TMutStubMap::get(UShort_t arm)
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       0,    
						       0,     
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1,        
						       0,    
						       0,     
						       0,
						       0)-1;
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TMutStubMap::const_iterator 
TMutStubMap::get(UShort_t arm) const
{
  TMutKeyGen::key_type lower_key = TMutKeyGen::get_key(arm,        
						       0,    
						       0,     
						       0,
						       0);
  
  TMutKeyGen::key_type upper_key = TMutKeyGen::get_key(arm+1,        
						       0,    
						       0,     
						       0,
						       0)-1;
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}






