#include<TMui1DRoadMapO.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//______________________________________________________________________________
TMui1DRoadMapO::iterator TMui1DRoadMapO::insert_new(const UShort_t& arm, const UShort_t& panel, const UShort_t& orientation)
{  
  
  UShort_t index = get_roll_count( arm, panel, orientation ) ;

  // get the key for the new 1D road
  TMuiKeyGen::key_type key = TMuiKeyGen::get_key(arm,
    0, 
    panel,
    orientation,
    index);
  
  // full key
  Key full_key(get_map_key(),key);
  
  // insert cluster
  insert(full_key, new value_imp_type(full_key,
    arm,
    panel,
    orientation,
    index));
  
  // okay not so efficient 
  return find(full_key);
}

//______________________________________________________________________________
TMui1DRoadMapO::iterator TMui1DRoadMapO::get(const UShort_t& arm)		
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//______________________________________________________________________________
TMui1DRoadMapO::const_iterator TMui1DRoadMapO::get(const UShort_t& arm) const		
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//______________________________________________________________________________
TMui1DRoadMapO::iterator TMui1DRoadMapO::get(const UShort_t& arm,
  const UShort_t& panel,
  const UShort_t& orientation)
{

  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm,
    0,
    panel,
    orientation
    );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


//______________________________________________________________________________
TMui1DRoadMapO::const_iterator TMui1DRoadMapO::get(const UShort_t& arm,
  const UShort_t& panel,
  const UShort_t& orientation) const
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(
    arm,
    0,
    panel,
    orientation
    );
  
  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
