#include<TMuiClusterMapO.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//__________________________________________________________
TMuiClusterMapO::iterator TMuiClusterMapO::insert_new(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel,
  const UShort_t& orientation
  )
{

  // Get the new index from the roll count;
  UShort_t index = get_roll_count();

  // get the key for the new cluster
  TMuiKeyGen::key_type key = TMuiKeyGen::get_key(
    arm,
    plane,
    panel,
    orientation,
    index);
  
  // full key
  Key full_key(get_map_key(),key);

  // insert cluster
  insert(full_key, new value_imp_type(
    full_key,
    arm,
    plane,
    panel,
    orientation,
    index));

  // okay not so efficient
  return find(full_key);
  
}

//_________________________________________________________________
TMuiClusterMapO::iterator TMuiClusterMapO::get( const UShort_t& arm )
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
  
}

//________________________________________________________________________
TMuiClusterMapO::const_iterator TMuiClusterMapO::get( const UShort_t& arm) const
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(arm);

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//___________________________________________________________________
TMuiClusterMapO::iterator TMuiClusterMapO::get(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel,
  const UShort_t& orientation)
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(
    arm,
    plane,
    panel,
    orientation
    );

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


//___________________________________________________________________
TMuiClusterMapO::const_iterator TMuiClusterMapO::get(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel ) const
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(
    arm,
    plane,
    panel
    );

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

//___________________________________________________________________
TMuiClusterMapO::iterator TMuiClusterMapO::get(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel)
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(
    arm,
    plane,
    panel
    );

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


//___________________________________________________________________
TMuiClusterMapO::const_iterator TMuiClusterMapO::get(
  const UShort_t& arm,
  const UShort_t& plane,
  const UShort_t& panel,
  const UShort_t& orientation) const
{
  
  // key range associated with this plane
  TMuiKeyGen::key_range range = TMuiKeyGen::get_key_range(
    arm,
    plane,
    panel,
    orientation
    );

  // return the iterator with specified range
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}
