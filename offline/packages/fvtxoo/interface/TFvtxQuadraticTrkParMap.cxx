
#include <PHKeyIterator.h>
#include <PHConstKeyIterator.h>
#include <TFvtxQuadraticTrkParMap.h>

TFvtxQuadraticTrkParMap::TFvtxQuadraticTrkParMap():
  _count(0)
{}

TFvtxQuadraticTrkParMap::TFvtxQuadraticTrkParMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TFvtxQuadraticTrkPar, TFvtxQuadraticTrkPar_v1>(map_key),
  _count(0)
{}

TFvtxQuadraticTrkParMap::iterator
TFvtxQuadraticTrkParMap::insert_new( unsigned short arm )
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
TFvtxQuadraticTrkParMap::iterator
TFvtxQuadraticTrkParMap::insert_clone(const TFvtxQuadraticTrkParMap::pointer trk_ptr)
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

TFvtxQuadraticTrkParMap::iterator
TFvtxQuadraticTrkParMap::get(unsigned short arm)		
{

  // key range associated with this plane
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,0,0);
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,0,0);

  // return the iterator with specified range
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TFvtxQuadraticTrkParMap::const_iterator
TFvtxQuadraticTrkParMap::get(unsigned short arm) const		
{  
  // key range associated with this plane
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,0,0);
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,0,0);
  
  // return the iterator with specified range
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}
