#include<TFvtxEvalMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


TFvtxEvalMap::TFvtxEvalMap() :
  _count(0){;}


TFvtxEvalMap::TFvtxEvalMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type,TFvtxEval,TFvtxEval_v1>(map_key),
  _count(0){;}

TFvtxEvalMap::iterator TFvtxEvalMap::insert_new(unsigned short arm,
					      unsigned short cage)
{

  unsigned short index = get_roll_count();
  
  // get the key for the new cluster
  //
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key(arm,
						 cage,
						 index);
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert cluster
  //
  insert(full_key, new TFvtxEval_v1(full_key,
				arm,
				cage,
				index));
  
  // okay not so efficient 
  //
  return find(full_key);
}


TFvtxEvalMap::iterator 
TFvtxEvalMap::get(unsigned short arm,
		unsigned short cage)
{
  // key range associated with this plane
  //
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,cage);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TFvtxEvalMap::const_iterator 
TFvtxEvalMap::get(unsigned short arm,
		unsigned short cage) const
{
  // key range associated with this gap
  //
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,cage);

  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


TFvtxEvalMap::iterator 
TFvtxEvalMap::get(unsigned short arm)		
{
  // key range associated with this plane
  //
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,0,0);
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,0,0);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}


TFvtxEvalMap::const_iterator 
TFvtxEvalMap::get(unsigned short arm) const		
{
  // key range associated with this plane
  //
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,0,0);
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,0,0);
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}






