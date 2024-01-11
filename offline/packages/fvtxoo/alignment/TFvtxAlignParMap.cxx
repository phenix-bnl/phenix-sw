// $Id: TFvtxAlignParMap.cxx,v 1.2 2011/12/01 04:16:18 slash Exp $

#include<TFvtxAlignParMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_________________________________________________________
TFvtxAlignParMap::TFvtxAlignParMap():
_count(0)
{}

//_________________________________________________________
TFvtxAlignParMap::TFvtxAlignParMap(PHKey::map_key_type map_key) :
    PHMap<PHKey::key_type, TFvtxAlignPar, TFvtxAlignPar_v1 >(map_key),
    _count(0)
{}

//_________________________________________________________
TFvtxAlignParMap::iterator TFvtxAlignParMap::insert_new(unsigned short arm)
{

    unsigned short index = get_roll_count();

    // get the key for the new cluster
    TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, index);

    // full key
    Key full_key(get_map_key(),key);

    // insert
    insert(full_key, new TFvtxAlignPar_v1(full_key, arm, index));

    return find(full_key);
}


//_________________________________________________________
TFvtxAlignParMap::iterator TFvtxAlignParMap::get(unsigned short arm)
{

    // key range associated with this plane
    TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

    // return the iterator with specified range
    Key lower(get_map_key(),range.first);
    Key upper(get_map_key(),range.second);
    return find(lower,upper);
}


//_________________________________________________________
TFvtxAlignParMap::const_iterator TFvtxAlignParMap::get(unsigned short arm) const
{

    // key range associated with this gap
    TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

    // return the iterator with specified range
    Key lower(get_map_key(),range.first);
    Key upper(get_map_key(),range.second);
    return find(lower,upper);
}
