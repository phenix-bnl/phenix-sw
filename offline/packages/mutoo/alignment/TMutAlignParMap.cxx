// $Id: TMutAlignParMap.cxx,v 1.2 2011/12/24 04:48:18 slash Exp $
#include<TMutAlignParMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

//_________________________________________________________
TMutAlignParMap::TMutAlignParMap():
_count(0)
{}

//_________________________________________________________
TMutAlignParMap::TMutAlignParMap(PHKey::map_key_type map_key) :
    PHMap<PHKey::key_type, TMutAlignPar, TMutAlignPar_v1 >(map_key),
    _count(0)
{}

//_________________________________________________________
TMutAlignParMap::iterator TMutAlignParMap::insert_new(unsigned short arm)
{

    unsigned short index = get_roll_count();

    // get the key for the new cluster
    TMutKeyGen::key_type key = TMutKeyGen::get_key(arm, index);

    // full key
    Key full_key(get_map_key(),key);

    // insert
    insert(full_key, new TMutAlignPar_v1(full_key, arm, index));

    return find(full_key);
}


//_________________________________________________________
TMutAlignParMap::iterator TMutAlignParMap::get(unsigned short arm)
{

    // key range associated with this plane
    TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

    // return the iterator with specified range
    Key lower(get_map_key(),range.first);
    Key upper(get_map_key(),range.second);
    return find(lower,upper);
}


//_________________________________________________________
TMutAlignParMap::const_iterator TMutAlignParMap::get(unsigned short arm) const
{

    // key range associated with this gap
    TMutKeyGen::key_range range = TMutKeyGen::get_key_range(arm);

    // return the iterator with specified range
    Key lower(get_map_key(),range.first);
    Key upper(get_map_key(),range.second);
    return find(lower,upper);
}
