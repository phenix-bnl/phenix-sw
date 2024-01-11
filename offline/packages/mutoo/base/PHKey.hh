
#ifndef __PHKEY_H__
#define __PHKEY_H__

// $Id: PHKey.hh,v 1.13 2012/06/19 12:17:57 bbannier Exp $

/*!
   \file PHKey.hh
   \brief  associated object technology
   \author S.Kelly H.Pereira
   \version $Revision: 1.13 $
   \date $Date: 2012/06/19 12:17:57 $
*/

// ROOT headers
#include<TDataType.h>
#include<TObject.h>
#include<algorithm>

// STL/BOOST headers
#include<vector>

#ifndef __CINT__
#include <set>
#include <map>
#include <list>
#include <boost/smart_ptr.hpp>
#include <boost/any.hpp>
#include "PHClassId.hh"
#include "PHNonStaticMapFinder.h"
#endif

// MUTOO header
#include "MUTOO.h"

// Forward declerations
template<typename Value> class PHKeyIterator;
template<typename Value> class PHConstKeyIterator;
template<typename Key, typename Value> class PHMapBaseT;

typedef unsigned long ULong_t;
typedef unsigned short UShort_t;

/*! \ingroup classes */
//! Key provides the interface to two 32 bit keys
/*! Key provides the interface to two 32 bit keys. The
  first serves to identify the container associated with
  a given object. The second identifies the object itself
*/
class Key
{

  public:

  typedef ULong_t object_key_type;
  typedef ULong_t map_key_type;

  //! constructor
  Key():
    _map_key(0),
    _obj_key(0)
  {}

  //! constructor
  Key(ULong_t map_key,ULong_t obj_key):
    _map_key(map_key),
    _obj_key(obj_key)
  {}

  //! virtual destructor
  virtual ~Key()
  {}

  //! object unique id (within map)
  const ULong_t& get_obj_key() const
  { return _obj_key;}

  //! parent map unique id
  const ULong_t& get_map_key() const
  { return _map_key;}

  //! object unique id (within map)
  void set_obj_key(ULong_t key)
  { _obj_key = key;}

  void set_map_key(ULong_t key)
  { _map_key = key;}

  //! less than operator
  bool operator<(const Key& rhs) const
  { return _obj_key<rhs._obj_key; }

  //! equal to operator
  bool operator==(const Key& rhs) const
  { return _obj_key==rhs._obj_key && _map_key == rhs._map_key; }

  //! larger than or equal to operator
  bool operator>=(const Key& rhs) const
  { return _obj_key>=rhs._obj_key && _map_key >= rhs._map_key; }

  //! print key
  void print(std::ostream& os = std::cout) const
  { os << "map_key: " << _map_key << " " << "object_key: " << _obj_key << std::endl; }

  private:

  //! map unique id
  map_key_type _map_key;

  //! object unique id in the map
  object_key_type _obj_key;

  ClassDef(Key,1)

};

/*! \ingroup classes */

//! PHKeyList provides key list interface
/*!
details
*/
class PHKeyList : public TObject
{
public:

  typedef Key key_type;
  typedef std::vector<key_type> key_list_type;

  //! default constructor
  PHKeyList()
  {}

  //! virtual destructor
  virtual ~PHKeyList()
  {}

  //! get a read only list of this objects forward keys
  const key_list_type& get_keys() const
  {return _key_list;}

  //! add object in_key to forward keys
  void add_key(const Key& in_key)
  { _key_list.push_back(in_key); }

  //! return number of keys in key list
  size_t size() const
  { return _key_list.size(); }

  //! remove key in_key_value from forward keys
  void remove_key(key_type in_key_value)
  { _key_list.erase(std::remove(_key_list.begin(), _key_list.end(), in_key_value), _key_list.end()); }

  //! Print key list
  void print(std::ostream& os = std::cout) const;

private:

  //! list of associated keys
  std::vector<Key> _key_list;

  ClassDef(PHKeyList,1)

};

/*! \ingroup classes */

//! PHKey provides key interface
/*!
PHKey maintains an instance of type Key to locate the this object
or a derived object.
*/

/*
  PHKey implements the
  the bulk of the associated object technology used in MUTOO.    It maintains
  3 collections of associated keys.	   It has a std::vector of Key objects that
  that is root streamable and represents the master keys list that is made
  persistent. It also has a std::list of shared pointers to associated
  objects that is used to provide access to the objects via the get_associated<>
  functions, it also has a std::list of PHKey* used to remove *this* object
  from associated objects keys lists in the event that *this* object is destroyed.
  The degeneracy of key containers is necessary because the requirements for
  persistence, typed access and untyped access have different levels of awareness
  of the derived types.
*/

class PHKey : public TObject
{
  public:

  typedef Key key_type;
  typedef Key::object_key_type object_key_type;
  typedef Key::map_key_type map_key_type;

  #ifndef __CINT__
  typedef std::multimap< PHClassId::id_type, boost::any > AssociateMap;
  typedef std::list< PHKey* > AssociateKeyList;
  typedef std::set< PHClassId::id_type > SynchroSet;
  typedef AssociateMap::iterator associate_iterator;
  typedef AssociateMap::const_iterator const_associate_iterator;
  #endif

  //! default constructor
  PHKey() : _synchro_flag(0)
  {}

  //! construct with Key
  PHKey(const Key& in_key) :
      _key(in_key),
      _synchro_flag(0)
  {}

  //! virtual destructor
  virtual ~PHKey()
  {}

  //! set this objects key value
  void set_key(const key_type& key)
  {_key = key;}

  //! get this objects key value
  key_type get_key() const
  {return _key;}

  //! print the key
  virtual void print(std::ostream& os = std::cout) const
  { _key.print(os); }

  //! list of associated keys
  const PHKeyList& get_key_list() const
  {return _key_list;}

  //! list of associated keys
  PHKeyList& get_key_list()
  {return _key_list;}

  #ifndef __CINT__

  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return PHClassId::get( GetName() ); }

  //! Associate two objects
  template<typename T, typename M> static void associate(boost::shared_ptr<T>&,  boost::shared_ptr<M>&);

  //! Associate two objects
  template<typename T, typename M> static void associate_unique(boost::shared_ptr<T>&, boost::shared_ptr<M>&);
  //! Associate two objects
  template<typename T, typename M> static void associate_unique(boost::shared_ptr<T>*, boost::shared_ptr<M>*);

  //! Associate two objects
  template<typename T, typename M> static void associate(boost::shared_ptr<T>*, boost::shared_ptr<M>*);

  //! Remove an association between two objects
  template<typename T, typename M> static void disassociate(boost::shared_ptr<T>&, boost::shared_ptr<M>&);

  //! Remove an association between two objects
  template<typename T, typename M> static void disassociate(boost::shared_ptr<T>*, boost::shared_ptr<M>*);

  //! used to find object with matching key in list
  template<typename T>
  class SameKeyFTor
  {
    public:

    //! constructor
    SameKeyFTor( const key_type key ):
      _key( key )
    {}

    //! predicate
    bool operator() ( const AssociateMap::value_type& value ) const
    {
      	typedef boost::shared_ptr<T> value_type;
        return (boost::any_cast<value_type>(value.second)).get()->get_key() == _key; }

    private:

    //! predicted key
    key_type _key;

  };

  //! Get an iterator to associated objects of type T
  template<typename T> PHKeyIterator<T> get_associated();

  //! Get an constant iterator to associated objects of type T
  template<typename T> PHConstKeyIterator<T> get_associated() const;

  //! dump associations
  template<typename T> void dump_associations( void ) const;

  //! Return true if this key is associate to argument object
  template<typename T> bool is_associated( const boost::shared_ptr<T>* ) const;

  //! Remove an associated key from key list
  template<typename T> void remove_associated_key(boost::shared_ptr<T>*);


  void clear_associations()
  {
    // Clear forward associations, note this method does not clear the
    // reflexive associations.
    //
    _associate_map.clear();
    _associate_key_list.clear();
  }

  //! Clear all forward and backward associations
  template<typename T>
  void remove_associations(boost::shared_ptr<T>* ptr)
  {
    // Forward association
    //
    _associate_map.erase(_associate_map.begin(),_associate_map.end());

    // Loop over all associated objects and remove this object from their
    // key list
    //
    AssociateKeyList::iterator iter;
    for(iter = _associate_key_list.begin(); iter != _associate_key_list.end(); ++iter){
      (*iter)->remove_associated_key<T>(ptr);
    }
  }

  //! Synchro flag indicates local map of pointers and key list are not synchronized
  void set_synchro_flag()
  { _synchro_flag=1; }

  //! Synchro flag indicates local map of pointers and key list are not synchronized
  void unset_synchro_flag()
  { _synchro_flag=0; }

  //! Synchro flag indicates local map of pointers and key list are not synchronized
  UShort_t get_synchro_flag() const
  { return _synchro_flag;}

  //! binary functo to see if keys have associated objects of type T
  template<typename T> class has_associates_ftor
  {
    public:

    //! predicate
    bool operator() (const PHKey& key ) const
    { return key.get_associated<T>().count(); }

  };

  #endif

  private:

  //! This object keys
  key_type _key;

  //! List of associated keys
  PHKeyList _key_list;

  /*!
    Synchro Flag, indicates if this objects associate_map and
    associate list are potentially unsynchronized.
  */
  UShort_t _synchro_flag;

  #ifndef __CINT__

  /*!
    Local cache for object any pointers that are associated
    (we only do the actual map look via associated key once)
  */
  AssociateMap _associate_map;

  /*!
    Local cache of associated PHKey*.
    Storing these is necessary to clear
    associations when objects are erased.
  */
  AssociateKeyList _associate_key_list;

  /*!
    keep track of which types have been lookup up via associated key.
    That is is the master key list (the persistent thing) consistent with
    the associate map.
  */
  SynchroSet _synchro_set;

  /*!
    Sync key_list and associate map.	That is attempt, to re-establish
    the associations after reading back this object from persistent
    storage
  */
  template<typename T> void synchronize_key_list();

  //! Reverse association.
  template<typename T, typename M> void static reflexive_associate(boost::shared_ptr<T>*, boost::shared_ptr<M>*);
  #endif

  ClassDef(PHKey,1)
};

//
// Member function template definitions
//
#ifndef __CINT__

//__________________________________________________________________
template<typename T,typename M>
void PHKey::associate_unique(boost::shared_ptr<T>& fwd_ref,
 boost::shared_ptr<M>& bwd_ref)
{
  PHKeyList::key_list_type::const_iterator iter = bwd_ref.get()->_key_list.get_keys().begin();

  // If match is found then punt without associating
  for(;iter!=bwd_ref.get()->_key_list.get_keys().end();++iter)
  if(fwd_ref.get()->get_key() == *iter) return;

  PHKey::associate(fwd_ref, bwd_ref);
}

//__________________________________________________________________
template<typename T,typename M>
void PHKey::associate_unique(boost::shared_ptr<T>* fwd_ptr,
 boost::shared_ptr<M>* bwd_ptr)
{
  PHKeyList::key_list_type::const_iterator iter = bwd_ptr->get()->_key_list.get_keys().begin();

  // If match is found then punt without associating
  for(;iter!=bwd_ptr->get()->_key_list.get_keys().end();++iter)
  if(fwd_ptr->get()->get_key() == *iter) return;

  PHKey::associate(fwd_ptr, bwd_ptr);
}

//__________________________________________________________________
template<typename T,typename M>
void PHKey::associate(boost::shared_ptr<T>& fwd_ref, boost::shared_ptr<M>& bwd_ref)
{

//   std::cout
//     << "PHKey::associate - ref - "
//     << " fwd: " << fwd_ref.get()->get_key().get_obj_key()
//     << " bwd: " << bwd_ref.get()->get_key().get_obj_key()
//     << std::endl;

  // Check for synchronziation before making new association
  if(bwd_ref.get()->get_synchro_flag()) bwd_ref.get()->template synchronize_key_list<T>();
  if(fwd_ref.get()->get_synchro_flag()) fwd_ref.get()->template synchronize_key_list<M>();

  // append key to persistent key list
  bwd_ref.get()->_key_list.add_key(fwd_ref.get()->get_key());

  // append PHKey pointer to local list
  bwd_ref.get()->_associate_key_list.push_back(fwd_ref.get());

  // append object ref pointer local multimap
  bwd_ref.get()->_associate_map.insert( std::make_pair( T().get_class_id(),fwd_ref));

  // establish the reverse association
  PHKey::reflexive_associate(&bwd_ref,&fwd_ref);
}

//__________________________________________________________________
template<typename T, typename M>
void PHKey::associate(boost::shared_ptr<T>* fwd_ptr, boost::shared_ptr<M>* bwd_ptr)
{

//   std::cout
//     << "PHKey::associate - pointer - "
//     << " fwd: " << fwd_ptr->get()->get_key().get_obj_key()
//     << " bwd: " << bwd_ptr->get()->get_key().get_obj_key()
//     << std::endl;

  // Check for synchronziation before making new association
  if(bwd_ptr->get()->get_synchro_flag()) bwd_ptr->get()->template synchronize_key_list<T>();
  if(fwd_ptr->get()->get_synchro_flag()) fwd_ptr->get()->template synchronize_key_list<M>();

  // append key to persistent key list
  bwd_ptr->get()->_key_list.add_key(fwd_ptr->get()->get_key());

  // append PHKey pointer to local list
  bwd_ptr->get()->_associate_key_list.push_back(fwd_ptr->get());

  // append object ref pointer local multimap
  bwd_ptr->get()->_associate_map.insert(std::make_pair( T().get_class_id() ,*fwd_ptr));

  // establish the reverse association
  PHKey::reflexive_associate(bwd_ptr,fwd_ptr);
}

//__________________________________________________________________
template<typename T,typename M>
void PHKey::disassociate(boost::shared_ptr<T>& fwd_ref, boost::shared_ptr<M>& bwd_ref)
{ disassociate(&fwd_ref,&bwd_ref); }

//__________________________________________________________________
template<typename T, typename M>
void PHKey::disassociate(boost::shared_ptr<T>* fwd_ptr, boost::shared_ptr<M>* bwd_ptr)
{
  // remove forward and backwards associations
  bwd_ptr->get()->remove_associated_key(fwd_ptr);
  fwd_ptr->get()->remove_associated_key(bwd_ptr);
}

//__________________________________________________________________
template<typename T, typename M>
void PHKey::reflexive_associate(boost::shared_ptr<T>* bwd_ptr, boost::shared_ptr<M>* fwd_ptr)
{
  // append key to persistent key list
  fwd_ptr->get()->_key_list.add_key(bwd_ptr->get()->get_key());

  // append PHKey pointer to local list
  fwd_ptr->get()->_associate_key_list.push_back(bwd_ptr->get());

  // append object ref pointer local multimap
  fwd_ptr->get()->_associate_map.insert(std::make_pair( T().get_class_id(),*bwd_ptr));
}

//__________________________________________________________________
// When objects are read back from persistent storage. The key_list
// and associate_map are not synchronized.	  method reads from
// the key list and populates the associate map by looking up object
// with keys specified in key_list in the appropriate map and storing
// the resulting pointers in the associate map.
template<typename T>
void PHKey::synchronize_key_list()
{

  NonStaticMapFinder< PHMapBaseT<Key,T>*> finder;

  // Check to see if already synchronized for this type
  if(_synchro_set.count( T().get_class_id() )) return;

  // iterate through key list
  for(
    PHKeyList::key_list_type::const_iterator key_iter = _key_list.get_keys().begin();
    key_iter!=_key_list.get_keys().end();
    ++key_iter)
  {

    map_key_type map_key = key_iter->get_map_key();

    // look up the associated map in the map manager
    try {

      //PHMapBaseT<Key,T>* map_ptr = MapFinder< PHMapBaseT<Key,T>* >::get(map_key);
      PHMapBaseT<Key,T>* map_ptr = finder.get(map_key);

      // Note -- handling missing map with an exception generated illegal instruction
      // in g++ 2.95.3 in optimized code -- Now we must explicitly check the return
      // of the map.
      if(!map_ptr) continue;

      boost::shared_ptr<T> ptr = map_ptr->private_find(*key_iter);
      if(ptr.get())
      {
        _associate_map.insert(std::make_pair( T().get_class_id(), ptr ));
        _associate_key_list.push_back(ptr.get());
      }

    } catch(std::exception& e) {

      // We get here if the map doesn't exist (wasn't written out) *or*
      // the current key_list entry is associated with type T. In other
      // words the any_cast in MapFinder failed. In either case we just
      // move on.

    }
  }
  // Synchronized
  _synchro_set.insert( T().get_class_id() );
}

//__________________________________________________________________
template<typename T> PHKeyIterator<T> PHKey::get_associated()
{

  // Synchronize master key list with associate map.
  if( get_synchro_flag() ) synchronize_key_list<T>();

  // name for range of AssociateMap iterators
  typedef std::pair<associate_iterator,associate_iterator> Range;

  // find range of keys associated with this type
  Range range = _associate_map.equal_range( T().get_class_id() );

  // construct a PHKeyIterator with range
  return PHKeyIterator<T>(range);

}

//__________________________________________________________________
template<typename T> PHConstKeyIterator<T> PHKey::get_associated() const
{

  // check to see if key_list and map are synchronized
  if( get_synchro_flag() )
  {
    // Not as evil as it seems.	The const here applies to the
    // abstract state of the object.	Sychronizing the
    // key list doesn't change the abstract state of this
    // so we choose to allow this modification to preserve the
    // ability to present a const interface to associated objects.
    const_cast<PHKey*>(this)->synchronize_key_list<T>();
  }

  // name for range of AssociateMap iterators
  typedef std::pair<const_associate_iterator, const_associate_iterator> Range;

  // find range of keys associated with this type
  Range range = _associate_map.equal_range( T().get_class_id() );

  // construct a PHKeyIterator with range
  return PHConstKeyIterator<T>(range);
}

//__________________________________________________________________
template<typename T> void PHKey::dump_associations() const
{

  if( get_synchro_flag() ){ const_cast<PHKey*>(this)->synchronize_key_list<T>(); }

  // name for range of AssociateMap iterators
  typedef std::pair<const_associate_iterator, const_associate_iterator> Range;
  Range range = _associate_map.equal_range( T().get_class_id() );

  for( const_associate_iterator iter = range.first; iter != range.second; iter++ )
  {
    typedef boost::shared_ptr<T> value_type;
    value_type ptr = boost::any_cast<value_type>( iter->second );
    std::cout
      << T().GetName()
      << " map key: " << ptr.get()->get_key().get_map_key()
      << " object key: " << ptr.get()->get_key().get_obj_key()
      << std::endl;

  }

  return;

}

//__________________________________________________________________
template<typename T> bool PHKey::is_associated( const boost::shared_ptr<T>* ptr ) const
{

  // check to see if key_list and map are synchronized
  if( get_synchro_flag() ){

    // Not as evil as it seems.	The const here applies to the
    // abstract state of the object.	Sychronizing the
    // key list doesn't change the abstract state of this
    // so we choose to allow this modification to preserve the
    // ability to present a const interface to associated objects.
    const_cast<PHKey*>(this)->synchronize_key_list<T>();

  }

  // name for range of AssociateMap iterators
  typedef std::pair<const_associate_iterator, const_associate_iterator> Range;

  // find range of keys associated with this type
  Range range = _associate_map.equal_range( T().get_class_id() );

  // return true if could find ptr key in range
  return std::find_if( range.first, range.second, SameKeyFTor<T>( ptr->get()->get_key() ) ) != range.second;

}

//__________________________________________________________________
template<typename T> void PHKey::remove_associated_key(boost::shared_ptr<T>* ptr)
{
  // remove from the master key list (vector of type Key)
  //
  _key_list.remove_key(ptr->get()->get_key());

  {
    // remove from the list of PHKey pointers
    AssociateKeyList::iterator iter;
    for(iter=_associate_key_list.begin(); iter!=_associate_key_list.end(); ++iter)
    if ((*iter)->get_key() == ptr->get()->get_key())
    iter = _associate_key_list.erase(iter);

  }

  // remove from the associate map
  {
    typedef std::pair<associate_iterator,associate_iterator> Range;
    Range range = _associate_map.equal_range( T().get_class_id() );
    associate_iterator iter;
    for(iter=range.first; iter!=range.second; ++iter)
    {

      // Cast the any pointer to a ref pointer to object of type T
      boost::shared_ptr<T> obj_rptr = boost::any_cast< boost::shared_ptr<T> >(iter->second);

      // If T's key matches then erase from associate map
      if (obj_rptr->get_key() == ptr->get()->get_key())
      {
        _associate_map.erase(iter);
        break;
      }
    }
  }
}


#endif


#endif
