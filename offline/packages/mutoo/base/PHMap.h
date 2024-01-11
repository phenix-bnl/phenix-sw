//////////////////////////////////////////////////////////////////
//
// Utility class: PHMap:
// Author: S.Kelly/H.Pereira
// Date: 1/07/01
// Description:
//
//////////////////////////////////////////////////////////////////

#ifndef __PHMap_H__
#define __PHMap_H__

// STL/BOOST headers
//
#include<map>
#include<boost/weak_ptr.hpp>

// MUTOO headers
//
#include "PHMapIterator.h"
#include "PHConstMapIterator.h"
#include "PHMapBase.h"
#include "PHMapBaseT.h"
#include "TMutMapIO.h"
#include "TMutNode.h"
#include "PHIOArray_v1.hh"

// ROOT/PHOOL headers
//
#include<TClonesArray.h>
#include<PHCompositeNode.h>
#include<PHObject.h>
#include<PHIODataNode.h>

// forward declarations
//
template<typename Value> class PHKeyIterator;
template<typename Value> class PHConstKeyIterator;

/*! @defgroup classes Class Library
  The class library contains classes with MUTR specific utility
  aswell as more general utility classes. Non detector specific
  class are prefixed with a PH, Muon tracker specific class are
  prefixed with a TMut. Anything that is not an interface object,
  container, or analysis module properly belongs in this group.

*/

/*! \ingroup classes */

//! PHMap maps type Key onto type Value.

/*!
PHMap implements a mapping from type Key to type Value with support for
iterators that model STL forward-iteration and ROOT style iteration
with a next() method and iterator bounds. PHMap is an ordered
container that uses less<Key> for ordering. This class is a
building block for detector specific containers. The important
aspects of the public interface are denumerated below.<br>
<ul>
<li> PHMaps are required to be heap allocated (protected destructor idiom).
When instantiated PHMaps automatically are assigned a unique key and
register themselves with the statically scoped PHMapManager.
PHMapManager maintains ownership and is responsible for resource deallocation.
<strong>It is *never* safe to delete a pointer to a PHMap.</strong>

<li> PHMap is an ordered container. Access to a range of elements in the map
can be requested by specifing the appropriate key range to the find
function. Practically speaking this means that a detector specific container
class derived from PHMap can order keys such that all objects associated with
a section of a detector are contiguous and can be looped over by specifing
an appropriate range of keys.

<li> Access to map Value types is via bounded iterators. Bounded iterators
are described in the PHMapIterator PHConstMapIterator documentation. Simply
stated iterators provide a safe interface for looping over a range of objectso
in a PHMap. Iterators are "safe" in the sense that they are aware of the
range over which they are valid, maintain ref counted ownership semantics,
provide const and non-const access to PHMap Value types, and are protected
against gross acts of software malfeasance (example: de-referencing an out of range
iterator) by exceptions.

<li> Value types in PHMap are held by reference counted pointer.  Reference counted
pointers are discussed in technical notes, practically speaking users
can safely insert heap allocated pointers to Value types or references
to Value types.  The former case is more efficient as it does not require
the creation of a temporary. Upon insertion ownership of map Value types
obeys reference counted semantics. That is resources associated with
the Value type are automatically deallocated when the last ref pointer
goes out of scope. <strong> It is *never* safe to delete a pointer to
a heap allocated Value type that has been inserted into a PHMap via that
pointer</strong>

<li> PHMaps can map their contents onto a TClonesArray or TObjArray for
ROOT persistent I/O.
</ul>
*/

template<typename Key, typename Value, typename ValueImp=Value>
class PHMap : public PHMapBaseT<Key,Value>
{
  public:

  //! shortcut to content shared_pointer
  typedef boost::shared_ptr<Value> value_type;

  //! shortcut to pointer to content shared_pointer
  typedef value_type* pointer;
  
  //! shortcut to const pointer to content shared_pointer
  typedef const value_type* const_pointer;
  
  //! shortcut to reference to content shared_pointer
  typedef value_type& reference;
  
  //! shortcut to constant reference to content shared_pointer
  typedef const value_type& const_reference;
  
  //! shortcut to iterator over content
  typedef PHMapIterator<Key,Value> iterator;
  
  //! shortcut to iterator over content (sorted by key)
  typedef PHKeyIterator<Value> key_iterator;
  
  //! shortcut to iterator over constant content
  typedef PHConstMapIterator<Key, Value > const_iterator;
  
  //! shortcut to iterator over constant content (sorted by key)
  typedef PHConstKeyIterator< Value > const_key_iterator;
  
  //! shortcut current content implemented version
  typedef ValueImp value_imp_type;

  //! shortcut to persistant array base class
  typedef PHIOArray array_type;

  //! shortcut to persistant array implemented version
  typedef PHIOArray_v1 array_imp_type;
  
  //! @name Constructors/Destructors
  //@{

  //! DefaultConstructor 
  PHMap();

  //! Construct with provided key 
  PHMap(ULong_t map_key);

  //! Virtual 
  virtual ~PHMap()
  {}
  
  //@}

  //! @name Accessors
  //@{
  //! Returns a const_iterator to all keys in map 
  const_iterator range() const;

  //! Returns an iterator to all keys in map 
  iterator range();

  /*! \brief
    Returns an iterator with bounds given by the
    key interval [lower, lower+1). The value of the
    iterator is the element with key lower.
  */
  iterator find(const Key& lower);

  /*! \brief
    Returns an const_iterator with bounds given by the
    key interval [lower, lower+1). The value of the
    iterator is the element with key lower.
  */
  const_iterator find(const Key& lower) const;

  /*! \brief
    Returns an iterator with bounds given by the
    key interval [lower, upper). The value of the
    iterator is the element with key lower.
  */
  iterator find(const Key& lower, const Key& upper);

  /*! \brief
    Returns an const_iterator with bounds given by the
    key interval [lower, upper). The value of the
    iterator is the element with key lower.
  */
  const_iterator find(const Key& lower, const Key& upper) const;
  //@}

  //! @name Clear/Insert/statistics
  //@{
  //! Insert object Key Value pair into the map. 
  void insert(const Key& ,const Value&);

  //! Insert object Key Value pair into the map. 
  void insert(const Key& , Value*);

  //! Check if object map key is equal to this map key 
  bool check_map_key( const Key& );

  //! Erase entry with given key 
  bool erase(const Key&);

  //! Erase all entries in the map 
  virtual void clear();

  //! store map statistics 
  virtual void update_statistics();

  //@}

  //! @name I/O Methods
  //@{
  //! Make this map persistent 
  virtual void make_persistant(PHCompositeNode* dst_node, const std::string& dst_name);

  //! Set output state 
  void set_writable(bool is_writeable)
  {
    _is_writeable = is_writeable;
    if(_is_writeable && !_io_utility.is_initialized()){
      _io_utility.initialize();
    }
  }

  //! Ouput has been initialized 
  virtual bool is_writeable() const
  { return _is_writeable;}

  //! Input  has been initialized 
  virtual bool is_readable() const
  { return _is_readable;}

  //! Set input state 
  void set_readable( const bool& is_readable)
  { _is_readable = is_readable; }

  //! Write Contents of this map to PHIOArray 
  virtual void write_array();

  //! Populate this map to by reading from PHIOArray on DST node 
  virtual void read_array(PHCompositeNode* dst_node);

  //! Clear Input/Output array 
  virtual void clear_array()
  { if(is_writeable()) _io_utility.clear(); }

  //! Set name of IO map 
  void set_io_map_name(const std::string& io_map_name)
  { _io_map_name = io_map_name; }

  //! Get this maps IO array 
  TClonesArray* get_array()
  { return _io_utility.get_array(); }

  //! Set this maps IO array 
  void set_array(TClonesArray* io_array)
  { _io_utility.set_array(io_array);}

  //@}

  //! @name Dumpers
  //@{
  //! returns name of the map 
  virtual std::string get_name() const
  { return std::string(ValueImp().GetName())+ " Map"; }

  //! current size
  /*! \brief
    this method is called "count" rather than
    size so that it can be called from the base class PHMapBase
  */
  virtual size_t count() const
  { return size(); }

  //! returns total number of stored object
  virtual size_t get_accumulated_size() const
  { return _accumulated_size; }

  //! returns total number of cycles
  virtual size_t get_ncycle() const
  { return _ncycle; }

  //! Dump contents of map to specified stream
  virtual void print( std::ostream& os = std::cout) const;

  //! Dump statistics of map to specified stream
  virtual void print_stat( std::ostream& os = std::cout ) const
  {
    std::string value_name(ValueImp().GetName());
    MUTOO::PRINT(os, value_name + " Map statistics" );
    os << " accumulated size: " << _accumulated_size << std::endl;
    os << " per cycle size: " << double(_accumulated_size)/_ncycle << std::endl;
    MUTOO::PRINT(os,"**");
  }

  //! Number of entries in this map
  size_t size() const
  {return _map.size();}

  //! True if map is empty
  bool empty() const;
  
  /*! \brief
    PHMaps are affored the limited ability to check for resource
    leaks due to circular references among ref pointers. What is
    indicated to via this call is the number of outstanding ref
    pointers to objects contained in this map. If the number is
    not zero after a call to map clear this is indicative of a
    potential resourse leak. We use boost weak pointer to keep
    track of outstanding ref pointers (these do not contribute
    to the ref count)<br><br>
    Dump resource use stats to specified ostream.
  */
  void print_leak_stats( std::ostream& os = std::cout )
  {
    typename leak_check_vector::const_iterator iter = _leak_check.begin();
    MUTOO::PRINT(os, "Resource Statistics");
    os << " type: " << ValueImp().GetName() << std::endl;
    os << " usage: " << _leak_check.size() << std::endl;
    int leaked=0;
    for(;iter!=_leak_check.end();++iter){
      leaked += iter->use_count();
    }

    os << " ref count: " << leaked << std::endl;
    MUTOO::PRINT(os, "**");
    _leak_check.clear();
  }

  //! Print contents of Input/Ouput array
  virtual void print_array( std::ostream& os = std::cout ) const
  {_io_utility.print(os);}

  //@}

  //! input/output interface to read/write persistent map contents from/to TClonesArray
  TMutMapIO<Value,ValueImp>& get_io_utility()
  { return _io_utility; }

  protected:

  //! Set output status flag 
  void set_writeable( const bool& is_writeable)
  {
    _is_writeable = is_writeable;
    if(_is_writeable && !_io_utility.is_initialized()){
      _io_utility.initialize();
    }
  }
  
  private:

  friend class PHKey;

  //! implements PHMapBaseT private find
  value_type private_find(const Key&);

  //! content map
  typedef std::map< Key, boost::shared_ptr<Value> > private_map;
  
  //! iterator over the private map
  typedef typename private_map::iterator private_iterator;

  //! const iterator over the private map
  typedef typename private_map::const_iterator const_private_iterator;

  typedef std::vector< boost::weak_ptr<Value> > leak_check_vector;
  
  //! private content map
  private_map _map;

  //! weak pointer to check for resource leaks
  leak_check_vector _leak_check;

  //! name of the IO map
  std::string _io_map_name;
  
  //! for persistant IO
  array_type* _io_array;

  //! for persistant IO
  TMutMapIO<Value,ValueImp> _io_utility;

  //! true if map is persistant
  bool _is_writeable;

  //! true if map is read from input
  bool _is_readable;

  //! total number of objects stored in this map (updated at every clear())
  size_t _accumulated_size;

  //! total number of cycles (clear())
  size_t _ncycle;
};

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
PHMap<Key,Value,ValueImp>::PHMap() :
  _io_array(0),
  _is_writeable(false),
  _is_readable(false),
  _accumulated_size(0),
  _ncycle(0)
  {}


//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
PHMap<Key,Value,ValueImp>::PHMap(ULong_t key) :
  PHMapBaseT<Key,Value>(key),
  _io_array(0),
  _is_writeable(false),
  _is_readable(false),
  _accumulated_size(0),
  _ncycle(0)
  {}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::iterator
PHMap<Key,Value,ValueImp>::find(const Key& key)
{ return find(key,key); }

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::const_iterator
PHMap<Key,Value,ValueImp>::find(const Key& key) const
{ return find(key,key); }

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::iterator
PHMap<Key,Value,ValueImp>::find(const Key& lower, const Key& upper)
{
  private_iterator lower_bound = _map.lower_bound(lower);
  return PHMapIterator<Key,Value>(lower_bound, lower_bound, _map.upper_bound(upper));
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::const_iterator
PHMap<Key,Value,ValueImp>::find(const Key& lower, const Key& upper) const
{
  const_private_iterator lower_bound = _map.lower_bound(lower);
  return PHConstMapIterator<Key,Value>(lower_bound, lower_bound, _map.upper_bound(upper));;
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::iterator PHMap<Key,Value,ValueImp>::range()
{
  return PHMapIterator<Key,Value>(_map.begin(), _map.begin(), _map.end());
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::const_iterator PHMap<Key,Value,ValueImp>::range() const
{
  return PHConstMapIterator<Key,Value>(_map.begin(), _map.begin(), _map.end());
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::insert(const Key& key, const Value& value)
{
  boost::shared_ptr<Value> refptr(new Value(value));
  _map.insert( std::make_pair(key,refptr) );
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::insert(const Key& key, Value* ptr)
{
  boost::shared_ptr<Value> refptr(ptr);
  _map.insert( std::make_pair(key,refptr) );
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
bool PHMap<Key,Value,ValueImp>::check_map_key(const Key& key)
{ return key.get_map_key() == this->get_map_key(); }

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
inline bool PHMap<Key,Value,ValueImp>::erase(const Key& key){

  iterator iter = find(key);

  // Remove all associations
  if(!iter.at_end()) iter->get()->remove_associations(iter.current());
  return _map.erase(key);

}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::clear()
{

  // First clear forward associations
  iterator iter = range();
  while(pointer ptr = iter.next()){
    ptr->get()->clear_associations();
  }
  
  // Clear the entries in the map
  _map.erase(_map.begin(),_map.end());
  
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::update_statistics()
{
  // update ncycle and accumulated size
  _accumulated_size+=size();
  _ncycle++;
}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
bool PHMap<Key,Value,ValueImp>::empty() const
{ return _map.empty(); }

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::print( std::ostream& os ) const
{
  std::string value_name(ValueImp().GetName());
  value_name += " Map";
  MUTOO::PRINT(os,value_name.c_str());
  os << "map key: " << this->get_map_key() << std::endl;
  os << "map entries: " << size() << std::endl;
  const_iterator iter = range();
  while(const_pointer ptr = iter.next()){
    ptr->get()->print(os);
  }
  MUTOO::PRINT(os,"End Map Dump");
}


//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::write_array()
{

  // clear io
  _io_utility.clear();

  // do nothing if map is empty
  if( empty() ) return;
  
  // copy contents in the IO interface
  iterator iter = range();
  while(pointer ptr = iter.next()) _io_utility.insert(ptr->get());

}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::make_persistant(PHCompositeNode* dst_node, const std::string& name)
{

  // do nothing if map is already writable
  if( is_writeable() )
  {
    std::cout << "PHMap::make_persistant - map name " << name << " already made persistent. Doing nothing." << std::endl;
    return;
  }
  
  // first check if a node is not already found with the correct name
  PHNodeIterator iter(dst_node);
  PHIODataNode<PHObject>* old_node =  static_cast< PHIODataNode<PHObject>* >(iter.findFirst("PHIODataNode",name.c_str()));

  // if found rename it, since we want a new node to be written
  if( old_node ) {
    std::cout << "PHMap::make_persistant - found an io_array named " << name << ". Renaming." << std::endl;
    old_node->setName( (name+"_old").c_str() );
  }
  
  // create a new PHIOArray coupled to PHMap's io array
  _io_array = new array_imp_type( get_array(), this->get_map_key() );
  PHIODataNode<PHObject>* io_node = new PHIODataNode<PHObject>( _io_array, name.c_str() );

  // Append to DST node
  dst_node->addNode(io_node);

  // tells map it must be written to the DST node
  set_writeable(true);
  return;

}

//________________________________________________________
template <typename Key, typename Value, typename ValueImp>
void PHMap<Key,Value,ValueImp>::read_array(PHCompositeNode* dst_node)
{

  // Clear this map
  clear();

  // Get the PHIOArray from the node tree
  PHIOArray* io_array = TMutNode<PHIOArray>::find_io_node(dst_node,_io_map_name.c_str());

  // Get the TClonesArray of interface objects from the PHIOArray
  TClonesArray* clones_array = io_array->get_array();

  // Fill the IOC from the clones array
  for(int i = 0; i<clones_array->GetEntries();++i)
  {
    Value* obj = static_cast<Value*>(clones_array->At(i));

    // check object key against map
    if( !check_map_key( obj->get_key() ) )
    {

      std::cout
        << "PHMap::read_array - " << get_name()
        << " : changing map key"
        << " from " << this->get_map_key()
        << " to " << obj->get_key().get_map_key()
        << std::endl;

      this->change_map_key( obj->get_key().get_map_key() );

    }

    // construct the implementation from base value_imp_type(value_type*)
    insert(obj->get_key(), new value_imp_type(*obj));

  }

}

//________________________________________________________
// Private find return a value_type directly.  This
// is used by PHKey to re-establish associations between
// interface objects without having to make PHKey aware
// of the iterator interface
//
template <typename Key, typename Value, typename ValueImp>
typename PHMap<Key,Value,ValueImp>::value_type
PHMap<Key,Value,ValueImp>::private_find(const Key& key)
{
  iterator iter = find(key,key);
  if(iter.at_end()) return value_type();
  else return *iter;
}

#endif




