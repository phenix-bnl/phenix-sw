#ifndef __PHMAPMANAGER_H__
#define __PHMAPMANAGER_H__

// $Id: PHMapManager.h,v 1.9 2010/06/15 18:47:13 hpereira Exp $

/*!
\file PHMapManager.h
\brief static storage of registered PHMap
\author Sean Kelly, Hugo Pereira
\version $Revision: 1.9 $
\date $Date: 2010/06/15 18:47:13 $
*/

#include "PHException.h"
#include "MUTOO.h"

#ifndef __CINT__
#include "MUTOO_HASH_MAP.h"
#include <boost/any.hpp>
#include <boost/smart_ptr.hpp>
#endif //__CINT__

#include <PHCompositeNode.h>
#include <set>

#ifndef __CINT__
#include "PHMapBase.h"
#else
class PHMapBase;
#endif
// class PHMapBase;

/*! \ingroup classes */
//! Static class for managing PHMaps

/*!
PHMapManager proveds three essential pieces of functionality.
It owns all PHMaps and derived types and it alone can cleanup
associated resources.	It provides keyed lookup of PHMaps in
static scope.	This functionality is used by PHKeyIterator and
PHConstKeyIterator to access the maps they need to iterate over.
It also provides a write method that loops on PHMaps that have
their persistant flag set and calls the PHMap::write() for each
such instance.	PHMapManagers thus provides access, resource
management, and persistent IO for PHMaps and derived types.
Developers note: PHMap objects can only be heap allocated
(stack allocated PHMaps will fail at compile time).	Upon
instantiation the map becomes the property of the manager hence
one should never call operator delete with a PHMap pointer as
an arguement.
*/

class PHMapManager
{
  public:

  #ifndef __CINT__

  typedef PHMapBase::key_type key_type;

  /*!
  Returns a reference to PHMap with given key or return default any
  if no map is associated with given key
  */
  static boost::any get(key_type key);

  //! Register provided map
  template<typename T> static void register_map(T*);

  //! Unregister provided map
  template<typename T> static void unregister_map(T*);

  //! change map key for provided map
  template<typename T> static void change_map_key(T*, key_type);

  #endif //__CINT__

  //! Returns the number of registered maps in manager
  static unsigned short size() { return map().size(); }

  //! Iterates over persistent maps, calls their write_array methods
  static void write();

  //! Iterates over maps coupled to an input dst, calls their read_array methods
  static void read(PHCompositeNode* dst_node);

  //! Dump contents of persistent map input/output buffers
  static void print_array(std::ostream& os = std::cout);

  //! Clear entries from all managed maps
  static void clear();

  //! Clear arrays for all managed maps
  static void clear_arrays();

  //! dump all map keys
  inline static void dump_map_keys( void );

  //! disable map clearing
  static void disable_clear()
  {_disable_clear=true;}

  //! enable map clearing
  static void enable_clear()
  {_disable_clear=false;}

  //! verbosity
  static void set_verbosity( const MUTOO::Verbosity& verbosity )
  { _verbosity = verbosity; }

  //! verbosity
  static const MUTOO::Verbosity& get_verbosity( void )
  { return _verbosity; }

  //! prints all managed maps statistics
  static void print_stat(std::ostream& os = std::cout);

  private:

  #ifndef __CINT__
  //! shortcut to store maps, key association
  typedef MUTOO::hash_map<key_type,boost::any>::type PrivateMap;

  //! shortcut to store set of maps
  typedef std::set<PHMapBase*> PrivateSet;

  //! iterator over maps
  typedef PrivateMap::iterator private_map_iterator;

  //! iterator over maps
  typedef PrivateSet::iterator private_set_iterator;

  //! all registered map coupled with their unique key
  static PrivateMap& map();

  //! set of registered maps
  static PrivateSet& set();
  #endif //__CINT__

  //! when true, PHMapManager does nothing
  static bool _disable_clear;

  //! verbosity
  static MUTOO::Verbosity _verbosity;

};


#ifndef __CINT__
//_______________________________________________
template<typename T>
  void PHMapManager::register_map(T* value)
{
  key_type key = value->get_map_key();
  if(map().find(key) != map().end()){
    throw std::invalid_argument(DESCRIPTION("This key/map association already exists"));
  }
  map().insert(std::make_pair(key,value));
  set().insert(value);
}

//_______________________________________________
template<typename T>
  void PHMapManager::unregister_map(T* value)
{
  key_type key = value->get_map_key();
  PrivateMap::iterator iter = map().find(key);
  if(iter == map().end()){
    throw std::invalid_argument(DESCRIPTION("This key/map association does not exists"));
  }
  map().erase(iter);
}

//_______________________________________________
template<typename T>
  void PHMapManager::change_map_key(T* value, key_type new_key)
{

  // check if new_key is in the list
  if( map().find(new_key) != map().end() )
    throw std::invalid_argument(DESCRIPTION("The new key is already in the map"));

  // make sure the map is in the set
  if( set().find( value ) == set().end() )
    throw std::invalid_argument(DESCRIPTION("The map cannot be found in the registered map set"));

  // try retrieve map from it's key
  key_type key = value->get_map_key();
  PrivateMap::iterator iter = map().find(key);
  if(iter == map().end())
    throw std::invalid_argument(DESCRIPTION("This key/map association does not exists"));

  // make a copy of the stored value
  boost::any map_ptr( iter->second );

  // erase iterator
  map().erase(iter);

  // reinsert the map using it's new key
  value->set_key( new_key );

  // note. It is _assumed_ that the value and the map_ptr point to the same object.
  // As a double-check, we make sure that the value pointer is also found in the PrivateSet
  // This assumption is needed to propagate the correct hidden type inside the boost::any pointer
  // during re-insertion. When not done, some later boost::any_cast fail.
  map().insert( std::make_pair(new_key,map_ptr));
}

//_______________________________________________
inline void PHMapManager::dump_map_keys( void )
{
  std::cout << "PHMapManager::dump_map_keys - ";
  for( PrivateMap::iterator iter = map().begin(); iter != map().end(); iter++ )
    std::cout << iter->first << " ";
  std::cout << std::endl;
}

#endif //__CINT__

#endif // __PHMAPMANAGER_H__
