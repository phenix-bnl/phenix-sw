//
// Interface Object Container Class : TMCPrimaryMap
// Author: S.Kelly 
// Date: 9/07/03
// Description: Container class for TMCPrimary
//

#ifndef __TMCPRIMARYMAP_H__
#define __TMCPRIMARYMAP_H__

// BOOST headers
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
//
#include<TMCPrimary_v3.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMCPrimary objects
/*! 
  
  TMCPrimaryMap is the container for TMCPrimary interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TMCPrimary. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TMCPrimaryMap::get methods provide access to objects in the containers.
  The return value of all TMCPrimaryMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.
*/
class TMCPrimaryMap : public PHMap< PHKey::key_type, TMCPrimary, TMCPrimary_v3 >
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMCPrimaryMap();

  /*! Default contructor */  
  TMCPrimaryMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TMCPrimaryMap() {;}

  //@}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMCPrimary into map and return an iterator to the newly created
    object.
  */
  iterator insert_new();
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMCPrimary, TMCPrimary_v3>::clear(); }
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




