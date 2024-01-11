//
// Interface Object Container Class : TMutMCTrkMap
// Author: S.Kelly 
// Date: 2/11/01
// Description: Container class for muon tracker monte-carlo track objects
//

#ifndef __TMUTMCTRKMAP_H__
#define __TMUTMCTRKMAP_H__

// BOOST headers
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMutMCTrk_v5.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutMCTrk objects
/*! 
  
  TMutMCTrkMap is the container for TMutMCTrk interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TMutMCTrk. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TMutMCTrkMap::get methods provide access to objects in the containers.
  The return value of all TMutMCTrkMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.
*/
class TMutMCTrkMap : public PHMap< PHKey::key_type, TMutMCTrk, TMutMCTrk_v5 >
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMutMCTrkMap();

  /*! Default contructor */  
  TMutMCTrkMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TMutMCTrkMap() {;}

  //@}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMutMCTrk into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm);
  //@}

  //! @name Extractors
  //@{    
  /*! Get an iterator to all TMutMCTrk in given arm */
  iterator get(UShort_t arm);
  /*! Get an iterator to all TMutMCTrk in given arm */
  const_iterator get(UShort_t arm) const; 
  //@}
  
  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMutMCTrk, TMutMCTrk_v5>::clear(); }
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




