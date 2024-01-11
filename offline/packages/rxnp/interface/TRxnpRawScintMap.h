// $Id: TRxnpRawScintMap.h,v 1.3 2006/12/08 20:21:35 zhangc Exp $

#ifndef _TRxnpRawScintMap_h_
#define _TRxnpRawScintMap_h_

/*!
  \file TRxnpRawScintMap.h
  \brief Container for TRxnpRawScint objects
  \author Chun Zhang
  \version $Revision: 1.3 $
  \date    $Date: 2006/12/08 20:21:35 $
*/

// 
//
#include <iostream>

// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>
//
#include<TRxnpKeyGen.h>
#include"TRxnpRawScint_v1.h"

/*! \ingroup container */
//! Container for TRxnpRawScint objects

/*! 
  
  TRxnpRawScintMap is the container for TRxnpRawScint interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TRxnpRawScint_v1. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TRxnpScintMap::get methods provide access to objects in the containers.
  The return value of all TRxnpRawScintMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.

*/
class TRxnpRawScintMap : public PHMap< PHKey::key_type, TRxnpRawScint, TRxnpRawScint_v1 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TRxnpRawScintMap():
    _count(0)
    {}

  /*! Construct with key */
  TRxnpRawScintMap(PHKey::map_key_type map_key): 
    PHMap<PHKey::key_type,TRxnpRawScint, TRxnpRawScint_v1>(map_key),
    _count(0)
    {}  

  /*! Virtual destructor */
  virtual ~TRxnpRawScintMap() 
    {}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TRxnpScint into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm, UShort_t ring, UShort_t scint );
  
  /*! Returns an iterator to the cloned object */
  iterator insert_clone(const TRxnpRawScintMap::pointer scint_ptr);
	
  //@}

  //! @name Extractors
  //@{    
 
  /*! Get an iterator to a scintilator at a given location */
  iterator get(UShort_t arm, UShort_t ring, UShort_t scint);
  
  /*! Get a const iterator to a scintilator at a given location */	       
  const_iterator get(UShort_t arm, UShort_t ring, UShort_t scint ) const;
 
  /*! Get an iterator to all scintilators at a given ring */
  iterator get(UShort_t arm, UShort_t ring );
  
  /*! Get a const iterator to all scintilators at a given ring  */	       
  const_iterator get(UShort_t arm, UShort_t ring ) const;

  /*! Get an iterator to all scintilators in a given arm */
  iterator get(UShort_t arm );
  
  /*! Get a const iterator to all scintulators in a given arm */	       
  const_iterator get(UShort_t arm ) const;
  
  //@}
  //! @name Clear
  //@{
  void clear() { 
    _count=0; 
    PHMap<PHKey::key_type, TRxnpRawScint, TRxnpRawScint_v1>::clear(); 
  }
  //@}
  // dumpper of map
  //
  virtual void print(std::ostream& os = std::cout) const;
  
 private:
  
  UShort_t get_roll_count() { return _count++%TRxnpKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




