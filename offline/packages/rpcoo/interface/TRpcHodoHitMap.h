
#ifndef _TRpcHodoHitMap_h_
#define _TRpcHodoHitMap_h_

/*!
  \file TRpcHodoHitMap.h
  \brief Container for TRpcHodoHit objects
  \author R.S.Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/02/13 02:53:09 $
*/


// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TRpcHodoHit_v1.h>
#include<TRpcKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for TRpcHodoHit objects

/*! 
  
  TRpcHodoHitMap is the container for TRpcHodoHit interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TRpcClus. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TRpcHodoHitMap::get methods provide access to objects in the containers.
  The return value of all TRpcHodoHitMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.

*/
class TRpcHodoHitMap : public PHMap< PHKey::key_type, TRpcHodoHit, TRpcHodoHit_v1 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TRpcHodoHitMap()
  {}

  /*! Construct with key */
  TRpcHodoHitMap(PHKey::map_key_type map_key): 
  	PHMap<PHKey::key_type,TRpcHodoHit, TRpcHodoHit_v1>(map_key)
  {}  

  /*! Virtual destructor */
  virtual ~TRpcHodoHitMap() 
  {}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TRpcHodoHit into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm, UShort_t station, UShort_t strip );
  
  /*! Returns an iterator to the cloned object */
  iterator insert_clone(const TRpcHodoHitMap::pointer hit_ptr);
  
  //@}

  //! @name Extractors
  //@{    
 
  /*! Get an iterator to all hits in given strip */
  // iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t strip );
  
  /*! Get a const iterator to all hits in given strip */	       
  // const_iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t strip ) const;
 
  /*! Get an iterator to all hits in given strip */
  iterator get(UShort_t arm, UShort_t station );
  
  /*! Get a const iterator to all hits in given strip */	       
  const_iterator get(UShort_t arm, UShort_t station ) const;

  /*! Get an iterator to all hits in given arm */
  iterator get(UShort_t arm );
  
  /*! Get a const iterator to all hits in given gap */	       
  const_iterator get(UShort_t arm ) const;
  
   
  /*! 
    Get an iterator to a specific hit strip. If the strip is not
    in the map the iterator's current value will be equal to end() 
    and at_end() will return true.	It is *never* safe to attempt
    to dereference the iterator returned by this method without 
    checking at_end().
  */
  iterator get(UShort_t arm, 
	       UShort_t station, 
	       UShort_t strip);
  
  //@}
  
};


#endif




