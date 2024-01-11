// $Id: TRpcHitMap.h,v 1.3 2009/09/24 07:53:35 hpereira Exp $

#ifndef _TRpcHitMap_h_
#define _TRpcHitMap_h_

/*!
  \file TRpcHitMap.h
  \brief Container for TRpcHit objects
  \author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2009/09/24 07:53:35 $
*/


// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TRpcHit_v1.h>
#include<TRpcKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for TRpcHit objects

/*! 
  
  TRpcHitMap is the container for TRpcHit interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TRpcClus. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TRpcHitMap::get methods provide access to objects in the containers.
  The return value of all TRpcHitMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.

*/
class TRpcHitMap : public PHMap< PHKey::key_type, TRpcHit, TRpcHit_v1 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TRpcHitMap()
  {}

  /*! Construct with key */
  TRpcHitMap(PHKey::map_key_type map_key): 
  	PHMap<PHKey::key_type,TRpcHit, TRpcHit_v1>(map_key)
  {}  

  /*! Virtual destructor */
  virtual ~TRpcHitMap() 
  {}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TRpcHit into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t strip );
  
  /*! Returns an iterator to the cloned object */
  iterator insert_clone(const TRpcHitMap::pointer hit_ptr);
  
  //@}

  //! @name Extractors
  //@{    
 
  /*! Get an iterator to all hits in given strip */
  // iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t strip );
  
  /*! Get a const iterator to all hits in given strip */	       
  // const_iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t strip ) const;
 
  /*! Get an iterator to all hits in given strip */
  iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg );
  
  /*! Get a const iterator to all hits in given strip */	       
  const_iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg ) const;

  /*! Get an iterator to all hits in given station */
  iterator get(UShort_t arm, UShort_t station );
  
  /*! Get a const iterator to all hits in given station */	       
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
         UShort_t octant,
         UShort_t half_octant,
         UShort_t rseg, 
         UShort_t strip);

  //@}
      
};


#endif




