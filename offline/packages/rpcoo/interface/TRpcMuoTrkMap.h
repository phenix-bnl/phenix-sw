// $Id: TRpcMuoTrkMap.h,v 1.1 2012/04/03 18:47:21 phnxrpc Exp $

#ifndef _TRpcMuoTrkMap_h_
#define _TRpcMuoTrkMap_h_

/*!
	\file TRpcMuoTrkMap.h
	\brief Container for TRpcMuoTrk objects
	\author Richard Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/04/03 18:47:21 $
*/


// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TRpcMuoTrk_v1.h>
#include<TRpcKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for TRpcMuoTrk objects

/*! 
  
  TRpcMuoTrkMap is the container for TRpcMuoTrk interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TRpcClus. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TRpcMuoTrkMap::get methods provide access to objects in the containers.
  The return value of all TRpcMuoTrkMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.

*/
class TRpcMuoTrkMap : public PHMap< PHKey::key_type, TRpcMuoTrk, TRpcMuoTrk_v1>  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TRpcMuoTrkMap():
  	_count(0)
	{}

  /*! Construct with key */
  TRpcMuoTrkMap(PHKey::map_key_type map_key): 
  	PHMap<PHKey::key_type,TRpcMuoTrk, TRpcMuoTrk_v1>(map_key),
  	_count(0)
	{}  

  /*! Virtual destructor */
  virtual ~TRpcMuoTrkMap() 
	{}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TRpcMuoTrk into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm );
  //@}

  //! @name Extractors
  //@{    

  /*! Get an iterator to all hits in given arm */
  iterator get(UShort_t arm );
  
  /*! Get a const iterator to all hits in given gap */	       
  const_iterator get(UShort_t arm ) const;
	
  //@}
  //! @name Clear
  //@{
  void clear() { 
		_count=0; 
		PHMap<PHKey::key_type, TRpcMuoTrk, TRpcMuoTrk_v1>::clear(); 
	}
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TRpcKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




