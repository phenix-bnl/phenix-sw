// $Id: TRpcTrkMap.h,v 1.4 2012/04/05 08:12:05 phnxrpc Exp $

#ifndef _TRpcTrkMap_h_
#define _TRpcTrkMap_h_

/*!
	\file TRpcTrkMap.h
	\brief Container for TRpcTrk objects
	\author H. Pereira Da Costa
  \version $Revision: 1.4 $
  \date    $Date: 2012/04/05 08:12:05 $
*/


// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TRpcTrk_v3.h>
#include<TRpcKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for TRpcTrk objects

/*! 
  
  TRpcTrkMap is the container for TRpcTrk interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TRpcClus. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TRpcTrkMap::get methods provide access to objects in the containers.
  The return value of all TRpcTrkMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.

*/
class TRpcTrkMap : public PHMap< PHKey::key_type, TRpcTrk, TRpcTrk_v3 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TRpcTrkMap():
  	_count(0)
	{}

  /*! Construct with key */
  TRpcTrkMap(PHKey::map_key_type map_key): 
  	PHMap<PHKey::key_type,TRpcTrk, TRpcTrk_v3>(map_key),
  	_count(0)
	{}  

  /*! Virtual destructor */
  virtual ~TRpcTrkMap() 
	{}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TRpcTrk into map and return an iterator to the newly created
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
		PHMap<PHKey::key_type, TRpcTrk, TRpcTrk_v3>::clear(); 
	}
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TRpcKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




