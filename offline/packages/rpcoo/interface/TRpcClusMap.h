// $Id: TRpcClusMap.h,v 1.2 2008/08/28 00:50:18 kempel Exp $

#ifndef _TRpcClusMap_h_
#define _TRpcClusMap_h_

/*!
	\file TRpcClusMap.h
	\brief Container for TRpcClus objects
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:18 $
*/


// BOOST headers
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TRpcClus_v1.h>
#include<TRpcKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for TRpcClus objects

/*! 
  TRpcClusMap is the container for TRpcClus interface objects.  As is
  the case for all MUTOO interface object containers, the actual
  objects that are stored in this container, ie. the value_type in STL
  speak, is a shared_ptr to a TRpcClus. Shared pointers are
  described in the <a href=index.html> FAQ </a>.  The various
  TRpcClusMap::get methods provide access to objects in the containers.
  The return value of all TRpcClusMap::get methods is a iterator or
  const_iterator.  Iterators are also described in the <a
  href=index.html> FAQ </a>.

*/
class TRpcClusMap : public PHMap< PHKey::key_type, TRpcClus, TRpcClus_v1 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TRpcClusMap():
  	_count(0)
	{}

  /*! Construct with key */
  TRpcClusMap(PHKey::map_key_type map_key): 
  	PHMap<PHKey::key_type,TRpcClus, TRpcClus_v1>(map_key),
  	_count(0)
	{}  

  /*! Virtual destructor */
  virtual ~TRpcClusMap() 
	{}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TRpcClus into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg );
  //@}

  //! @name Extractors
  //@{    
 
  /*! Get an iterator to all hits in given rseg */
  iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg );
  
  /*! Get a const iterator to all hits in given rseg */	       
  const_iterator get(UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg ) const;
 
  /*! Get an iterator to all hits in given station */
  iterator get(UShort_t arm, UShort_t station );
  
  /*! Get a const iterator to all hits in given station */	       
  const_iterator get(UShort_t arm, UShort_t station) const;

  /*! Get an iterator to all hits in given arm */
  iterator get(UShort_t arm );
  
  /*! Get a const iterator to all hits in given gap */	       
  const_iterator get(UShort_t arm ) const;
	
  //@}
  //! @name Clear
  //@{
  void clear() { 
		_count=0; 
		PHMap<PHKey::key_type, TRpcClus, TRpcClus_v1>::clear(); 
	}
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TRpcKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




