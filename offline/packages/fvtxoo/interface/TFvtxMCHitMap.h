// $Id: TFvtxMCHitMap.h,v 1.7 2011/12/01 04:16:20 slash Exp $

#ifndef TFvtxMCHitMap_h
#define TFvtxMCHitMap_h

/*!
  \file    TFvtxMCHitMap.h
  \brief   Container for Forward vertex TFvtxMCHit objects
  \author  H. Pereira
  \version $Revision: 1.7 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

#include<boost/smart_ptr.hpp>
#include<TFvtxMCHit_v1.h>
#include<TFvtxKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for forward vertex TFvtxMCHit objects

/*! 
	
	TFvtxMCHitMap is the container for TFvtxMCHit interface objects.	As is
	the case for all MUTOO interface object containers, the actual
	objects that are stored in this container, ie. the value_type in STL
	speak, is a shared_ptr to a TFvtxClus. Shared pointers are
	described in the <a href=index.html> FAQ </a>.	The various
	TFvtxMCHitMap::get methods provide access to objects in the containers.
	The return value of all TFvtxMCHitMap::get methods is a iterator or
	const_iterator.	Iterators are also described in the <a
	href=index.html> FAQ </a>.

*/
class TFvtxMCHitMap : public PHMap< PHKey::key_type, TFvtxMCHit, TFvtxMCHit_v1 >	
{
	
	public:

	//! @name Constructors/Destructors
	//@{		

	/*! Default contructor */
	TFvtxMCHitMap();

	/*! Construct with key */
	TFvtxMCHitMap(PHKey::map_key_type map_key);	

	/*! Virtual destructor */
	virtual ~TFvtxMCHitMap() {;}

	//! @name Insertors
	//@{		
	/*! 
		Insert an new TFvtxMCHit into map and return an iterator to the newly created
		object.
	*/
	iterator insert_new(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	);
	//@}

	//! @name Extractors
	//@{		

	/*! Get an iterator to all hits in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	);
		
	/*! Get a const iterator to all hits in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	) const ;
	
	/*! Get an iterator to all hits in given wedge */
	iterator get(
		     const unsigned short& arm,
                     const unsigned short& cage,
		     const unsigned short& station,
		     const unsigned short& sector
		     );
		
	/*! Get a const iterator to all hits in given wedge */
	const_iterator get(
			   const unsigned short& arm,
                           const unsigned short& cage,
			   const unsigned short& station,
			   const unsigned short& sector
			   ) const;

  /*! Get an iterator to all hits in given station */
  iterator get(
	       const unsigned short& arm,
               const unsigned short& cage,
	       const unsigned short& station
	       );
  
  /*! Get a const iterator to all hits in given station */ 
  const_iterator get(
		     const unsigned short& arm,
                     const unsigned short& cage,
		     const unsigned short& station
		     ) const;

  /*! Get an iterator to all hits in given cage */
  iterator get(
               const unsigned short& arm,
               const unsigned short& cage
               );

  /*! Get a const iterator to all hits in given cage */
  const_iterator get(
                     const unsigned short& arm,
                     const unsigned short& cage
                     ) const;
  
  /*! Get an iterator to all hits in given arm */
  iterator get( const unsigned short& arm );
  
  /*! Get a const iterator to all hits in given arm */
  const_iterator get( const unsigned short& arm ) const ;
  
	//@}
	//! @name Clear
	//@{
	
	//! clear map
	void clear() 
	{ _count=0; PHMap<PHKey::key_type, TFvtxMCHit, TFvtxMCHit_v1>::clear(); }
	
	//@}
	
	private:
	
	//! MC hit counter in event
	unsigned short get_roll_count() 
	{ return _count++%TFvtxKeyGen::get_max_index();}
	
	//! MC hit counter in event
	unsigned short _count;
	
};

#endif
