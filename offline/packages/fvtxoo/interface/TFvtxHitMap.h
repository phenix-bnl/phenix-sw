// $Id: TFvtxHitMap.h,v 1.8 2011/12/01 04:16:20 slash Exp $

#ifndef TFvtxHitMap_h
#define TFvtxHitMap_h

/*!
  \file    TFvtxHitMap.h
  \brief   Container for forward vertex hit objects
  \author  H. Pereira
  \version $Revision: 1.8 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

#include<boost/smart_ptr.hpp>
#include<TFvtxHit_v1.h>
#include<TFvtxKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for forward vertex TFvtxHit objects

/*! 
	
	TFvtxHitMap is the container for TFvtxHit interface objects. As is
	the case for all MUTOO interface object containers, the actual
	objects that are stored in this container, ie. the value_type in STL
	speak, is a shared_ptr to a TFvtxHit. Shared pointers are
	described in the <a href=index.html> FAQ </a>.	The various
	TFvtxHitMap::get methods provide access to objects in the containers.
	The return value of all TFvtxHitMap::get methods is a iterator or
	const_iterator.	Iterators are also described in the <a
	href=index.html> FAQ </a>.

*/
class TFvtxHitMap : public PHMap< PHKey::key_type, TFvtxHit, TFvtxHit_v1 >	
{
	
	public:

	//! @name Constructors/Destructors
	//@{		

	/*! Default contructor */
	TFvtxHitMap();

	/*! Construct with key */
	TFvtxHitMap(PHKey::map_key_type map_key);	

	/*! Virtual destructor */
	virtual ~TFvtxHitMap() {;}

	//! @name Insertors
	//@{		
	/*! 
		Insert an new TFvtxHit into map and return an iterator to the newly created
		object.
	*/
	iterator insert_new(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column,
		const unsigned short& strip
	);
	
	/*! Returns an iterator to the cloned object */
	iterator insert_clone(const TFvtxHitMap::pointer hit_ptr);
	
	//@}

	//! @name Extractors
	//@{		

	/*! Get an iterator to all hits in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column,
		const unsigned short& strip
	);
		
	/*! Get a const iterator to all hits in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column,
		const unsigned short& strip
	) const ;

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

	/*! Get an iterator to all hits in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector
	);
		
	/*! Get a const iterator to all hits in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector
	) const ;

	/*! Get an iterator to all hits in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station
	);
		
	/*! Get a const iterator to all hits in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station 
		) const ;

        /*! Get an iterator to all hits in given gap */
        iterator get(
                const unsigned short& arm,
                const unsigned short& cage
        );

        /*! Get a const iterator to all hits in given gap */
        const_iterator get(
                const unsigned short& arm,
                const unsigned short& cage
                ) const ;

	/*! Get an iterator to all hits in given gap */
	iterator get( const unsigned short& arm );
		
	/*! Get a const iterator to all hits in given gap */				 
	const_iterator get( const unsigned short& arm ) const ;
	
	//@}
		
};

#endif
