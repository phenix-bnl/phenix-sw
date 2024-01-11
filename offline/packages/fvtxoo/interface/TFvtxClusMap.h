// $Id: TFvtxClusMap.h,v 1.5 2011/12/01 04:16:20 slash Exp $

#ifndef TFvtxClusMap_h
#define TFvtxClusMap_h

/*!
	\file		TFvtxClusMap.h
	\brief	 Container for forward vertex cluster objects
	\author	H. Pereira
	\version $Revision: 1.5 $
	\date		$Date: 2011/12/01 04:16:20 $
*/

#include<boost/smart_ptr.hpp>
#include<TFvtxClus_v1.h>
#include<TFvtxKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for forward vertex TFvtxClus objects

/*! 
	
	TFvtxClusMap is the container for TFvtxClus interface objects. As is
	the case for all MUTOO interface object containers, the actual
	objects that are stored in this container, ie. the value_type in STL
	speak, is a shared_ptr to a TFvtxClus. Shared pointers are
	described in the <a href=index.html> FAQ </a>.	The various
	TFvtxClusMap::get methods provide access to objects in the containers.
	The return value of all TFvtxClusMap::get methods is a iterator or
	const_iterator.	Iterators are also described in the <a
	href=index.html> FAQ </a>.

*/
class TFvtxClusMap : public PHMap< PHKey::key_type, TFvtxClus, TFvtxClus_v1 >	
{
	
	public:

	//! @name Constructors/Destructors
	//@{		

	/*! Default contructor */
	TFvtxClusMap();

	/*! Construct with key */
	TFvtxClusMap(PHKey::map_key_type map_key);	

	/*! Virtual destructor */
	virtual ~TFvtxClusMap() {;}

	//! @name Insertors
	//@{		
	/*! 
		Insert an new TFvtxClus into map and return an iterator to the newly created
		object.
	*/
	iterator insert_new(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	);
	
	/*! Returns an iterator to the cloned object */
	iterator insert_clone(const TFvtxClusMap::pointer hit_ptr);
	
	//@}

	//! @name Extractors
	//@{		

	/*! Get an iterator to all clusters in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	);
		
	/*! Get a const iterator to all clusters in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column
	) const ;

	/*! Get an iterator to all clusters in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector
	);
		
	/*! Get a const iterator to all clusters in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector
	) const ;

	/*! Get an iterator to all clusters in given gap */
	iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station
	);
		
	/*! Get a const iterator to all clusters in given gap */				 
	const_iterator get(
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station 
		) const ;

        /*! Get an iterator to all clusters in given gap */
        iterator get(
                const unsigned short& arm,
                const unsigned short& cage
        );

        /*! Get a const iterator to all clusters in given gap */
        const_iterator get(
                const unsigned short& arm,
                const unsigned short& cage
                ) const ;

	/*! Get an iterator to all clusters in given gap */
	iterator get( const unsigned short& arm );
		
	/*! Get a const iterator to all clusters in given gap */				 
	const_iterator get( const unsigned short& arm ) const ;
	
	//@}

	//! @name Clear
	//@{
	void clear() 
	{ _count=0; PHMap<PHKey::key_type, TFvtxClus, TFvtxClus_v1>::clear(); }
	//@}

	private:

	unsigned short get_roll_count() 
	{ return _count++%TFvtxKeyGen::get_max_index();}
	
	unsigned short _count;
		
};

#endif
