// $Id: TMutMCHitMap.h,v 1.12 2011/12/29 20:19:30 slash Exp $

#ifndef __TMUTMCHITMAP_H__
#define __TMUTMCHITMAP_H__

/*!
  \file    TMutMCHitMap.h
  \brief   Container for MUTR TMutMCHit objects
  \author  S. Kelly
  \version $Revision: 1.12 $
  \date    $Date: 2011/12/29 20:19:30 $
*/

#include<boost/smart_ptr.hpp>
#include<TMutMCHit_v2.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutMCHit objects

/*! 
	
	TMutMCHitMap is the container for TMutMCHit interface objects.	As is
	the case for all MUTOO interface object containers, the actual
	objects that are stored in this container, ie. the value_type in STL
	speak, is a shared_ptr to a TMutClus. Shared pointers are
	described in the <a href=index.html> FAQ </a>.	The various
	TMutMCHitMap::get methods provide access to objects in the containers.
	The return value of all TMutMCHitMap::get methods is a iterator or
	const_iterator.	Iterators are also described in the <a
	href=index.html> FAQ </a>.

*/
class TMutMCHitMap : public PHMap< PHKey::key_type, TMutMCHit, TMutMCHit_v2 >	
{
	
	public:

	//! @name Constructors/Destructors
	//@{		

	/*! Default contructor */
	TMutMCHitMap();

	/*! Construct with key */
	TMutMCHitMap(PHKey::map_key_type map_key);	

	/*! Virtual destructor */
	virtual ~TMutMCHitMap() {;}

	//! @name Insertors
	//@{		
	/*! 
		Insert an new TMutMCHit into map and return an iterator to the newly created
		object.
	*/
	iterator insert_new(
		const UShort_t& arm, 
		const UShort_t& station, 
		const UShort_t& octant, 
		const UShort_t& half_octant, 
		const UShort_t& gap);
	//@}

	//! @name Extractors
	//@{		
	/*! Get an iterator to all hits in given gap */
	iterator get(const MUTOO::gap_locator&);	 

	/*! Get an const iterator to all hits in given gap */
	const_iterator get(const MUTOO::gap_locator&) const;	 

	/*! Get an iterator to all hits in given gap */
	iterator get(
		const UShort_t& arm, 
		const UShort_t& station, 
		const UShort_t& octant,
		const UShort_t& half_octant,
		const UShort_t& gap);
	
	/*! Get a const iterator to all hits in given gap */				 
	const_iterator get(
			const UShort_t& arm, 
			const UShort_t& station, 
			const UShort_t& octant,
			const UShort_t& half_octant,
			const UShort_t& gap) const ;
	//@}
	//! @name Clear
	//@{
	
	//! clear
	void clear() 
	{ _count=0; PHMap<PHKey::key_type, TMutMCHit, TMutMCHit_v2>::clear(); }
	
	//@}
	
	private:
	
	//! hit index counter
	UShort_t get_roll_count() 
	{ return _count++%TMutKeyGen::get_max_index();}
	
	//! hit index counter
	UShort_t _count;
	
};


#endif




