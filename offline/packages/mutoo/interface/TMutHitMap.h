//
// Interface Object Container Class : TMutHitMap
// Author: S.Kelly 
// Date: 2/11/01
// Description: Container class for muon tracker hits
//

#ifndef __TMUTHITMAP_H__
#define __TMUTHITMAP_H__

#include<boost/smart_ptr.hpp>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

#include"TMutHit_v2.hh"

/*! \ingroup container */
//! Container for MUTR TMutHit objects

/*! 
	
	TMutHitMap is the container for TMutHit interface objects.	As is
	the case for all MUTOO interface object containers, the actual
	objects that are stored in this container, ie. the value_type in STL
	speak, is a shared_ptr to a THitObject. Shared pointers are
	described in the <a href=index.html> FAQ </a>.	The various
	TMutHitMap::get methods provide access to objects in the containers.
	The return value of all TMutHitMap::get methods is a iterator or
	const_iterator.	Iterators are also described in the <a
	href=index.html> FAQ</a>.	For an example of how to use this
	containers see <a href=testTMutHitMap_8cxx-source.html> testHitMap </a>.

*/
class TMutHitMap : public PHMap< PHKey::key_type, TMutHit, TMutHit_v2>	
{
	
public:

	//! @name Constructors/Destructors
	//@{		

	/*! Default contructor */
	TMutHitMap();

	/*! Construct with map key */
	TMutHitMap(PHKey::map_key_type map_key);

	/*! Virtual destructor */
	virtual ~TMutHitMap() {;}

	//@}

	//! @name Insertors
	//@{		

	/*! Returns an iterator to the newly created object */
	iterator insert_new(UShort_t arm, 
					UShort_t station, 
					UShort_t octant, 
					UShort_t half_octant, 
					UShort_t gap, 
					UShort_t cathode, 
					UShort_t strip);
	
	/*! Returns an iterator to the cloned object */
	iterator insert_clone(const TMutHitMap::pointer hit_ptr);
	
	//@}

	//! @name Extractors
	//@{		

	/*! Get an iterator to all hits in given cathode */
	iterator get(const MUTOO::cathode_locator&);	 

	/*! Get an iterator to all hits in given cathode */
	const_iterator get(const MUTOO::cathode_locator&) const; 

	/*! Get an iterator to all hits in given cathode */
	iterator get(UShort_t arm,
				 UShort_t station, 
				 UShort_t octant,
				 UShort_t half_octant,
				 UShort_t gap, 
				 UShort_t cathode);

	/*! Get an iterator to all hits in given cathode */
	const_iterator get(UShort_t arm, 
				 UShort_t station, 
				 UShort_t octant,
				 UShort_t half_octant,
				 UShort_t gap, 
				 UShort_t cathode) const ;

	/*! Get an iterator to all hits in given station */
	iterator get(UShort_t arm,
				 UShort_t station,
				 UShort_t octant);
	
	/*! Get an iterator to all hits in given station */
	const_iterator get(UShort_t arm, 
				 UShort_t station,
				 UShort_t octant) const ;

	/*! Get an iterator to all hits in given station */
	iterator get(UShort_t arm, UShort_t station);
	
	/*! Get an iterator to all hits in given station */
	const_iterator get(UShort_t arm, UShort_t station) const ;

	/*! Get an iterator to all hits in given arm */
	iterator get(UShort_t arm );
	
	/*! Get an iterator to all hits in given arm */
	const_iterator get(UShort_t arm ) const ;
				 
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
				 UShort_t gap, 
				 UShort_t cathode,
				 UShort_t strip);

	//@}

};



#endif




