//
// Interface Object Container Class : TMutClusMap
// Author: S.Kelly 
// Date: 2/11/01
// Description: Container class for muon tracker cluster objects
//

#ifndef __TMUTCLUSMAP_H__
#define __TMUTCLUSMAP_H__

// $Id: TMutClusMap.h,v 1.14 2011/12/29 20:19:29 slash Exp $

/*!
	 \file		TMutClusMap.h
	 \brief	 Interface Object Container : TMutClusMap
	 \author	S.Kelly
	 \version $Revision: 1.14 $
	 \date		$Date: 2011/12/29 20:19:29 $
*/

// BOOST headers
#include<boost/smart_ptr.hpp>
#include<TMutClus_v3.hh>
#include<TMutKeyGen.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutClus objects

/*!	 
	TMutClusMap is the container for TMutClus interface objects.	As is
	the case for all MUTOO interface object containers, the actual
	objects that are stored in this container, ie. the value_type in STL
	speak, is a shared_ptr to a TMutClus. Shared pointers are
	described in the <a href=index.html> FAQ </a>.	The various
	TMutClusMap::get methods provide access to objects in the containers.
	The return value of all TMutClusMap::get methods is a iterator or
	const_iterator.	Iterators are also described in the <a
	href=index.html> FAQ</a>.	For an example of how to use this
	containers see testTMutClus.cxx.
*/
class TMutClusMap : 
public PHMap< PHKey::key_type, TMutClus, TMutClus_v3 >	
{	
 public:
	
	//! @name Constructors/Destructors
	//@{		
	
	/*! Default contructor */
	TMutClusMap();
	
	/*! Construct with map key */
	TMutClusMap(PHKey::map_key_type map_key);	
	
	/*! Virtual destructor */
	virtual ~TMutClusMap() {;}
	
	//@}
	
	//! @name Insertors
	//@{		
	
	/*! 
		Insert new object at given location. Returns an
		iterator to the newly created object.
	*/
	iterator insert_new(const MUTOO::cathode_locator&);
	
	/*! 
		Insert new object at given location. Returns an
		iterator to the newly created object.
	*/	
	iterator insert_new(const UShort_t& arm, 
					const UShort_t& station, 
					const UShort_t& octant, 
					const UShort_t& half_octant, 
					const UShort_t& gap, 
					const UShort_t& cathode);
	
	//@}
	
	//! @name Extractors
	//@{		
	
	/*! Get an iterator to all TMutClus objects in given cathode */
	iterator get(const MUTOO::cathode_locator&);
	
	/*! Get an iterator to all TMutClus objects in given cathode */
	const_iterator get(const MUTOO::cathode_locator&) const;	 
	
	/*! Get an iterator to all TMutClus objects in given cathode */	
	iterator get(const UShort_t& arm, 
				 const UShort_t& station, 
				 const UShort_t& octant,
				 const UShort_t& half_octant,
				 const UShort_t& gap, 
				 const UShort_t& cathode);
	
	/*! Get an iterator to all TMutClus objects in given cathode */	
	const_iterator get(const UShort_t& arm, 
				 const UShort_t& station, 
				 const UShort_t& octant,
				 const UShort_t& half_octant,
				 const UShort_t& gap, 
				 const UShort_t& cathode) const ;

	/*! Get an iterator to all TMutClus objects in given octant */
	iterator get(const UShort_t& arm, const UShort_t& station, const UShort_t& octant);
	
	/*! Get an iterator to all TMutClus objects in given octant */
	const_iterator get(const UShort_t& arm, const UShort_t& station, const UShort_t& octant) const ;

	/*! Get an iterator to all TMutClus objects in given station */
	iterator get(const UShort_t& arm, const UShort_t& station);
	
	/*! Get an iterator to all TMutClus objects in given station */
	const_iterator get(const UShort_t& arm, const UShort_t& station) const ;
	
	//@}

	//! @name Clear
	//@{
	void clear() 
	{ _count=0; PHMap<PHKey::key_type, TMutClus, TMutClus_v3>::clear(); }
	//@}

 private:

	UShort_t get_roll_count() 
	{ return _count++%TMutKeyGen::get_max_index();}
	
	UShort_t _count;

};


#endif







