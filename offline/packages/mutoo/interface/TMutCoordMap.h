//
// Interface Object Container Class : TMutCoordMap
// Author: S.Kelly 
// Date: 2/11/01
// Description: Container class for muon tracker global coordinate
//

#ifndef __TMUTCOORDMAP_H__
#define __TMUTCOORDMAP_H__
// BOOST headers
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMutCoord_v3.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutCoord objects

/*!   

*/
class TMutCoordMap : 
public PHMap< PHKey::key_type, TMutCoord, TMutCoord_v3 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default Constructor */

  /*! Default contructor */
  TMutCoordMap();

  /*! Construct with map key */  
  TMutCoordMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor  */  
  virtual ~TMutCoordMap() {;}

  //@}

  //! @name Insertors
  //@{    

  /*! 
    Insert new object at given location. Returns an
    iterator to the newly created object.
  */

  iterator insert_new(UShort_t arm, 
		      UShort_t station, 
		      UShort_t octant, 
		      UShort_t half_octant, 
		      UShort_t gap, 
		      UShort_t cathode);
  /*! 
    Insert new object using cathode locator. Returns an
    iterator to the newly created object.
  */
  
  iterator insert_new(const MUTOO::cathode_locator& location);

  //@}


  //! @name Extractors
  //@{    

  /*!
    Returns an iterator to all TMutCoord in given cathode plane.
   */
  iterator get(const MUTOO::cathode_locator&);   

  /*!
    Returns an const_iterator to all TMutCoord in given cathode plane.
   */
  const_iterator get(const MUTOO::cathode_locator&) const;   

  /*!
    Returns an iterator to all TMutCoord in given cathode plane.
   */
  iterator get(UShort_t arm, 
	       UShort_t station, 
	       UShort_t octant,
	       UShort_t half_octant,
	       UShort_t gap, 
	       UShort_t cathode);
  
  /*!
    Returns an const_iterator to all TMutCoord in given cathode plane.
   */
  const_iterator get(UShort_t arm, 
		     UShort_t station, 
		     UShort_t octant,
		     UShort_t half_octant,
		     UShort_t gap, 
		     UShort_t cathode) const ;
  /*!
    Returns an iterator to all TMutCoord in given half_octant.
  */  
  iterator get(UShort_t arm, 
	       UShort_t station, 
	       UShort_t octant,
	       UShort_t half_octant);

  /*!
    Returns an const_iterator to all TMutCoord in given half_octant.
  */  
  const_iterator get(UShort_t arm, 
		     UShort_t station, 
		     UShort_t octant,
		     UShort_t half_octant) const;
  
  /*!
    Returns an iterator to all TMutCoord in given octant.
  */  
  iterator get(UShort_t arm, 
	       UShort_t station, 
	       UShort_t octant);

  /*!
    Returns an const_iterator to all TMutCoord in given octant.
  */  
  const_iterator get(UShort_t arm, 
		     UShort_t station, 
		     UShort_t octant) const;
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMutCoord, TMutCoord_v3>::clear(); }
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




