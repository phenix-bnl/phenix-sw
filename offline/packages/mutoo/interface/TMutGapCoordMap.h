//
// Interface Object Container Class : TMutGapCoordMap
// Author: S.Kelly 
// Date: 2/11/01
// Description: Container class for muon tracker global coordinate
//

#ifndef __TMUTGAPCOORDMAP_H__
#define __TMUTGAPCOORDMAP_H__
// BOOST headers
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
//
#include<TMutGapCoord_v2.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutGapCoord objects

/*!   
  Interface Object Container for TMutGapCoord objects.
*/
class TMutGapCoordMap : public PHMap< PHKey::key_type, TMutGapCoord, TMutGapCoord_v2 >  {
  
 public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */  
  TMutGapCoordMap();

  /*! Construct with map key */
  TMutGapCoordMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TMutGapCoordMap() {;}  

  //@}

  //! @name Insertors
  //@{    

  /*! 
    Create a new TMutGapCoord object.  Returns an iterator to 
    the newly created object 
  */  
  iterator insert_new(UShort_t arm, 
		      UShort_t station, 
		      UShort_t octant, 
		      UShort_t half_octant, 
		      UShort_t gap);
    
  //@}

  //! @name Extractors
  //@{    

  /*! Get an iterator to all TMutGapCoord objects in given gap */ 
  iterator get(const MUTOO::gap_locator&);   

  /*! Get an iterator to all TMutGapCoord objects in given gap */ 
  const_iterator get(const MUTOO::gap_locator&) const; 
  
  /*! Get an iterator to all TMutGapCoord objects in given gap */ 
  iterator get(UShort_t arm, 
	       UShort_t station, 
	       UShort_t octant,
	       UShort_t half_octant,
	       UShort_t gap);

  /*! Get an iterator to all TMutGapCoord objects in given gap */ 
  const_iterator get(UShort_t arm, 
		     UShort_t station, 
		     UShort_t octant,
		     UShort_t half_octant,
		     UShort_t gap) const ;
  
  /*! Get an iterator to all TMutGapCoord objects in given half_octant */ 
  iterator get(UShort_t arm, 
	       UShort_t station, 
	       UShort_t octant,
	       UShort_t half_octant);

  /*! Get an iterator to all TMutGapCoord objects in given half_octant */ 
  const_iterator get(UShort_t arm, 
		     UShort_t station, 
		     UShort_t octant,
		     UShort_t half_octant) const ;
  
  /*! Get an iterator to all TMutGapCoord objects in given station */ 
  iterator get(UShort_t arm, 
	       UShort_t station);
  
  /*! Get an iterator to all TMutGapCoord objects in given station */ 	         
  const_iterator get(UShort_t arm, 
		     UShort_t station) const;
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMutGapCoord, TMutGapCoord_v2>::clear(); }
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;
  
};


#endif




