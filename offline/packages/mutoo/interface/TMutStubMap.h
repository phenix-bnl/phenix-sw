//
// Interface Object Container Class : TMutStubMap
// Author: chun zhang
// Date: 06/30/02
// Description: Cotainer class for muon tracker stub object
//

#ifndef __TMUTSTUBMAP_H__
#define __TMUTSTUBMAP_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMutStub_v1.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUTR TMutStub objects

/*!   
  TMutStubMap is the container for TMutStub interface objects.
*/

class TMutStubMap :
public PHMap<PHKey::key_type, TMutStub, TMutStub_v1 >
{

 public: 

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */
  TMutStubMap();
  
  /*! Construct with map key */
  TMutStubMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TMutStubMap() {;}

  //! @name Insertors
  //@{    

  /*! 
    Add new TMutStub to map, returns an iterator to the newly
    created object
  */  
  iterator insert_new(UShort_t arm,
		      UShort_t station,
		      UShort_t octant,
		      UShort_t half_octant); 

  //@}

  //! @name Extractors
  //@{    

  /*! Get an iterator to all TMutStub in given station */    
  iterator get(UShort_t arm);

  /*! Get an iterator to all TMutStub in given station */    
  const_iterator get(UShort_t arm) const;

  /*! Get an iterator to all TMutStub in given station */    
  iterator get(UShort_t arm,
	       UShort_t station);

  /*! Get an const iterator to all TMutStub in given station */    
  const_iterator get(UShort_t arm,
		     UShort_t station) const;

  /*! Get an iterator to all TMutStub in given octant */    
  iterator get(UShort_t arm,
	       UShort_t station,
	       UShort_t octan);
  
  /*! Get an const iterator to all TMutStub in given octant */    
  const_iterator get(UShort_t arm,
		     UShort_t station,
		     UShort_t octant) const; 
  
  /*! Get an iterator to all TMutStub in given half octant */    
  iterator get(UShort_t arm,
	       UShort_t station,
	       UShort_t octant,
	       UShort_t half_octant);
  
  /*! Get an const iterator to all TMutStub in given half octant */    
  const_iterator get(UShort_t arm,
		     UShort_t station,
		     UShort_t octant,
		     UShort_t half_octant) const; 
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMutStub, TMutStub_v1>::clear(); }
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;
  
};

#endif




