//
// Interface Object Container Class : TMutMuiPseudoLL1Map
// Description: Cotainer class for muioo pseudo-LL1
//

#ifndef __TMUIPSEUDOLL1MAPO_H__
#define __TMUIPSEUDOLL1MAPO_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMuiPseudoLL1.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMutMuiPseudoLL1 objects
/*! 
  TMutMuiPseudoLL1Map - IOC for TMutMuiPseudoLL1 objects
*/

class TMuiPseudoLL1Map :
public PHMap<PHKey::key_type, TMuiPseudoLL1, TMuiPseudoLL1_v1 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMuiPseudoLL1Map();

  /*! Construct with key */  
  TMuiPseudoLL1Map(PHKey::map_key_type map_key);  

  /*! Virtual destructor */  
  virtual ~TMuiPseudoLL1Map() {;}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMuiPseudoLL1 into map and return an iterator to the newly created
    object. 
  */  
  iterator insert_new(UShort_t arm); 
  //@}

  //! @name Extractors
  //@{    
  
  /*! Get an iterator to the TMuiPseudoLL1 object in given arm, one for each arm */
  iterator get(UShort_t arm);  
  
  /*! Get a const iterator to the TMuiPseudoLL1 object in given arm, one for each arm. */
  const_iterator get(UShort_t arm) const; 
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMuiPseudoLL1, TMuiPseudoLL1_v1 >::clear(); }
  //@}

 private:
  
  UShort_t get_roll_count() { return _count++%TMuiKeyGen::get_max_index();}
  UShort_t _count;

};

#endif

