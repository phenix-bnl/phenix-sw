//
// Interface Object Container Class : TMutMuiPseudoBLTMapO
// Author: chun zhang
// Date: 12/03/03
// Description: Cotainer class for muioo pseudo-BLT
//

#ifndef __TMUIPSEUDOBLTMAPO_H__
#define __TMUIPSEUDOBLTMAPO_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMuiPseudoBLTO.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMutMuiPseudoBLTO objects
/*! 
  TMutMuiPseudoBLTMapO - IOC for TMutMuiPseudoBLT objects
*/

class TMuiPseudoBLTMapO :
public PHMap<PHKey::key_type, TMuiPseudoBLTO, TMuiPseudoBLTO_v1 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMuiPseudoBLTMapO();

  /*! Construct with key */  
  TMuiPseudoBLTMapO(PHKey::map_key_type map_key);  

  /*! Virtual destructor */  
  virtual ~TMuiPseudoBLTMapO() {;}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMuiPseudoBLTO into map and return an iterator to the newly created
    object. 
  */  
  iterator insert_new(UShort_t arm); 
  //@}

  //! @name Extractors
  //@{    
  
  /*! Get an iterator to the TMuiPseudoBLT object in given arm, one for each arm */
  iterator get(UShort_t arm);  
  
  /*! Get a const iterator to the TMuiPseudoBLT object in given arm, one for each arm. */
  const_iterator get(UShort_t arm) const; 
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMuiPseudoBLTO, TMuiPseudoBLTO_v1 >::clear(); }
  //@}

 private:
  
  UShort_t get_roll_count() { return _count++%TMuiKeyGen::get_max_index();}
  UShort_t _count;

};

#endif

