//
// Interface Object Container Class : TMuiMCHitMapO
// Author: Sean Kelly
// Date: 6/24/03
// Description: Cotainer class for mui monte-carlo hit
//

#ifndef __TMUIMCHITMAPO_H__
#define __TMUIMCHITMAPO_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
//
#include<TMuiMCHitO_v1.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMutMCHitO objects
/*! 
  TMutMCHitMapO - IOC for TMutMCHitO objects
*/


class TMuiMCHitMapO :
public PHMap<PHKey::key_type, TMuiMCHitO, TMuiMCHitO_v1 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMuiMCHitMapO();

  /*! Construct with key */  
  TMuiMCHitMapO(PHKey::map_key_type map_key);  

  /*! Virtual destructor */  
  virtual ~TMuiMCHitMapO() {;}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMuiMCHitO into map and return an iterator to the newly created
    object.
  */
  iterator insert_new(UShort_t arm,
                      UShort_t plane);
  //@}

  //! @name Extractors
  //@{    
  
  /*! Get an iterator to all roads in given arm */
  iterator get(UShort_t arm);  
  
  /*! Get a const iterator to all roads in given arm */
  const_iterator get(UShort_t arm) const; 

  /*! Get an iterator to all roads in given panel_orientation */
  iterator get(UShort_t arm,
               UShort_t plane);

  /*! Get a const iterator to all roads in given panel_orientation */
  const_iterator get(UShort_t arm,
                     UShort_t plane) const;
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMuiMCHitO, TMuiMCHitO_v1 >::clear(); }
  //@}

 private:

  UShort_t get_roll_count() { return _count++%TMuiKeyGen::get_max_index();}
  UShort_t _count;
  
};

#endif

