//
// Interface Object Container Class : TMui1DRoadMapO
// Author: Jason Newby
// Date: 02/12/03
// Description: Cotainer class for mui road
//

#ifndef __TMUI1DROADMAPO_H__
#define __TMUI1DROADMAPO_H__

// BOOST header
#include<boost/smart_ptr.hpp>
#include<boost/array.hpp>

// PHENIX headers
#include<TMui1DRoadO_v1.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMui1DRoadO objects
/*! 
  TMui1DRoadMapO - IOC for TMui1DRoadO objects
*/

class TMui1DRoadMapO :
public PHMap<PHKey::key_type, TMui1DRoadO, TMui1DRoadO_v1 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  //! Default contructor 
  TMui1DRoadMapO()
  { _counts.assign(0); }

  //! Construct with key
  TMui1DRoadMapO(PHKey::map_key_type map_key):
    PHMap<PHKey::key_type, TMui1DRoadO, TMui1DRoadO_v1>(map_key)
  { _counts.assign(0); }

  //! Virtual destructor
  virtual ~TMui1DRoadMapO() 
  {}

  //! @name Insertors
  //@{    
  
  /*! 
    \brief
    Insert an new TMui1DRoadO into map and return an iterator to the newly created
    object.
  */  
  iterator insert_new(const UShort_t& arm, const UShort_t& panel, const UShort_t& orientation);
  
  //@}

  //! @name Extractors
  //@{    
  
  //! Get an iterator to all roads in given arm 
  iterator get( const UShort_t& arm);  
  
  //! Get a const iterator to all roads in given arm
  const_iterator get( const UShort_t& arm) const;

  //! Get an iterator to all roads in given arm
  iterator get(const UShort_t& arm, const UShort_t& panel, const UShort_t& orientation);

  //! Get a const iterator to all roads in given arm
  const_iterator get(const UShort_t& arm, const UShort_t& panel, const UShort_t& orientation) const;
  
  //@}


  //! @name Clear
  //@{
  void clear() 
  { 
    _counts.assign(0); 
    PHMap<PHKey::key_type, TMui1DRoadO, TMui1DRoadO_v1 >::clear(); 
  }
  //@}

 private:
  
  //! rolling counter for (arm, panel, orientation)
  UShort_t get_roll_count(const UShort_t& arm, const UShort_t& panel, const UShort_t& orientation) 
  { 
    UShort_t index( orientation + MUIOO::MAX_ORIENTATION*( panel + MUIOO::MAX_ORIENTATION*arm )); 
    return _counts[index]++%TMuiKeyGen::get_max_index();
  }
  
  //! rolling counter for (arm, panel, orientation)
  boost::array< UShort_t, MUIOO::MAX_ARM*MUIOO::MAX_PANEL*MUIOO::MAX_ORIENTATION > _counts;

};

#endif

