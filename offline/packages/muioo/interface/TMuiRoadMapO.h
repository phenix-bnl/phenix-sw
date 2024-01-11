//
// Interface Object Container Class : TMuiRoadO
// Author: Jason Newby
// Date: 02/12/03
// Description: Cotainer class for mui road
//

#ifndef __TMUIROADMAPO_H__
#define __TMUIROADMAPO_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>
// PHENIX headers
//
#include<TMuiRoadO_v2.h>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMuiRoadMapO objects
/*! 
  TMuiRoadMapO - IOC for TMuiRoadO objects
*/

class TMuiRoadMapO :
public PHMap<PHKey::key_type, TMuiRoadO, TMuiRoadO_v2 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMuiRoadMapO();

  /*! Construct with key */  
  TMuiRoadMapO(PHKey::map_key_type map_key);  

  /*! Virtual destructor */  
  virtual ~TMuiRoadMapO() {;}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMuiRoadO into map and return an iterator to the newly created
    object.
  */  
  iterator insert_new(UShort_t arm); 
  
  // clone a road, insert it in the map
  iterator insert_clone( const TMuiRoadMapO::pointer road_ptr ); 
  
  //@}

  //! @name Extractors
  //@{    
  
  /*! Get an iterator to all roads in given arm */
  iterator get(UShort_t arm);  
  
  /*! Get a const iterator to all roads in given arm */
  const_iterator get(UShort_t arm) const; 
  
  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMuiRoadO, TMuiRoadO_v2 >::clear(); }
  //@}

 private:
  
  UShort_t get_roll_count() { return _count++%TMuiKeyGen::get_max_index();}
  UShort_t _count;

};

#endif

