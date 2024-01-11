//
// Interface Object Container Class : TMuiEvalMap
// Author: Sean Kelly
// Date: 9/03/03
// Description: Container class for TMuiEval
//

#ifndef __TMUIEVALMAP_H__
#define __TMUIEVALMAP_H__

// BOOST header
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
//
#include<TMuiEval.hh>
#include<TMuiKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for MUID TMuiEval objects
/*! 
  TMuiEvalMap - IOC for TMuiEval objects
*/

class TMuiEvalMap :
public PHMap<PHKey::key_type, TMuiEval, TMuiEval_v1 >
{

 public:
  
  //! @name Constructors/Destructors
  //@{    

  /*! Default contructor */
  TMuiEvalMap();

  /*! Construct with key */  
  TMuiEvalMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */  
  virtual ~TMuiEvalMap() {;}

  //! @name Insertors
  //@{    
  /*! 
    Insert an new TMuiEval into map and return an iterator to the newly created
    object.
  */  
  iterator insert_new(UShort_t arm); 
  //@}

  //! @name Extractors
  //@{    
  
  /*! Get an iterator to all roads in given arm */
  iterator get(UShort_t arm);  
  
  /*! Get a const iterator to all roads in given arm */
  const_iterator get(UShort_t arm) const; 
  
  //@}
 private:
  
  UShort_t get_roll_count() { return _count++%TMuiKeyGen::get_max_index();}
  UShort_t _count;

};

#endif

