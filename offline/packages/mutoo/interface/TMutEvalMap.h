#ifndef __TMUTEVALMAP_H__
#define __TMUTEVALMAP_H__

// BOOST headers
//
#include<boost/smart_ptr.hpp>
// PHENIX Headers
//
#include<TMutEval_v1.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

class TMutEvalRes;
class TMutTrkEval;

/*! \ingroup container */
//! Container for MUTR TMutEval objects

/*!   
  TMutEvalMap is the container for TMutHit interface objects.
*/
class TMutEvalMap :
public PHMap<PHKey::key_type, TMutEval, TMutEval_v1>
{

 public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */  
  TMutEvalMap();

  /*! Construct with map key */
  TMutEvalMap(PHKey::map_key_type map_key);

  /*! Virtual destructor */
  virtual ~TMutEvalMap(){;}
  
  //@}

  //! @name Insertors
  //@{    
  
  /*! Add new TMutEval object to map */    
  iterator insert_new(UShort_t arm,
		      UShort_t octant);
  
  //@}

  //! @name Extractors
  //@{    

  /*! Get an iterator to all TMutEval in given arm */    
  iterator get(UShort_t arm);
  /*! Get a const_iterator to all TMutEval in given arm */    
  const_iterator get(UShort_t arm) const;

  /*! Get an iterator to all TMutEval in given octant */    
  iterator get(UShort_t arm, UShort_t octant);
  /*! Get a const_iterator to all TMutEval in given octant */    
  const_iterator get(UShort_t arm, UShort_t octant) const;

  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TMutEval, TMutEval_v1>::clear(); }
  //@}
  
 private:
  
  UShort_t get_roll_count() { return _count++%TMutKeyGen::get_max_index();}
  UShort_t _count;
  
};

#endif






