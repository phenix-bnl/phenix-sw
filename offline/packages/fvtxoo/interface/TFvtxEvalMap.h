#ifndef __TFvtxEvalMAP_H__
#define __TFvtxEvalMAP_H__

// BOOST headers
//
#include<boost/smart_ptr.hpp>
// PHENIX Headers
//
#include<TFvtxEval_v1.h>
#include<TFvtxKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

class TFvtxEvalRes;
class TFvtxTrkEval;

/*! \ingroup container */
//! Container for FVTX TFvtxEval objects

/*!   
  TFvtxEvalMap is the container for TFvtxHit interface objects.
*/
class TFvtxEvalMap :
public PHMap<PHKey::key_type, TFvtxEval, TFvtxEval_v1>
{

 public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */  
  TFvtxEvalMap();

  /*! Construct with map key */
  TFvtxEvalMap(PHKey::map_key_type map_key);

  /*! Virtual destructor */
  virtual ~TFvtxEvalMap(){;}
  
  //@}

  //! @name Insertors
  //@{    
  
  /*! Add new TFvtxEval object to map */    
  iterator insert_new(unsigned short arm,
		      unsigned short cage);
  
  //@}

  //! @name Extractors
  //@{    

  /*! Get an iterator to all TFvtxEval in given arm */    
  iterator get(unsigned short arm);
  /*! Get a const_iterator to all TFvtxEval in given arm */    
  const_iterator get(unsigned short arm) const;

  /*! Get an iterator to all TFvtxEval in given cage */    
  iterator get(unsigned short arm, unsigned short cage);
  /*! Get a const_iterator to all TFvtxEval in given cage */    
  const_iterator get(unsigned short arm, unsigned short cage) const;

  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxEval, TFvtxEval_v1>::clear(); }
  //@}
  
 private:
  
  unsigned short get_roll_count() { return _count++%TFvtxKeyGen::get_max_index();}
  unsigned short _count;
  
};

#endif






