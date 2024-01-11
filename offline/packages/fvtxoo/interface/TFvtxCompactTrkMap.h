#ifndef __TFVTXCOMPACTTRKMAP_H__
#define __TFVTXCOMPACTTRKMAP_H__

/*!
  \file    TFvtxCompactTrkMap.h
  \brief   Interface Object Container Class : TFvtxCompactTrkMap
  \author  Cesar L. da Silva
  \version $Revision: 1.6 $
  \date    $Date: 2013/12/20 06:48:48 $
*/

// BOOST headers
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TFvtxCompactTrk_v4.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for Fvtx TFvtxCompactTrk objects

/*! Interface Object Container for TFvtxCompactTrk objects. */
class TFvtxCompactTrkMap: public PHMap< PHKey::key_type, TFvtxCompactTrk, TFvtxCompactTrk_v4 >
{
  
  public:

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */  
  TFvtxCompactTrkMap();

  /*! Construct with map key */  
  TFvtxCompactTrkMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxCompactTrkMap() {;}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(unsigned short arm); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxCompactTrkMap::pointer trk_ptr ); 
  
  /*! Get an iterator to all TFvtxStub in given arm */      
  iterator get(unsigned short arm);

  /*! Get an iterator to all TFvtxStub in given arm */    
  const_iterator get(unsigned short arm) const; 


  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxCompactTrk, TFvtxCompactTrk_v4>::clear(); }
  //@}
  
  private:
  
  unsigned short get_roll_count() 
  { return _count++%TFvtxKeyGen::get_max_index();}
  
  unsigned short _count;
  

};

#endif
