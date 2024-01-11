#ifndef __TFVTXPISAHITMAP_H__
#define __TFVTXPISAHITMAP_H__
/*!
  \file    TFvtxPisaHitMap.h
  \brief   Interface Object Container Class : TFvtxPisaHitMap
  \author  M. Brooks
  \version $Revision: 1.2 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

// BOOST headers
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TFvtxPisaHit_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for Fvtx TFvtxPisaHit objects

/*! Interface Object Container for TFvtxPisaHit objects. */
class TFvtxPisaHitMap: public PHMap< PHKey::key_type, TFvtxPisaHit, TFvtxPisaHit_v1 >
{
  
  public:

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */  
  TFvtxPisaHitMap();

  /*! Construct with map key */  
  TFvtxPisaHitMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxPisaHitMap() 
  {}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(void); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxPisaHitMap::pointer ); 

  //@}

  //! @name Clear
  //@{
  void clear() 
  { 
    _count=0; 
    PHMap<PHKey::key_type, TFvtxPisaHit, TFvtxPisaHit_v1>::clear(); 
  }
  //@}
  
  private:
  
  //! return counter on hits
  unsigned short get_roll_count() 
  { return _count++%TFvtxKeyGen::get_max_index(); }
  
  unsigned short _count;

};

#endif
