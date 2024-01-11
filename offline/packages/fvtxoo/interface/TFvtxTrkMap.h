// $Id: TFvtxTrkMap.h,v 1.3 2011/12/01 04:16:21 slash Exp $ 

#ifndef __TFVTXTRKMAP_H__
#define __TFVTXTRKMAP_H__

/*!
  \file    TFvtxTrkMap.h
  \brief   Interface Object Container Class : TFvtxTrkMap
  \author  M. Brooks
  \version $Revision: 1.3 $
  \date    $Date: 2011/12/01 04:16:21 $
*/

// BOOST headers
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TFvtxTrk_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for Fvtx TFvtxTrk objects

/*! Interface Object Container for TFvtxTrk objects. */
class TFvtxTrkMap: public PHMap< PHKey::key_type, TFvtxTrk, TFvtxTrk_v1 >
{
  
  public:

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */  
  TFvtxTrkMap();

  /*! Construct with map key */  
  TFvtxTrkMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxTrkMap() {;}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(unsigned short arm); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxTrkMap::pointer trk_ptr ); 
  
  /*! Get an iterator to all TFvtxStub in given arm */      
  iterator get(unsigned short arm);

  /*! Get an iterator to all TFvtxStub in given arm */    
  const_iterator get(unsigned short arm) const; 


  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxTrk, TFvtxTrk_v1>::clear(); }
  //@}
  
  private:
  
  unsigned short get_roll_count() 
  { return _count++%TFvtxKeyGen::get_max_index();}
  
  unsigned short _count;
  

};

#endif
