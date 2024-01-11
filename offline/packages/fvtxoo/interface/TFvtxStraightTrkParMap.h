// $Id: TFvtxStraightTrkParMap.h,v 1.2 2011/12/01 04:16:20 slash Exp $

#ifndef __TFVTXSTRAIGHTTRKPARMAP_H__
#define __TFVTXSTRAIGHTTRKPARMAP_H__

/*!
  \file    TFvtxStraightTrkParMap.h
  \brief   Interface Object Container Class : TFvtxStraightTrkParMap
  \author  D. Winter
  \version $Revision: 1.2 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

// BOOST headers
//
//#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TFvtxStraightTrkPar_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for Fvtx TFvtxStraightTrkPar objects

/*! Interface Object Container for TFvtxStraightTrkPar objects. */
class TFvtxStraightTrkParMap: public PHMap< PHKey::key_type, TFvtxStraightTrkPar, TFvtxStraightTrkPar_v1 >
{
  
  public:

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */  
  TFvtxStraightTrkParMap();

  /*! Construct with map key */  
  TFvtxStraightTrkParMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxStraightTrkParMap() {;}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(unsigned short arm); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxStraightTrkParMap::pointer trk_ptr ); 
  
  /*! Get an iterator to all TFvtxStub in given arm */      
  iterator get(unsigned short arm);

  /*! Get an iterator to all TFvtxStub in given arm */    
  const_iterator get(unsigned short arm) const; 


  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxStraightTrkPar, TFvtxStraightTrkPar_v1>::clear(); }
  //@}
  
  private:
  
  unsigned short get_roll_count() 
  { return _count++%TFvtxKeyGen::get_max_index();}
  
  unsigned short _count;
  

};

#endif
