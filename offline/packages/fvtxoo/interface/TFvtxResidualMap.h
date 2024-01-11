// $Id: TFvtxResidualMap.h,v 1.3 2011/12/01 04:16:20 slash Exp $

#ifndef __TFVTXRESIDUALMAP_H__
#define __TFVTXRESIDUALMAP_H__

/*!
  \file    TFvtxResidualMap.h
  \brief   Interface Object Container Class : TFvtxResidualMap
  \author  D. Winter
  \version $Revision: 1.3 $
  \date    $Date: 2011/12/01 04:16:20 $
*/

#include<TFvtxResidual_v1.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for Fvtx TFvtxResidual objects

/*! Interface Object Container for TFvtxResidual objects. */
class TFvtxResidualMap : public PHMap< PHKey::key_type, TFvtxResidual, TFvtxResidual_v1 >
{
  
  public:

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */  
  TFvtxResidualMap();

  /*! Construct with map key */  
  TFvtxResidualMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxResidualMap() {;}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(const unsigned short arm); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxResidualMap::pointer trk_ptr ); 
  
  /*! Get an iterator to all TFvtxStub in given arm */      
  iterator get(const unsigned short arm);

  /*! Get an iterator to all TFvtxStub in given arm */    
  const_iterator get(const unsigned short arm) const; 


  //@}

  //! @name Clear
  //@{
  void clear() { _count=0; PHMap<PHKey::key_type, TFvtxResidual, TFvtxResidual_v1>::clear(); }
  //@}
  
  private:
  
  unsigned short get_roll_count() 
  { return _count++%TFvtxKeyGen::get_max_index();}
  
  unsigned short _count;
  
};

#endif
