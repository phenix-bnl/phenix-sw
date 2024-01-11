#ifndef __TFVTXSVXCLUSTERMAP_H__
#define __TFVTXSVXCLUSTERMAP_H__
/*!
  \file    TFvtxSvxClusterMap.h
  \brief   Interface Object Container Class : TFvtxSvxClusterMap
  \author  Dave Winter
  \version $Revision: 1.3 $
  \date    $Date: 2012/10/01 04:28:01 $
*/

// BOOST headers
//
#include<boost/smart_ptr.hpp>

// PHENIX headers
#include<TFvtxSvxCluster_v4.h>
#include<TFvtxKeyGen.h>
#include<PHMap.h>
#include<PHKey.hh>

/*! \ingroup container */
//! Container for Fvtx TFvtxSvxCluster objects

/*! Interface Object Container for TFvtxSvxCluster objects. */
class TFvtxSvxClusterMap: public PHMap< PHKey::key_type, TFvtxSvxCluster, TFvtxSvxCluster_v4 >
{
  
public:

  //! @name Constructors/Destructors
  //@{    
 
  /*! Default constructor */  
  TFvtxSvxClusterMap();

  /*! Construct with map key */  
  TFvtxSvxClusterMap(PHKey::map_key_type map_key);  

  /*! Virtual destructor */
  virtual ~TFvtxSvxClusterMap() {}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(void); 
  
  // clone a track, insert it in the map
  iterator insert_clone( const TFvtxSvxClusterMap::pointer ); 

  //@}

  //! @name Clear
  //@{
  void clear() 
  { 
    _count=0; 
    PHMap<PHKey::key_type, TFvtxSvxCluster, TFvtxSvxCluster_v4>::clear(); 
  }
  //@}
  
private:
  
  //! return counter on hits
  unsigned short get_roll_count() { return _count++%TFvtxKeyGen::get_max_index(); }
  
  unsigned short _count;

};

#endif
