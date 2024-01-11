// $Id: mRpcResponse.h,v 1.3 2008/08/28 00:53:56 kempel Exp $
#ifndef __mRpcResponse_h__
#define __mRpcResponse_h__

/*!
  \file    mRpcResponse.h
  \brief   rpc response module. Generate TRpcHit from TMutMCTrk and TRpcMCHit
  \author  H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2008/08/28 00:53:56 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>
#include <PHGslRng.h>

#include "mRpcResponsePar.h"
#include "TRpcMCHitMap.h"
#include "TRpcHitMap.h"

class PHCompositeNode;

//@{ 
/*! \ingroup modules */
//! rpc response module. Generate TRpcHit from TMutMCTrk and TRpcMCHit
class mRpcResponse
{
 public: 
  
	//! constructor
  mRpcResponse(); 
 
 	//! destructor
  virtual ~mRpcResponse(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
	
  private:  

  //! get local pointers to needed nodes/maps
  void set_interface_ptrs(PHCompositeNode* top_node);
  
 	//! loop over MC hits
  void response_loop();

	//! create raw hits from MC hits
  void create_hit(TRpcMCHitMap::pointer mc_hit_ptr);
 	
	//! parameter table
  const mRpcResponsePar* _mod_par;           
	
	//! mc trks container
  TMutMCTrkMap* _mc_trk_map;                
	
	//! mc hits container
  TRpcMCHitMap* _mc_hit_map;                
	
	//! raw hits container
  TRpcHitMap* _hit_map;                
  
  //! Random number generator
  PHGslRng _rng;

  //! module timer
  PHTimeServer::timer _timer;
};

//@}

#endif







