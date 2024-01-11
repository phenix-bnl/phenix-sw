#ifndef __mRpcMuoTrack_h__
#define __mRpcMuoTrack_h__

/*!
	\file mRpcMuoTrack.h
	\brief Module to find and attach an RPC cluster to the MutTrk object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.1 $
	\date $Date: 2012/04/03 18:48:46 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
#include <PHTimeServer.h>
#include <PHGslRng.h>

#include "TRpcClusMap.h"
#include "TRpcCoordMap.h"
#include "MWGCuts.h"

class PHCompositeNode;
class TRpcMuoTrkMap;
class PHMuoTracksOut;

//@{ 
/*! \ingroup modules */
//! Module to find and attach an RPC cluster to the MuiRoad object
class mRpcMuoTrack
{
 public: 
  
	//! constructor
  mRpcMuoTrack(); 
 
 	//! destructor
  virtual ~mRpcMuoTrack(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
	
  private:  

  //! get local pointers to needed nodes/maps
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! loop over MC hits
  void attach_track();
	
  //! raw hits container
  TRpcClusMap* _rpc_clus_map;

  TRpcMuoTrkMap* _rpc_trk_map;
  
  PHMuoTracksOut* _muoo;

  MWGCuts *_muocuts;

  //! module timer
  PHTimeServer::timer _timer;
};

//@}

#endif







