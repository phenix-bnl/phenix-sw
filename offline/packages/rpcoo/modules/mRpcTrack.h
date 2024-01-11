#ifndef __mRpcTrack_h__
#define __mRpcTrack_h__

/*!
	\file mRpcTrack.h
	\brief Module to find and attach an RPC cluster to the MutTrk object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.2 $
	\date $Date: 2011/02/04 20:38:30 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>
#include <PHGslRng.h>

#include "TRpcClusMap.h"
#include "TRpcCoordMap.h"

class PHCompositeNode;
class TRpcTrkMap;
class TMutTrkMap;
class TMuiRoadMapO;

//@{ 
/*! \ingroup modules */
//! Module to find and attach an RPC cluster to the MuiRoad object
class mRpcTrack
{
 public: 
  
	//! constructor
  mRpcTrack(); 
 
 	//! destructor
  virtual ~mRpcTrack(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
	
  private:  

  //! get local pointers to needed nodes/maps
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! loop over MC hits
  void attach_track();
	
  //! raw hits container
  TRpcClusMap* _rpc_clus_map;

  TMuiRoadMapO* _mui_road_map;

  TMutTrkMap *_mut_trk_map;

  TRpcTrkMap* _rpc_trk_map;
  
  //! module timer
  PHTimeServer::timer _timer;
};

//@}

#endif







