#ifndef __mRpcRoad_h__
#define __mRpcRoad_h__

/*!
	\file mRpcRoad.h
	\brief Module to find and attach an RPC cluster to the MuiRoad object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.1 $
	\date $Date: 2010/09/09 20:38:13 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
#include <PHTimeServer.h>
#include <TMutMCTrkMap.h>
#include <PHGslRng.h>

#include "TRpcClusMap.h"

class PHCompositeNode;
class TRpcRoadMap;
class TMuiRoadMapO;

//@{ 
/*! \ingroup modules */
//! Module to find and attach an RPC cluster to the MuiRoad object
class mRpcRoad
{
 public: 
  
	//! constructor
  mRpcRoad(); 
 
 	//! destructor
  virtual ~mRpcRoad(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
	
  private:  

  //! get local pointers to needed nodes/maps
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! loop over MC hits
  void attach_road();
	
  //! raw hits container
  TRpcClusMap* _rpc_clus_map;

  TMuiRoadMapO* _mui_road_map;

  TRpcRoadMap* _rpc_road_map;
  
  //! module timer
  PHTimeServer::timer _timer;
};

//@}

#endif







