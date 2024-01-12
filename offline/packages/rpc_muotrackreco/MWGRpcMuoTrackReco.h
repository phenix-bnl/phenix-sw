#ifndef __MWGRpcMuoTrackReco_h__
#define __MWGRpcMuoTrackReco_h__

/*!
	\file MWGRpcMuoTrackReco.h
	\brief Module to find and attach an RPC cluster to the MutTrk object
	\author R. S. Hollis (rhollis@ucr.edu)
	\version $Revision: 1.2 $
	\date $Date: 2012/12/06 04:08:54 $
*/

// PHENIX includes
#include <PHPoint.h>

// Mutoo includes
//#include <PHTimeServer.h>
#include <PHTimer.h>
#include "MWGCuts.h"

class PHCompositeNode;
class TRpcMuoTrkMap;
class TRpcClusMap;
class TRpcCoordMap;
class PHMuoTracksOut;

//@{ 
/*! \ingroup modules */
//! Module to find and attach an RPC cluster to the MuiRoad object
class MWGRpcMuoTrackReco
{
 public: 
  
	//! constructor
  MWGRpcMuoTrackReco(); 
 
 	//! destructor
  virtual ~MWGRpcMuoTrackReco();
  
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
  PHTimer  *_timer;
  //PHTimeServer::timer _timer;

};

//@}

#endif







