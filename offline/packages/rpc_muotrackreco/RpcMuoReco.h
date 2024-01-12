// $Id: RpcMuoReco.h,v 1.1 2012/05/18 17:04:27 phnxrpc Exp $
#ifndef _RpcMuoReco_h_
#define _RpcMuoReco_h_

/*!
  \file    RpcMuoReco.h  
  \ingroup supermodules
  \brief   RPC reconstruction module. 
	Reads TRpcHits from DST, create clusters coordinates and tracks
  \author  H. Pereira Da Costa
  \version $Revision: 1.1 $
  \date    $Date: 2012/05/18 17:04:27 $
*/

#include <string>
#include <MuonSubsysReco.h>

// Forward declerations
class PHCompositeNode;
class PHTimer;
class mRpcFindClus;
class mRpcFitClus;
class mRpcRoad;
class mRpcTrack;
class MWGRpcMuoTrackReco;
//class mRpcFindTrackMC;
//class mRpcKalFit;

//! RPC Reconstruction module
/*!
  \ingroup supermodules
	RPC reconstruction module. 
	Reads TRpcHits from DST, create clusters coordinates and tracks
*/
class RpcMuoReco: public MuonSubsysReco
{
 public:

  //! constructor
  RpcMuoReco();

  //! destructor
  virtual ~RpcMuoReco();

  //! run initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event processing
  int process_event(PHCompositeNode *topNode);
  
  //! end of process
  int End(PHCompositeNode *topNode);
 
 protected:
  
  //! create all new nodes
  int create_node_tree(PHCompositeNode *topNode);

  //! rpc working node
  PHCompositeNode* _rpc_node;
 
  // RPC module data members
  MWGRpcMuoTrackReco *_mRpcMuoTrack_mod;

  PHTimer* _timer;
  
};

#endif 
