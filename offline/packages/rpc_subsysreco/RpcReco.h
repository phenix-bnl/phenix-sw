// $Id: RpcReco.h,v 1.10 2012/12/03 13:29:14 richi Exp $
#ifndef _RpcReco_h_
#define _RpcReco_h_

/*!
  \file    RpcReco.h  
  \ingroup supermodules
  \brief   RPC reconstruction module. 
	Reads TRpcHits from DST, create clusters coordinates and tracks
  \author  H. Pereira Da Costa
  \version $Revision: 1.10 $
  \date    $Date: 2012/12/03 13:29:14 $
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
//class mRpcFindTrackMC;
//class mRpcKalFit;

//! RPC Reconstruction module
/*!
  \ingroup supermodules
	RPC reconstruction module. 
	Reads TRpcHits from DST, create clusters coordinates and tracks
*/
class RpcReco: public MuonSubsysReco
{
 public:

  //! constructor
  RpcReco();

  //! destructor
  virtual ~RpcReco();

  //! run initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event processing
  int process_event(PHCompositeNode *topNode);
  
  //! end of process
  int End(PHCompositeNode *topNode);
 
  void set_MatchOnly(bool kflag) { _MatchOnly = kflag; }
  void set_NoMatching(bool kflag) { _NoMatching = kflag; }
  
  void set_doRpcRoad(bool kflag) { _doRpcRoad = kflag; }

 protected:
  
  //! create all new nodes
  int create_node_tree(PHCompositeNode *topNode);

  //! rpc working node
  PHCompositeNode* _rpc_node;

  // RPC module data members
  mRpcFindClus* _mRpcFindClus_mod;
  mRpcFitClus* _mRpcFitClus_mod;
  mRpcRoad *_mRpcRoad_mod;
  mRpcTrack *_mRpcTrack_mod;
//  mRpcFindTrackMC* _mRpcFindTrackMC_mod;
//  mRpcKalFit* _mRpcKalFit_mod;
  PHTimer* _timer;

  //! flags
  bool _MatchOnly;
  bool _NoMatching;
  bool _doRpcRoad;
};

#endif 
