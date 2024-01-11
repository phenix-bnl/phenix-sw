// $Id: RpcMuoReco.cxx,v 1.1 2012/04/03 18:52:44 phnxrpc Exp $

/*!
  \file    RpcMuoReco.cxx
  \ingroup supermodules
  \brief   RPC reconstruction module.
  Reads TRpcHits from DST, create clusters coordinates and tracks
  \author  H. Pereira Da Costa
  \version $Revision: 1.1 $
  \date    $Date: 2012/04/03 18:52:44 $
*/

#include "RpcMuoReco.h"

#include <recoConsts.h>

// RPC IOC includes
#include <TRpcMuoTrkMap.h>

// RPCOO Module includes
#include <mRpcMuoTrack.h>

#include<PHTimer.h>
#include<PHTimeServer.h>

using namespace std;

//______________________________________________________
RpcMuoReco::RpcMuoReco() :
  MuonSubsysReco("RPCMUORECO"),
  _mRpcMuoTrack_mod(0),
  _timer( new PHTimer("RpcMuoReco") )
{}

//______________________________________________________
RpcMuoReco::~RpcMuoReco()
{
  delete _timer;
  delete _mRpcMuoTrack_mod;
  return;
}


//______________________________________________________
int RpcMuoReco::InitRun(PHCompositeNode *top_node)
{
  //cout << "RpcMuoReco::InitRun" << endl;
  create_node_tree(top_node);

  // Instantiate RPCOO analysis modules
  _mRpcMuoTrack_mod = new mRpcMuoTrack();

  return 0;
}

//______________________________________________________
int RpcMuoReco::create_node_tree(PHCompositeNode *top_node)
{
  //cout << "RpcMuoReco::create_node_tree" << endl;
  // RPC working space _rpc_node
  PHNodeIterator nodeItr(top_node);
  _rpc_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RPCOO"));
  if (!_rpc_node)
    {
      _rpc_node = new PHCompositeNode("RPCOO");
      top_node->addNode(_rpc_node);
    }

  // PHCompositeNode *dst_node;
  PHCompositeNode* dst_node = 0;
  dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!dst_node)
    {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }

  // interface object containers
  TRpcMuoTrkMap* muo_trk_map = TMutNode<TRpcMuoTrkMap>::new_node( _rpc_node, "TRpcMuoTrkMap" );
  muo_trk_map->make_persistant(dst_node, "TRpcMuoTrk");

  return 0;
}


//______________________________________________________
int RpcMuoReco::process_event(PHCompositeNode *top_node)
{
  //  cout << "RpcMuoReco::process_event" << endl;

  _timer->restart();

  try
    {
      load_vertex_if_needed( top_node );

      _mRpcMuoTrack_mod->event( top_node );
    }
  catch (exception& e)
    {
      RPCOO::TRACE(e.what());
    }

  write_maps_if_needed();
  _timer->stop();
  return 0;
}

//______________________________________________________
int RpcMuoReco::End(PHCompositeNode* top_node)
{
//   _timer->print_stat();
  return 0;
}
