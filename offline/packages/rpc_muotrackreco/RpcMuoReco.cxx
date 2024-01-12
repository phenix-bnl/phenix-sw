// $Id: RpcMuoReco.cxx,v 1.2 2013/12/16 16:35:55 richi Exp $

/*!
  \file    RpcMuoReco.cxx
  \ingroup supermodules
  \brief   RPC reconstruction module.
  Reads TRpcHits from DST, create clusters coordinates and tracks
  \author  H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2013/12/16 16:35:55 $
*/

#include "RpcMuoReco.h"

#include <recoConsts.h>

// RPC IOC includes
#include <TRpcMuoTrkMap.h>

// RPCOO Module includes
#include <RPCOO.h>
#include <MWGRpcMuoTrackReco.h>

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
  _mRpcMuoTrack_mod = new MWGRpcMuoTrackReco();

  //---------------------
  //check the rpcgeom flag...
  //---------------------
  recoConsts *flags = recoConsts::instance();
  //---------------------
  // Grab the run number
  //---------------------
  int thisrunnumber = flags->get_IntFlag( "RUNNUMBER" , -1);
  if(thisrunnumber<0) {
    cout << "RpcMuoReco::InitRun "
	 << "-- UNKNOWN RUN NUMBER " << thisrunnumber << endl; 
    return 0; }
  //---------------------
  // Grab the current RpcGeomType which is set
  //---------------------
  int current_flag = -1;
  if(flags->FlagExist("RpcGeomType")) {
    current_flag = flags->get_IntFlag( "RpcGeomType" ); }
  
  //Prototype-D Run 9:
  if(thisrunnumber<291580) {//291579 is the last Run 9 physics run
    flags->set_IntFlag( "RpcGeomType", 1 ); 
    if(current_flag!=-1 && current_flag != 1) {
      cout << "RpcMuoReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 1 << endl; }
    return 0;
  }//Prototype-D Run 9
  
  //Run 10+11: Full north RPC3 AND Run 11 full n+s RPC3 + RPC1 proto
  if(thisrunnumber<350578) {//350577 is the last Run 11 physics run
    flags->set_IntFlag( "RpcGeomType", 2 ); 
    if(current_flag!=-1 && current_flag != 2) {
      cout << "RpcMuoReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 2 << endl; } 
    return 0;
  }//Run 10+11

  //Everything else is flag 3 (full detector configuration)
  flags->set_IntFlag( "RpcGeomType", 3 ); 
  if(current_flag!=-1 && current_flag != 3) {
    cout << "RpcMuoReco::InitRun "
	 << "-- CORRECTING RpcGeomType Flag!! "
	 << "was " << current_flag << " " 
	 << "now " << 3 << endl; } 

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
