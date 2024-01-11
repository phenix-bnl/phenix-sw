// $Id: RpcReadbackDST.cxx,v 1.19 2013/12/16 16:35:21 richi Exp $

/*!
  \file		RpcReadbackDST.h
  \brief	 reads all possible maps from a DST whatever its origin is.
  \author	Hugo PEREIRA
  \version $Revision: 1.19 $
  \date		$Date: 2013/12/16 16:35:21 $
*/

#include "RpcReadbackDST.h"

#include <RpcGeom.h> 
#include <recoConsts.h>

#include <PHMapManager.h>
#include <PHTimeServer.h>

using namespace std;

//______________________________________________________
RpcReadbackDST::RpcReadbackDST() :
  MuonSubsysReco( "RPCREADBACKDST" ),
  _timer(PHTimeServer::get()->insert_new("RPCREADBACKDST") )
{}

//______________________________________________________
int RpcReadbackDST::InitRun(PHCompositeNode *top_node)
{
  RPCOO::PRINT(cout, "RpcReadbackDST::InitRun");

  // Create Node Tree
  CreateNodeTree(top_node);

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
int RpcReadbackDST::CreateNodeTree(PHCompositeNode *top_node)
{
  try {

    // try read RpcGeomVersion from top node
    try {

 			RpcGeom::read_arms( top_node );
      cout << "RpcReadbackDST::CreateNodeTree - geometry loaded from top_node." << endl;

    } catch (exception &e ) {
      cout << e.what() << endl;
      cout << "RpcReadbackDST::CreateNodeTree - error loading geometry from top_node. Will use default." << endl;
    }

    // Instantiate nodes for rpc containers
    PHCompositeNode* rpc_node;
    {
      PHNodeIterator nodeItr(top_node);
      rpc_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RPCOO"));
      if(!rpc_node)
      {
        rpc_node = new PHCompositeNode("RPCOO");
        top_node->addNode(rpc_node);
      }
    }

    // signal node (for DST reading)
    PHCompositeNode* dst_node;
    {
      PHNodeIterator nodeItr(top_node);
      dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
      if (!dst_node)
      {
        // Error condition -- need input DST node
        cerr << "RpcReadbackDST::CreateNodeTree - could not find DST node.\n";
      }
    }

    // load all rpc maps
    _load_map<TRpcClusMap>( dst_node, rpc_node, "TRpcClusMap",	"TRpcClus"	);
    _load_map<TRpcCoordMap>( dst_node, rpc_node, "TRpcCoordMap",	"TRpcCoord"	);
    _load_map<TRpcHitMap>( dst_node, rpc_node, "TRpcHitMap",		"TRpcHit" );
    _load_map<TRpcMCHitMap>( dst_node, rpc_node, "TRpcMCHitMap",	"TRpcMCHit"	);
    _load_map<TRpcTrkMap>( dst_node, rpc_node, "TRpcTrkMap",		"TRpcTrk"	);
    _load_map<TRpcRoadMap>( dst_node, rpc_node, "TRpcRoadMap",		"TRpcRoad"	);
    _load_map<TRpcHodoHitMap>( dst_node, rpc_node, "TRpcHodoHitMap",		"TRpcHodoHit" );
    _load_map<TRpcMuoTrkMap>( dst_node, rpc_node, "TRpcMuoTrkMap",		"TRpcMuoTrk" );

  } catch (exception& e) {
    RPCOO::TRACE(e.what());
  }
  return 0;
}

//______________________________________________________
int RpcReadbackDST::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  // load external vertex
  load_vertex_if_needed( top_node );

  // read maps
  PHMapManager::read(top_node);

  // write maps
  write_maps_if_needed();

  _timer.get()->stop();
  return 0;
}

//______________________________________________________
int RpcReadbackDST::End(PHCompositeNode* top_node)
{
//   _timer.get()->print_stat();
  return 0;
}
