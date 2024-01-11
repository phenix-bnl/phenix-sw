// $Id: RpcReco.cxx,v 1.17 2013/12/16 16:35:21 richi Exp $

/*!
  \file    RpcReco.cxx
  \ingroup supermodules
  \brief   RPC reconstruction module.
  Reads TRpcHits from DST, create clusters coordinates and tracks
  \author  H. Pereira Da Costa
  \version $Revision: 1.17 $
  \date    $Date: 2013/12/16 16:35:21 $
*/

#include "RpcReco.h"

#include <recoConsts.h>

// RPC IOC includes
#include <TRpcTrkMap.h>
#include <TRpcRoadMap.h>

// RPCOO Module includes
#include <mRpcFindClus.h>
#include <mRpcFitClus.h>
#include <mRpcRoad.h>
#include <mRpcTrack.h>

#include<PHTimer.h>
#include<PHTimeServer.h>

using namespace std;

//______________________________________________________
RpcReco::RpcReco() :
  MuonSubsysReco("RPCRECO"),
  _mRpcFindClus_mod(0),
  _mRpcFitClus_mod(0),
  //  _mRpcRoad_mod(0),
  _mRpcTrack_mod(0),
  //  _mRpcFindTrackMC_mod(0),
  //  _mRpcKalFit_mod(0),
  _timer( new PHTimer("RpcReco") ),
  _MatchOnly(0),
  _NoMatching(0),
  _doRpcRoad(0)
{}

//______________________________________________________
RpcReco::~RpcReco()
{
  delete _timer;
  delete _mRpcFindClus_mod;
  delete _mRpcFitClus_mod;
  //  if(_mRpcRoad_mod)  delete _mRpcRoad_mod;
  if(_mRpcTrack_mod) delete _mRpcTrack_mod;
  return;
}


//______________________________________________________
int RpcReco::InitRun(PHCompositeNode *top_node)
{
  //cout << "RpcReco::InitRun" << endl;
  create_node_tree(top_node);

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
  }
  //---------------------
  // Grab the current RpcGeomType which is set
  //---------------------
  int current_flag = -1;
  if(flags->FlagExist("RpcGeomType")) {
    current_flag = flags->get_IntFlag( "RpcGeomType"); }
  
  //Prototype-D Run 9:
  if(thisrunnumber<291580) {//291579 is the last Run 9 physics run
    flags->set_IntFlag( "RpcGeomType", 1 ); 
    if(current_flag!=-1 && current_flag != 1) {
      cout << "RpcReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 1 << endl; }
  }//Prototype-D Run 9
  
  //Run 10+11: Full north RPC3 AND Run 11 full n+s RPC3 + RPC1 proto
  else if(thisrunnumber<350578) {//350577 is the last Run 11 physics run
    flags->set_IntFlag( "RpcGeomType", 2 ); 
    if(current_flag!=-1 && current_flag != 2) {
      cout << "RpcReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 2 << endl; } 
  }//Run 10+11

  //Everything else is flag 3 (full detector configuration)
  else {
    flags->set_IntFlag( "RpcGeomType", 3 ); 
    if(current_flag!=-1 && current_flag != 3) {
      cout << "RpcReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 3 << endl; } 
  }
  // Instantiate RPCOO analysis modules
  if(_MatchOnly==false) {
    _mRpcFindClus_mod = new mRpcFindClus();
    _mRpcFitClus_mod = new mRpcFitClus();
    //    if(_NoMatching==false) {
    if(_doRpcRoad) {
      _mRpcRoad_mod = new mRpcRoad(); }
  }
  if(_NoMatching==false) {
    _mRpcTrack_mod = new mRpcTrack(); }
  //  _mRpcFindTrackMC_mod = new mRpcFindTrackMC();
  //	_mRpcKalFit_mod = new mRpcKalFit();

  return 0;
}

//______________________________________________________
int RpcReco::create_node_tree(PHCompositeNode *top_node)
{
  //cout << "RpcReco::create_node_tree" << endl;
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
  if(_MatchOnly==false) {
    TRpcClusMap* clus_map = TMutNode<TRpcClusMap>::new_node( _rpc_node, "TRpcClusMap" );
    clus_map->make_persistant(dst_node, "TRpcClus");
    TRpcCoordMap* coord_map = TMutNode<TRpcCoordMap>::new_node( _rpc_node, "TRpcCoordMap" );
    coord_map->make_persistant(dst_node, "TRpcCoord");
    if(_doRpcRoad) {
      TRpcRoadMap* road_map = TMutNode<TRpcRoadMap>::new_node( _rpc_node, "TRpcRoadMap" );
      road_map->make_persistant(dst_node, "TRpcRoadv1"); }
  }
  
  TRpcTrkMap* trk_map = TMutNode<TRpcTrkMap>::new_node( _rpc_node, "TRpcTrkMap" );
  trk_map->make_persistant(dst_node, "TRpcTrk");

  // Module parameters tables
  mRpcFindClusPar *mRpcFindClus_par = TMutNode<mRpcFindClusPar>::new_node( _rpc_node, "mRpcFindClusPar" );
  mRpcFitClusPar *mRpcFitClus_par = TMutNode<mRpcFitClusPar>::new_node( _rpc_node, "mRpcFitClusPar" );
  //	mRpcFindTrackMCPar *mRpcFindTrackMC_par = TMutNode<mRpcFindTrackMCPar>::new_node( _rpc_node, "mRpcFindTrackMCPar" );
  //	mRpcKalFitPar *mRpcKalFit_par = TMutNode<mRpcKalFitPar>::new_node( _rpc_node, "mRpcKalFitPar" );
  mRpcFindClus_par->set_verbosity( RPCOO::NONE );
  mRpcFitClus_par->set_verbosity( RPCOO::NONE );
  //	mRpcRoad_par->set_verbosity( RPCOO::NONE );
  //	mRpcFindTrackMC_par->set_verbosity( RPCOO::NONE );
  //  mRpcKalFit_par->set_verbosity( RPCOO::SOME );

  if ( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    {
      mRpcFindClus_par->print();
      mRpcFitClus_par->print();
      //		mRpcFindTrackMC_par->print();
      //		mRpcKalFit_par->print();
    }

  return 0;
}


//______________________________________________________
int RpcReco::process_event(PHCompositeNode *top_node)
{
  //  cout << "RpcReco::process_event" << endl;

  _timer->restart();

  try
    {
      load_vertex_if_needed( top_node );

      if(_MatchOnly==false) {
	_mRpcFindClus_mod->event( _rpc_node );
	_mRpcFitClus_mod->event( _rpc_node );
	//      if(_NoMatching==false) {
	if(_doRpcRoad) {
	  _mRpcRoad_mod->event( top_node ); }
      }
      if(_NoMatching==false) {
	_mRpcTrack_mod->event( top_node ); }
      //    _mRpcFindTrackMC_mod->event( top_node );
      //    _mRpcKalFit_mod->event( top_node );

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
int RpcReco::End(PHCompositeNode* top_node)
{
//   _timer->print_stat();
  return 0;
}
