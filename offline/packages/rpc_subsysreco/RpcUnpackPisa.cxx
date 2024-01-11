// $Id: RpcUnpackPisa.cxx,v 1.14 2015/02/09 15:51:05 richi Exp $

#include <MuPCGetGEA.h>
#include <mupcghitWrapper.h>
#include <mRpcResponse.h>
#include <mRpcSlowSim.h>
#include <PHIODataNode.h>
#include <PHTimer.h>
#include <PHTimeServer.h>
#include <recoConsts.h>
#include <RpcGeom.h>
#include <RPCOO.h>
#include <TMutMCTrkMap.h>
#include <TRpcHitMap.h>
#include <TRpcMCHitMap.h>

#include "RpcUnpackPisa.h"

using namespace std;

//______________________________________________________
RpcUnpackPisa::RpcUnpackPisa() : 
  MuonSubsysReco("RPCUNPACKPISA"),
  _rpc_node( 0 ),
  _mutoo_node( 0 ),
  _dst_node( 0 ),
  _mRpcSlowSim_mod( 0 ),
  _mRpcResponse_mod( 0 ),
  _run_response(false),
  _timer( new PHTimer("RPCUNPACKPISA") )
{}

//______________________________________________________
RpcUnpackPisa::~RpcUnpackPisa()
{
  if(_timer) { delete _timer; }
}

//______________________________________________________
int RpcUnpackPisa::InitRun(PHCompositeNode *topNode)
{
      
  // Create Node Tree
  create_node_tree(topNode);

  // Instantiate analysis modules
  _mRpcSlowSim_mod	= new mRpcSlowSim();
  _mRpcResponse_mod = new mRpcResponse();
  
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

//__________________________________________________________________________
int RpcUnpackPisa::create_node_tree(PHCompositeNode *top_node)
{
  
  // RUN node. Write RPC geometry
  {
    PHNodeIterator nodeItr(top_node);
    PHCompositeNode* run_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RUN"));
    if( !run_node ) cout << "RpcUnpackPisa::create_node_tree - unable to find RUN node. Geometry will not be saved" << endl;
    else RpcGeom::write_arms( run_node );
  }
  
  // RPCOO node
  {
    PHNodeIterator nodeItr(top_node);
    _rpc_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RPCOO"));
    if(!_rpc_node){
      _rpc_node = new PHCompositeNode("RPCOO");
      top_node->addNode(_rpc_node);
    }
  }
  
  // MUTOO node (for MCTrkMaps)
  {
    PHNodeIterator nodeItr(top_node);
    _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!_mutoo_node){
      _mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(_mutoo_node);
    }
  }
  
  // DST node
  {
    PHNodeIterator nodeItr(top_node);
    _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!_dst_node) {
      _dst_node = new PHCompositeNode("DST");
      top_node->addNode( _dst_node );
    }
  }

  // rpc MC hit map
  TRpcMCHitMap* mc_hit_map = TMutNode<TRpcMCHitMap>::new_node(_rpc_node,"TRpcMCHitMap");
  mc_hit_map->make_persistant(_dst_node,"TRpcMCHit");
  
  // rpc hit map
  TRpcHitMap* hit_map = TMutNode<TRpcHitMap>::new_node(_rpc_node,"TRpcHitMap");
  if( do_response() ) hit_map->make_persistant(_dst_node,"TRpcHit");
  
  // MC Trk map
  TMutMCTrkMap* mc_trk_map = 0;
  try {
    mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(_mutoo_node,"TMutMCTrkMap");
    cout << "RpcUnpackPisa::create_node_tree - found existing TMutMCTrkMap" << endl;
  } catch(std::exception& e){
    cout << "RpcUnpackPisa::create_node_tree - creating TMutMCTrkMap" << endl;
    mc_trk_map = TMutNode<TMutMCTrkMap>::new_node(_mutoo_node,"TMutMCTrkMap");
    mc_trk_map->make_persistant(_dst_node,"TMutMCTrk");
  }	
  
  // Set up pisa takes needed for mut/muioo slowsim
  PHNodeIterator nodeIter(top_node);
  PHCompositeNode *gea_node = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "GEA"));		
  
  // setup pisa. There is one RPC node per station
  mupcghitWrapper* mupc1ghit = new mupcghitWrapper("mupc1ghit", 15000);
  mupcghitWrapper* mupc2ghit = new mupcghitWrapper("mupc2ghit", 15000);
  mupcghitWrapper* mupc3ghit = new mupcghitWrapper("mupc3ghit", 15000);
  PHIODataNode<PHTable>* mupc1ghitNode = new PHIODataNode<PHTable>(mupc1ghit, "mupc1ghit");
  PHIODataNode<PHTable>* mupc2ghitNode = new PHIODataNode<PHTable>(mupc2ghit, "mupc2ghit");
  PHIODataNode<PHTable>* mupc3ghitNode = new PHIODataNode<PHTable>(mupc3ghit, "mupc3ghit");
  gea_node->addNode(mupc1ghitNode);
  gea_node->addNode(mupc2ghitNode);
  gea_node->addNode(mupc3ghitNode);

  // Module parameter tables
  mRpcSlowSimPar* mRpcSlowSim_par = TMutNode<mRpcSlowSimPar>::new_node( _rpc_node, "mRpcSlowSimPar" );
  mRpcResponsePar* mRpcResponse_par = TMutNode<mRpcResponsePar>::new_node( _rpc_node, "mRpcResponsePar" );
  
  mRpcSlowSim_par->set_verbosity(RPCOO::NONE);
  mRpcResponse_par->set_verbosity(RPCOO::NONE);
 	
  if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) 
  {
    mRpcSlowSim_par->print();
    mRpcResponse_par->print();
  }
    
  return 0;
}

//________________________________________________________________________________________
int RpcUnpackPisa::process_event(PHCompositeNode *top_node)
{
  _timer->restart();
      
  try {

    // load external vertex
    load_vertex_if_needed( top_node );

    // Fill rpchits table from PISA tree
    MuPCGetGEA(top_node);	
      
    // Call mutoo slow simulator and response
    // Be careful with the sequence now, mutr first 
    // muid later, but it will fix soon.
    _mRpcSlowSim_mod->event(top_node);
    
    // response modules
    if(do_response() ) _mRpcResponse_mod->event(top_node);
          
  } catch (std::exception& e) { MUTOO::TRACE(e.what()); }
    
  write_maps_if_needed();
  _timer->stop();

  return 0;

}

//____________________________________________________________
int RpcUnpackPisa::End(PHCompositeNode* top_node) 
{

//   _timer->print_stat();
  if( _mRpcSlowSim_mod ) _mRpcSlowSim_mod->print_summary();
  
  return 0;
}
