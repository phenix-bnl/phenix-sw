// $Id: RpcUnpackSim.cxx,v 1.20 2013/12/16 16:35:21 richi Exp $

/*!
  \file		RpcUnpackSim.cxx
  \ingroup supermodules
  \brief	 RPC Fast simulation module. Reads a slowsim DST. Creates TRpc MC Hits
  from MC tracks. Runs the restonse to create Rpc hits for later reconstruction
  \warning there is no embedding done so far in this module.
  \author	H. Pereira Da Costa
  \version $Revision: 1.20 $
  \date		$Date: 2013/12/16 16:35:21 $
*/

#include "RpcUnpackSim.h"

#include <Fun4AllServer.h>
#include <recoConsts.h>
#include <RPCOO.h>

// MUTOO IOC includes
#include <TMutMCTrkMap.h>

// RPC IOC includes
// RPCOO Module includes
#include <mRpcEmbedPar.h>

#include <RpcGeom.h>

#include <PHTimer.h>
#include <boost/array.hpp>

using namespace std;

//______________________________________________________
RpcUnpackSim::RpcUnpackSim( const char* name, unsigned int mode ) :
  MuonSubsysReco( name ),
  _signalNodeName( "SIGNAL" ),
  _backgroundNodeName( "BACKGROUND" ),
  _rpc_node(0),
  _signal_node(0),
  _ioc_signal_node(0),
  _background_node(0),
  _ioc_background_node(0),
  _timer(PHTimeServer::get()->insert_new(name) ),
  _mode( mode )
{}

//______________________________________________________
int RpcUnpackSim::InitRun(PHCompositeNode *top_node)
{

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "MuonUnpackSim::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  if ( rc->FlagExist("EMBED_REAL_TOPNODE") )
  {
    cout << "MuonUnpackSim::InitRun - reading _backgroundNodeName from recoConst EMBED_REAL_TOPNODE" << endl;
    SetBackgroundNodeName( rc->get_CharFlag("EMBED_REAL_TOPNODE") );
  }

  cout << "RpcUnpackSim::InitRun - _signalNodeName : " << _signalNodeName << endl;
  cout << "RpcUnpackSim::InitRun - _backgroundNodeName: " << _backgroundNodeName << endl;

  try {
    load_geometry( top_node );
    set_node_ptrs( top_node );
    set_interface_ptrs( top_node );
    set_module_ptrs( top_node );

  } catch(std::exception& e){ cout << e.what() << endl; }


  //---------------------
  //check the rpcgeom flag...
  //---------------------
  //---------------------
  // Grab the run number
  //---------------------
  int thisrunnumber = rc->get_IntFlag( "RUNNUMBER" , -1);
  if(thisrunnumber<0) {
    cout << "RpcMuoReco::InitRun "
	 << "-- UNKNOWN RUN NUMBER " << thisrunnumber << endl; 
    return 0; }
  //---------------------
  // Grab the current RpcGeomType which is set
  //---------------------
  int current_flag = -1;
  if(rc->FlagExist("RpcGeomType")) {
    current_flag = rc->get_IntFlag( "RpcGeomType" ); }
  
  //Prototype-D Run 9:
  if(thisrunnumber<291580) {//291579 is the last Run 9 physics run
    rc->set_IntFlag( "RpcGeomType", 1 ); 
    if(current_flag!=-1 && current_flag != 1) {
      cout << "RpcMuoReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 1 << endl; }
    return 0;
  }//Prototype-D Run 9
  
  //Run 10+11: Full north RPC3 AND Run 11 full n+s RPC3 + RPC1 proto
  if(thisrunnumber<350578) {//350577 is the last Run 11 physics run
    rc->set_IntFlag( "RpcGeomType", 2 ); 
    if(current_flag!=-1 && current_flag != 2) {
      cout << "RpcMuoReco::InitRun "
	   << "-- CORRECTING RpcGeomType Flag!! "
	   << "was " << current_flag << " " 
	   << "now " << 2 << endl; } 
    return 0;
  }//Run 10+11

  //Everything else is flag 3 (full detector configuration)
  rc->set_IntFlag( "RpcGeomType", 3 ); 
  if(current_flag!=-1 && current_flag != 3) {
    cout << "RpcMuoReco::InitRun "
	 << "-- CORRECTING RpcGeomType Flag!! "
	 << "was " << current_flag << " " 
	 << "now " << 3 << endl; } 


  return 0;
}

//______________________________________________________
void RpcUnpackSim::SetMode(unsigned int mode)
{
  _mode = mode;
  boost::array<const char*, 4> mode_string = {{
    "MC_SIGNAL_REAL_BG",
    "MC_SIGNAL_MC_BG",
    "MC_SIGNAL_NO_BG" }};

  MUTOO::PRINT(cout, "RpcUnpackSim SetMode");
  cout << "Mode set to " << mode_string[_mode] << endl;
  MUTOO::PRINT(cout, "**");

}

//______________________________________________________
int RpcUnpackSim::load_geometry(PHCompositeNode *top_node)
{

  try {

    // try read RpcGeomVersion from top node
    Fun4AllServer* se = Fun4AllServer::instance();
    PHCompositeNode* signal_top_node = se->topNode( _signalNodeName );
    RpcGeom::read_arms( signal_top_node );
    cout << "RpcUnpackSim::load_geometry - geometry loaded from top_node." << endl;

  } catch (exception &e ) {

    cout << e.what() << endl;
    cout << "RpcUnpackSim::load_geometry - error loading geometry from top_node. Will use default." << endl;

  }

  return 0;
}

//______________________________________________________
int RpcUnpackSim::set_node_ptrs(PHCompositeNode *top_node)
{

  // get signal and background top nodes
  Fun4AllServer* se = Fun4AllServer::instance();
  PHCompositeNode* signal_top_node = se->topNode( _signalNodeName );
  PHCompositeNode* background_top_node = se->topNode( _backgroundNodeName );

  // create relevant node iterators
  PHNodeIterator signal_nodeItr( signal_top_node );
  PHNodeIterator background_nodeItr( background_top_node );
  PHNodeIterator nodeItr(top_node);


  // Merged node -- rpcoo maps used for reconstruction
  _rpc_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "RPCOO"));
  if(!_rpc_node){
    _rpc_node = new PHCompositeNode("RPCOO");
    top_node->addNode(_rpc_node);
  }

  // Signal node -- Maps associated with signal DST
  // IOC node names are shared accross subsystems (MUTR/Muid/RPC and RXNP).
  // This is why they are all called MUTOO
  _ioc_signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_ioc_signal_node){
    _ioc_signal_node = new PHCompositeNode("MUTOO");
    signal_top_node->addNode(_ioc_signal_node);
  }

  // Background node -- Maps associated with background DST
  // IOC node names are shared accross subsystems (MUTR/Muid/RPC and RXNP).
  // This is why they are all called MUTOO
  _ioc_background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_ioc_background_node){
    _ioc_background_node = new PHCompositeNode("MUTOO");
    background_top_node->addNode(_ioc_background_node);
  }

  // DST Signal
  _signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!_signal_node) {
    throw runtime_error(DESCRIPTION("Cannot locate SIGNAL node"));
  }

  // DST Background
  _background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!_background_node) {
    throw runtime_error(DESCRIPTION("Cannot locate BACKGROUND node"));
  }

  return 0;

}

//______________________________________________________
int RpcUnpackSim::set_interface_ptrs(PHCompositeNode *top_node)
{


  try{

    // RPC maps
    // from signal_node
    TRpcMCHitMap* rpc_mchit_map = TMutNode<TRpcMCHitMap>::new_dst_input_node(_ioc_signal_node, "TRpcMCHitMap", _signal_node, "TRpcMCHit");

    // copy signal MC hits to output DST node
    PHNodeIterator nodeItr(top_node);
    PHCompositeNode* dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (dst_node)
    {
      rpc_mchit_map->make_persistant(dst_node,"TRpcMCHit");

      // Make sure EVA & GEA end up in the output DST if they are in the input signal DST
      PHNodeIterator dstItr(dst_node);
      PHNodeIterator signalItr(_signal_node);
      if( !dstItr.findFirst("PHCompositeNode", "GEA") )
      {
        cout << "Copying GEA node from SIGNAL to DST" << endl;
        PHCompositeNode* gea_node = static_cast<PHCompositeNode*>(signalItr.findFirst("PHCompositeNode", "GEA"));
        if( gea_node ) dst_node->addNode(gea_node);
      }

      if( !dstItr.findFirst("PHCompositeNode", "EVA") )
      {
        cout << "Copying EVA node from SIGNAL to DST" << endl;
        PHCompositeNode* eva_node = static_cast<PHCompositeNode*>(signalItr.findFirst("PHCompositeNode", "EVA"));
        if( eva_node ) dst_node->addNode(eva_node);
      }

    }

    // RPC signal hits
    TMutNode<TRpcHitMap>::new_node(_ioc_signal_node,"TRpcHitMap");

// following code is very dangerous and should not be necessary
// (Hugo 10/5/2009)
//
//     // copy signal MC hits onto rpcoo_node
//     {
//       PHNodeIterator iter(_ioc_signal_node);
//       PHDataNode<TRpcMCHit> *node = static_cast< PHDataNode<TRpcMCHit>* >(iter.findFirst("PHDataNode","TRpcMCHitMap"));
//       _rpc_node->addNode(node);
//     }

//     // copy signal MC tracks onto rpc_node
//     {
//
//       // try read MCTrkMap from signal_node
//       try { TMutNode<TMutMCTrkMap>::find_node(_ioc_signal_node, "TMutMCTrkMap"); }
//       catch( exception &e ) {
//
//         // try read the map from signal node and make persistant
//         TMutMCTrkMap* mctrk_map = TMutNode<TMutMCTrkMap>::new_dst_input_node(_ioc_signal_node, "TMutMCTrkMap", _signal_node, "TMutMCTrk");
//         if (dst_node) mctrk_map->make_persistant(dst_node,"TMutMCTrk");
//         RPCOO::TRACE( "FvtxUnpackSim::set_interface_ptrs - TMutMCTrkMap created and made persistant.\n" );
//
//       }
//
//       PHNodeIterator iter(_ioc_signal_node);
//       PHDataNode<TMutMCTrk> *node = static_cast< PHDataNode<TMutMCTrk>* >(iter.findFirst("PHDataNode","TMutMCTrkMap"));
//       _rpc_node->addNode(node);
//
//     }

    // from MC background node
    if(_mode == MC_SIGNAL_MC_BG) {

      TMutNode<TRpcMCHitMap>::new_dst_input_node(_ioc_background_node, "TRpcMCHitMap", _background_node, "TRpcMCHit");
      TMutNode<TRpcHitMap>::new_node(_ioc_background_node,"TRpcHitMap");

      // try load MCTrkMap from background node
      try{ TMutNode<TMutMCTrkMap>::find_node( _ioc_background_node, "TMutMCTrkMap" ); }
      catch(std::exception& e){
        TMutNode<TMutMCTrkMap>::new_dst_input_node(_ioc_background_node, "TMutMCTrkMap", _background_node, "TMutMCTrk");
      }

    }

    // from RD background node
    else if(_mode == MC_SIGNAL_REAL_BG)
    TMutNode<TRpcHitMap>::new_dst_input_node(_ioc_background_node, "TRpcHitMap", _background_node, "TRpcHit");

    // if no background, creade background Hit map anyway to avoid later exceptions in embedding module
    else if( _mode == MC_SIGNAL_NO_BG )
    TMutNode<TRpcHitMap>::new_node(_ioc_background_node,"TRpcHitMap");

    // RPC merged hits (on RPCOO node)
    TRpcHitMap* rpc_hit_map = TMutNode<TRpcHitMap>::new_node(_rpc_node, "TRpcHitMap");
    if (dst_node) rpc_hit_map->make_persistant(dst_node,"TRpcHit");

  } catch(std::exception& e){ cout << e.what() << endl; }

  return 0;

}

//______________________________________________________
int RpcUnpackSim::set_module_ptrs(PHCompositeNode *top_node)
{
  try{
    // Module parameters for signal node
    mRpcResponsePar *mRpcResponse_par = TMutNode<mRpcResponsePar>::new_node( _ioc_signal_node, "mRpcResponsePar" );
    mRpcResponse_par->set_verbosity( RPCOO::NONE );
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    {
      RPCOO::PRINT( cout, "MC signal parameters" );
      mRpcResponse_par->print();
    }

    // Module parameters for MC background node
    if(_mode == MC_SIGNAL_MC_BG) {
      mRpcResponsePar *mRpcResponse_par = TMutNode<mRpcResponsePar>::new_node( _ioc_background_node, "mRpcResponsePar" );
      mRpcResponse_par->set_verbosity( RPCOO::NONE );
      if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
      {
        RPCOO::PRINT( cout, "MC background parameters" );
        mRpcResponse_par->print();
      }
    }

    // embedding
    mRpcEmbedPar* mRpcEmbed_par =	TMutNode<mRpcEmbedPar>::new_node( _rpc_node, "mRpcEmbedPar" );
    mRpcEmbed_par->set_verbosity( RPCOO::NONE );
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    mRpcEmbed_par->print();

  } catch(std::exception& e){ cout << e.what() << endl; }

  return 0;
}


//______________________________________________________
int RpcUnpackSim::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();

  // Call the response
  try {

    // define pointers
    set_node_ptrs(top_node);

    // load external vertex if needed
    load_vertex_if_needed( _signal_node );

    // we need to read the map explicitely due to the Muon Unpacker
    TRpcMCHitMap* fg_mc_hit_map = TMutNode<TRpcMCHitMap>::find_node( _ioc_signal_node ,"TRpcMCHitMap");
    fg_mc_hit_map->read_array( _signal_node );
    _mRpcResponse_mod.event( _ioc_signal_node );

    if(_mode == MC_SIGNAL_MC_BG)
    {

      TRpcMCHitMap* bg_mc_hit_map = TMutNode<TRpcMCHitMap>::find_node( _ioc_background_node ,"TRpcMCHitMap");
      bg_mc_hit_map->read_array( _background_node );
      _mRpcResponse_mod.event( _ioc_background_node );

    } else if (_mode == MC_SIGNAL_REAL_BG) {

      TRpcHitMap* bg_hit_map = TMutNode<TRpcHitMap>::find_node( _ioc_background_node ,"TRpcHitMap");
      bg_hit_map->read_array( _background_node );

    }

    _mRpcEmbed_mod.event( _ioc_signal_node, _ioc_background_node, top_node );

  } catch (exception& e) {
    RPCOO::TRACE(e.what());
  }

  write_maps_if_needed();
  _timer.get()->stop();

  return 0;
}

//______________________________________________________
int RpcUnpackSim::End(PHCompositeNode* top_node)
{
  // print this module timer statistics
  //  _timer.get()->print_stat();
  return 0;
}
