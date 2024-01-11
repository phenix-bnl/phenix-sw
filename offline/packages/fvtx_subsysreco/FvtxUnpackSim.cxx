// $Id: FvtxUnpackSim.cxx,v 1.17 2009/12/10 21:00:33 hpereira Exp $

/*!
  \file FvtxUnpackSim.cxx
  \ingroup supermodules
  \brief FVTX Fast simulation module. Reads a slowsim DST. Creates TFvtx MC Hits
  from MC tracks. Runs the response to create Fvtx hits for later reconstruction
  \warning there is no embedding done so far in this module.
  \author X.R Wang
  \date  2006/07/18
*/

#include <Fun4AllServer.h>

#include <boost/array.hpp>
#include <FVTXOO.h>
#include <FvtxGeom.h>
#include <mFvtxEmbed.h>
#include <mFvtxEmbedPar.h>
#include <mFvtxResponse.h>
#include <PHTimer.h>
#include <recoConsts.h>

#include <TFvtxPisaHitMap.h>
#include <TMutMCTrkMap.h>

#include "FvtxUnpackSim.h"

using namespace std;

//______________________________________________________
FvtxUnpackSim::FvtxUnpackSim( const char* name, unsigned int mode ) :
  MuonSubsysReco( name ),
  _fvtx_node(0),
  _signalNodeName("SIGNAL"),
  _signal_node(0),
  _ioc_signal_node(0),
  _backgroundNodeName("BACKGROUND"),
  _background_node(0),
  _ioc_background_node(0),
  _mode( mode ),
  _timer( new PHTimer( name ) )
{}

//______________________________________________________
int FvtxUnpackSim::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT(cout, "FvtxUnpackSim::InitRun");

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "FvtxUnpackSim::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  if ( rc->FlagExist("EMBED_REAL_TOPNODE") )
  {
    cout << "FvtxUnpackSim::InitRun - reading _backgroundNodeName from recoConst EMBED_REAL_TOPNODE" << endl;
    SetBackgroundNodeName( rc->get_CharFlag("EMBED_REAL_TOPNODE") );
  }

  cout << "FvtxUnpackSim::InitRun - _signalNodeName : " << _signalNodeName << endl;
  cout << "FvtxUnpackSim::InitRun - _backgroundNodeName: " << _backgroundNodeName << endl;

  try {

    set_node_ptrs(top_node);
    set_interface_ptrs(top_node);
    set_module_ptrs(top_node);

  } catch(const std::exception& e){ cout << e.what() << endl; }

  return 0;
}

//______________________________________________________
void FvtxUnpackSim::SetMode(unsigned int mode)
{
  _mode = mode;
  boost::array<const char*,4> mode_string = {{
    "MC_SIGNAL_REAL_BG",
    "MC_SIGNAL_MC_BG",
    "MC_SIGNAL_NO_BG" }};

  MUTOO::PRINT(cout, "FvtxUnpackSim SetMode");
  cout << "Mode set to " << mode_string[_mode] << endl;
  MUTOO::PRINT(cout, "**");

}

//______________________________________________________
int FvtxUnpackSim::set_node_ptrs(PHCompositeNode *top_node)
{

  // Note: The node tree topology is a little confusing -- we need 2 nodes per
  // DST. One has the actual DST resident objects the other has the mutoo/muioo maps.
  // Once per event the objects in the DST are read in a used to poplulate the
  // "smart" mutoo/muioo containers.  We put module parameter tables in the same node
  // as the containers it interacts with, eg mMutResponsePar goes in IOC_SIGNAL.

  // get signal and background top nodes
  Fun4AllServer* se = Fun4AllServer::instance();
  PHCompositeNode* signal_top_node = se->topNode( _signalNodeName );
  PHCompositeNode* background_top_node = se->topNode( _backgroundNodeName );

  // create relevant node iterators
  PHNodeIterator signal_nodeItr( signal_top_node );
  PHNodeIterator background_nodeItr( background_top_node );
  PHNodeIterator nodeItr(top_node);

  // Merged node -- FVTXOO maps used for reconstruction
  _fvtx_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
  if(!_fvtx_node)
  {
    _fvtx_node = new PHCompositeNode("FVTXOO");
    top_node->addNode(_fvtx_node);
  }

  // Signal node -- Maps associated with signal DST -- create if it doesn't already exist
  _ioc_signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_ioc_signal_node)
  {
    cout << "FvtxUnpackSim::SetNodePtrs - creating ioc_signal_node" << endl;
    _ioc_signal_node = new PHCompositeNode("MUTOO");
    signal_top_node->addNode(_ioc_signal_node);
  }

  // Background node -- Maps associated with background DST -- create if it doesn't already exist
  _ioc_background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_ioc_background_node)
  {

    _ioc_background_node = new PHCompositeNode("MUTOO");
    background_top_node->addNode(_ioc_background_node);

  }

  // DST Signal
  _signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!_signal_node) throw runtime_error(DESCRIPTION("Cannot locate signal DST node"));

  // DST Background
  if(_mode != MC_SIGNAL_NO_BG)
  {

    _background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!_background_node) throw runtime_error(DESCRIPTION("Cannot locate background DST node"));

  }

  return 0;

}

//______________________________________________________
int FvtxUnpackSim::set_interface_ptrs(PHCompositeNode *top_node)
{

  try{

    // FVTX maps
    // from signal_node
    TFvtxMCHitMap* fvtx_mchit_map = TMutNode<TFvtxMCHitMap>::new_dst_input_node(_ioc_signal_node, "TFvtxMCHitMap", _signal_node, "TFvtxMCHit");
    TFvtxPisaHitMap* fvtx_pisahit_map = TMutNode<TFvtxPisaHitMap>::new_dst_input_node(_ioc_signal_node, "TFvtxPisaHitMap", _signal_node, "TFvtxPisaHit");

    // copy signal MC hits to output DST node
    PHNodeIterator nodeItr(top_node);
    PHCompositeNode* dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (dst_node) {

      fvtx_mchit_map->make_persistant(dst_node,"TFvtxMCHit");
      fvtx_pisahit_map->make_persistant(dst_node,"TFvtxPisaHit");

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

    // FVTX signal hits
    TMutNode<TFvtxHitMap>::new_node(_ioc_signal_node,"TFvtxHitMap");

    // try load MCTrkMap from signal node
    try{ TMutNode<TMutMCTrkMap>::find_node( _ioc_signal_node, "TMutMCTrkMap" ); }
    catch(std::exception& e){
      std::cout << "FvtxUnpackSim::set_interface_ptrs - creating input TMutMCTrkMap node" << endl;
      TMutNode<TMutMCTrkMap>::new_dst_input_node(_ioc_signal_node, "TMutMCTrkMap", _signal_node, "TMutMCTrk");
    }

    // from MC background node
    if(_mode == MC_SIGNAL_MC_BG) {

      TMutNode<TFvtxMCHitMap>::new_dst_input_node(_ioc_background_node, "TFvtxMCHitMap", _background_node, "TFvtxMCHit");
      TMutNode<TFvtxHitMap>::new_node(_ioc_background_node,"TFvtxHitMap");

      // try load MCTrkMap from background node
      try{ TMutNode<TMutMCTrkMap>::find_node( _ioc_background_node, "TMutMCTrkMap" ); }
      catch(std::exception& e){
        TMutNode<TMutMCTrkMap>::new_dst_input_node(_ioc_background_node, "TMutMCTrkMap", _background_node, "TMutMCTrk");
      }

    }

    // from RD background node
    else if(_mode == MC_SIGNAL_REAL_BG)
      TMutNode<TFvtxHitMap>::new_dst_input_node(_ioc_background_node, "TFvtxHitMap", _background_node, "TFvtxHit");

    // if no background, creade background Hit map anyway to avoid later exceptions in embedding module
    else if( _mode == MC_SIGNAL_NO_BG )
      TMutNode<TFvtxHitMap>::new_node(_ioc_background_node,"TFvtxHitMap");

    // FVTX merged hits (on FVTXOO node)
    TFvtxHitMap* fvtx_hit_map = TMutNode<TFvtxHitMap>::new_node(_fvtx_node, "TFvtxHitMap");
    if (dst_node) fvtx_hit_map->make_persistant(dst_node,"TFvtxHit");

    // top_node->print();

  } catch(std::exception& e){ cout << e.what() << endl; }

  return 0;

}

//______________________________________________________
int FvtxUnpackSim::set_module_ptrs(PHCompositeNode *top_node)
{
  try{
    // Module parameters for signal node
    mFvtxResponsePar *mFvtxResponse_par = TMutNode<mFvtxResponsePar>::new_node( _ioc_signal_node, "mFvtxResponsePar" );
    mFvtxResponse_par->set_verbosity( FVTXOO::NONE );
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    {
      FVTXOO::PRINT( cout, "MC signal parameters" );
      mFvtxResponse_par->print();
    }

    // Module parameters for MC background node
    if(_mode == MC_SIGNAL_MC_BG) {
      mFvtxResponsePar *mFvtxResponse_par = TMutNode<mFvtxResponsePar>::new_node( _ioc_background_node, "mFvtxResponsePar" );
      mFvtxResponse_par->set_verbosity( FVTXOO::NONE );
      if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
      {
        FVTXOO::PRINT( cout, "MC background parameters" );
        mFvtxResponse_par->print();
      }
    }

    // embedding
    mFvtxEmbedPar* mFvtxEmbed_par = TMutNode<mFvtxEmbedPar>::new_node( _fvtx_node, "mFvtxEmbedPar" );
    mFvtxEmbed_par->set_verbosity( FVTXOO::NONE );
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    mFvtxEmbed_par->print();

  } catch(std::exception& e){ cout << e.what() << endl; }

  return 0;
}


//______________________________________________________
int FvtxUnpackSim::process_event(PHCompositeNode *top_node)
{

  _timer->restart();

  // Call the response
  try {

    set_node_ptrs(top_node);

    // read external vertex
    load_vertex_if_needed( _signal_node );

    // we need to read the map explicitely due to the Muon Unpacker
    TMutNode<TFvtxMCHitMap>::find_node( _ioc_signal_node ,"TFvtxMCHitMap")->read_array( _signal_node );
    TMutNode<TFvtxPisaHitMap>::find_node( _ioc_signal_node ,"TFvtxPisaHitMap")->read_array( _signal_node );
    TMutNode<TMutMCTrkMap>::find_node(_ioc_signal_node,"TMutMCTrkMap")->read_array(_signal_node);

    _mFvtxResponse_mod.event( _ioc_signal_node );

    if(_mode == MC_SIGNAL_MC_BG)
    {

      TFvtxMCHitMap* bg_mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node( _ioc_background_node ,"TFvtxMCHitMap");
      bg_mc_hit_map->read_array( _background_node );
      _mFvtxResponse_mod.event( _ioc_background_node );

    } else if (_mode == MC_SIGNAL_REAL_BG) {

      TFvtxHitMap* bg_hit_map = TMutNode<TFvtxHitMap>::find_node( _ioc_background_node ,"TFvtxHitMap");
      bg_hit_map->read_array( _background_node );

    }

    _mFvtxEmbed_mod.event(_ioc_signal_node, _ioc_background_node, top_node);

  } catch (const exception& e) {
    FVTXOO::TRACE(e.what());
  }

  write_maps_if_needed();
  _timer->stop();
  return 0;
}

//______________________________________________________
int FvtxUnpackSim::End(PHCompositeNode* top_node)
{
  _timer->print_stat();
  return 0;
}
