/*!
  \file FvtxUnpackDST.cxx
  \ingroup supermodules
  \author : Zhengyun You
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

#include "FvtxUnpackDST.h"

using namespace std;

//______________________________________________________
FvtxUnpackDST::FvtxUnpackDST( const char* name, unsigned int mode ) :
  MuonSubsysReco( name ),
  _fvtx_node(0),
  _signalNodeName("TOP"),
  _signal_node(0),
  _ioc_signal_node(0),
  _backgroundNodeName("BACKGROUND"),
  _background_node(0),
  _ioc_background_node(0),
  _mode( mode ),
  _timer( new PHTimer( name ) )
{}

//______________________________________________________
int FvtxUnpackDST::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT(cout, "FvtxUnpackDST::InitRun");

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "FvtxUnpackDST::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  if ( rc->FlagExist("EMBED_REAL_TOPNODE") )
  {
    cout << "FvtxUnpackDST::InitRun - reading _backgroundNodeName from recoConst EMBED_REAL_TOPNODE" << endl;
    SetBackgroundNodeName( rc->get_CharFlag("EMBED_REAL_TOPNODE") );
  }

  cout << "FvtxUnpackDST::InitRun - _signalNodeName : " << _signalNodeName << endl;
  cout << "FvtxUnpackDST::InitRun - _backgroundNodeName: " << _backgroundNodeName << endl;

  try {

    set_node_ptrs(top_node);
    set_interface_ptrs(top_node);
    set_module_ptrs(top_node);

  } catch(const std::exception& e){ cout << e.what() << endl; }

  return 0;
}

//______________________________________________________
void FvtxUnpackDST::SetMode(unsigned int mode)
{
  _mode = mode;
  boost::array<const char*,9> mode_string = {{
    "REAL_SIGNAL_REAL_BG",
    "REAL_SIGNAL_MC_BG",
    "REAL_SIGNAL_NO_BG" }};

  MUTOO::PRINT(cout, "FvtxUnpackDST SetMode");
  cout << "Mode set to " << mode_string[_mode] << endl;
  MUTOO::PRINT(cout, "**");

}

//______________________________________________________
int FvtxUnpackDST::set_node_ptrs(PHCompositeNode *top_node)
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

  // FVTX working node _fvtx_node -- create if it doesn't already exist
  _fvtx_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
  if(!_fvtx_node)
  {
    _fvtx_node = new PHCompositeNode("FVTXOO");
    top_node->addNode(_fvtx_node);
  }

  _ioc_signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
  if(!_ioc_signal_node)
  {
    cout << "FvtxUnpackDST::SetNodePtrs - creating ioc_signal_node" << endl;
    _ioc_signal_node = new PHCompositeNode("FVTXOO");
    signal_top_node->addNode(_ioc_signal_node);
  }

  // Background node -- Maps associated with background DST -- create if it doesn't already exist
  _ioc_background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
  if(!_ioc_background_node)
  {

    _ioc_background_node = new PHCompositeNode("FVTXOO");
    background_top_node->addNode(_ioc_background_node);

  }

  // DST Signal
  _signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!_signal_node) throw runtime_error(DESCRIPTION("Cannot locate signal DST node"));

  // DST Background -- error condition if it SHOULD exist but doesn't
  if(_mode != REAL_SIGNAL_NO_BG)
  {

    _background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!_background_node) throw runtime_error(DESCRIPTION("Cannot locate background DST node"));

  }

  return 0;

}

//______________________________________________________
int FvtxUnpackDST::set_interface_ptrs(PHCompositeNode *top_node)
{

  try{

    // copy signal MC hits to output DST node
    PHNodeIterator nodeItr(top_node);

    TMutNode<TFvtxHitMap>::new_dst_input_node(_ioc_signal_node, "TFvtxHitMap", _signal_node, "TFvtxHit");

    // from MC background node
    if(_mode == REAL_SIGNAL_MC_BG) {

      TMutNode<TFvtxMCHitMap>::new_dst_input_node(_ioc_background_node, "TFvtxMCHitMap", _background_node, "TFvtxMCHit");
      TMutNode<TFvtxHitMap>::new_node(_ioc_background_node,"TFvtxHitMap");
    }

    // from RD background node
    else if(_mode == REAL_SIGNAL_REAL_BG)
      TMutNode<TFvtxHitMap>::new_dst_input_node(_ioc_background_node, "TFvtxHitMap", _background_node, "TFvtxHit");

    // if no background, creade background Hit map anyway to avoid later exceptions in embedding module
    else if(_mode == REAL_SIGNAL_NO_BG)
      TMutNode<TFvtxHitMap>::new_node(_ioc_background_node,"TFvtxHitMap");

    // FVTX merged hits (on FVTXOO node)
    TMutNode<TFvtxHitMap>::new_node(_fvtx_node, "TFvtxHitMap");

    // top_node->print();

  } catch(std::exception& e){ cout << e.what() << endl; }

  return 0;

}

//______________________________________________________
int FvtxUnpackDST::set_module_ptrs(PHCompositeNode *top_node)
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
    if(_mode == REAL_SIGNAL_MC_BG) {
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
int FvtxUnpackDST::process_event(PHCompositeNode *top_node)
{

  _timer->restart();

  // Call the response
  try {

    set_node_ptrs(top_node);

    TMutNode<TFvtxHitMap>::find_node( _ioc_signal_node ,"TFvtxHitMap")->read_array( _signal_node );

    if(_mode == REAL_SIGNAL_MC_BG) {
      TFvtxMCHitMap* bg_mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node( _ioc_background_node ,"TFvtxMCHitMap");
      bg_mc_hit_map->read_array( _background_node );
      _mFvtxResponse_mod.event( _ioc_background_node );
    }
    else if (_mode == REAL_SIGNAL_REAL_BG) {
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
int FvtxUnpackDST::End(PHCompositeNode* top_node)
{
  _timer->print_stat();
  return 0;
}
