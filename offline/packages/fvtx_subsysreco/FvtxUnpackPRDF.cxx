// $Id: FvtxUnpackPRDF.cxx,v 1.7 2015/07/02 18:41:32 jinhuang Exp $

#include<EventHeader.h>
#include<Fun4AllReturnCodes.h>
#include<iostream>

//#include<mMuiRawUnpackPar.h>
//#include<mMutCalibratePar.h>
#include<mFvtxUnpackPar.h>
#include <FvtxGeom.h>
#include <TMutNode.h>
#include <RawDataCheck.h>

//#include<mMutZeroSupPar.h>

#include<recoConsts.h>
#include<TFvtxHitMap.h>
#include<FVTXOO.h>
#include <TFvtxGlobalParCntrl.h>

#include "FvtxUnpackPRDF.h"
//#include "MuonUtil.h"

using namespace std;

//______________________________________________________
FvtxUnpackPRDF::FvtxUnpackPRDF(const char* name) :
    MuonSubsysReco(name), _flags(NONE), _timer(
        PHTimeServer::get()->insert_new(name))
{
}

//______________________________________________________
int
FvtxUnpackPRDF::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init(top_node);

  // Create Node Tree
  CreateNodeTree(top_node);

  FVTXOO::PRINT(cout, "FvtxUnpackPRDF::Init");
  cout << "flags: " << endl;
  cout << "SKIP_ZERO_SUPPRESSION : "
      << (get_flag(SKIP_ZERO_SUPPRESSION) ? "true" : "false") << endl;
  cout << "SKIP_FVTX             : " << (get_flag(SKIP_FVTX) ? "true" : "false")
      << endl;
  FVTXOO::PRINT(cout, "**");

  return 0;
}

//______________________________________________________
int
FvtxUnpackPRDF::InitRun(PHCompositeNode *top_node)
{
  FVTXOO::PRINT(cout, "FvtxUnpackPRDF::InitRun");

  TFvtxGlobalParCntrl::init_run();
  check_parameters(top_node);

  const bool frame_check_mode = recoConsts::instance()->get_IntFlag(
      "FVTX_UNPACK_EVENTFRAME_CHECK_MODE", 0);
  if (frame_check_mode)
    {
      _mFvtxUnpack_mod.frame_check_mode(true);
    }

  const string online_debug_tree = recoConsts::instance()->get_CharFlag(
      "SAVE_FVTX_UNPACK_ONLINE_DEBUG_TREE", "");
  if (online_debug_tree.length() > 0)
    {
      if (!_mFvtxUnpack_mod.save_online_debug_tree(online_debug_tree))
        {
          cout
              << "FvtxUnpackPRDF::InitRun - failed to make online_debug_tree in "
              << online_debug_tree << endl;
        }
    }

  return 0;
}

//______________________________________________________
int
FvtxUnpackPRDF::CreateNodeTree(PHCompositeNode *top_node)
{

  // Instantiate nodes for mutoo containers
    {
      PHNodeIterator nodeItr(top_node);
      fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "FVTXOO"));
      if (!fvtxoo_node && !get_flag(SKIP_FVTX))
        {
          fvtxoo_node = new PHCompositeNode("FVTXOO");
          top_node->addNode(fvtxoo_node);
        }
    }

    {
      PHNodeIterator nodeItr(top_node);
      dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "DST"));
      if (!dst_node)
        {
          dst_node = new PHCompositeNode("DST");
          top_node->addNode(dst_node);
        }
    }

  if (!get_flag(SKIP_FVTX))
    {
      TFvtxHitMap* hit_map = TMutNode<TFvtxHitMap>::new_node(fvtxoo_node,
          "TFvtxHitMap");
      hit_map->make_persistant(dst_node, "TFvtxHit");
    }

  // prdf unpackers
  if (!get_flag(SKIP_FVTX))
    {

      mFvtxUnpackPar* mFvtxUnpack_par = TMutNode<mFvtxUnpackPar>::new_node(
          fvtxoo_node, "mFvtxUnpackPar");

      mFvtxUnpack_par->set_verbosity(
          (FVTXOO::Verbosity) (recoConsts::instance()->get_IntFlag(
              "VERBOSITY_FVTX_UNPACK", (int) (FVTXOO::NONE))));

      mFvtxUnpack_par->set_check_user_word(false);
      mFvtxUnpack_par->set_check_detector_id(false);

    }

  return 0;
}
//______________________________________________________
int
FvtxUnpackPRDF::check_parameters(PHCompositeNode */*top_node*/)
{
  // prdf unpackers
  if (!get_flag(SKIP_FVTX))
    {

      mFvtxUnpackPar* mFvtxUnpack_par = TMutNode<mFvtxUnpackPar>::find_node(
          fvtxoo_node, "mFvtxUnpackPar");

      if (recoConsts::instance()->get_IntFlag("PRINT_FVTXOO_PARAMETERS", 1))
        {
          mFvtxUnpack_par->print();
        }

    }

  return 0;
}

//______________________________________________________
int
FvtxUnpackPRDF::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();

  static int counter = 0;

  // Call FVTXOO modules for unpacking and calibration
  try
    {

      counter++;

      // load vertex
      load_vertex_if_needed(top_node);

      if (!get_flag(SKIP_FVTX))
        {
          // upacking
          _mFvtxUnpack_mod.process_event(top_node);

          // zero suppression
//      if( !get_flag( SKIP_ZERO_SUPPRESSION ) )
//      { _mMutZeroSup_mod.event( top_node ); }

          // calibrations
//      _mMutCalibrate_mod.event(top_node);

        }

    }
  catch (std::exception& e)
    {
      FVTXOO::TRACE(e.what());
    }

  //! write persistant nodes
  write_maps_if_needed();

  _timer.get()->stop();


  if (_mFvtxUnpack_mod.get_fem_errorcode() == mFvtxUnpack::ErrCode_MaxHitPerFEMID
       or _mFvtxUnpack_mod.get_fem_errorcode() == mFvtxUnpack::ErrCode_MaxHitPerPacket)
    {
      cout <<"FvtxPackPRDF::process_event - Error and abort event - "
          <<" FVTX encoutered unpacker error "<<_mFvtxUnpack_mod.get_fem_errorcode()
          <<( _mFvtxUnpack_mod.get_fem_errorcode() == mFvtxUnpack::ErrCode_MaxHitPerFEMID?" (max hit in fem channel)":"" )
          <<( _mFvtxUnpack_mod.get_fem_errorcode() == mFvtxUnpack::ErrCode_MaxHitPerPacket?" (max hit in packet)":"" )
          <<endl;


      // keep a record of what is going on
      Event * _event = TMutNode<Event>::find_node(top_node, "PRDF");
      RawDataCheck *raw = RawDataCheck::instance();
      raw->AddToList(_event, "FVTXTOORAWHITOVERFLOW");

      return ABORTEVENT;
    }


  return EVENT_OK;
}

#include <PHTFileServer.h>
//______________________________________________________
int
FvtxUnpackPRDF::End(PHCompositeNode* top_node)
{

  // print this module timer statistics
  _timer.get()->print_stat();

  // unpacker summary
  _mFvtxUnpack_mod.print_summary();

  // zero suppression summary
//  if( !get_flag( SKIP_ZERO_SUPPRESSION ) )
//  { _mMutZeroSup_mod.print_summary(); }

  _mFvtxUnpack_mod.write_online_debug_tree();

  return 0;
}
