// $Id: MuonUnpackPRDF.cxx,v 1.35 2013/04/25 15:35:04 brooks Exp $

#include<EventHeader.h>

#include<mMuiRawUnpackPar.h>
#include<mMutCalibratePar.h>
#include<mMutUnpackPar.h>
#include<mMutZeroSupPar.h>

#include<recoConsts.h>
#include<TMuiHitMapO.h>
#include<TMutHitMap.h>

#include <MUTOO_ROTATION.h>

#include "MuonUnpackPRDF.h"
#include "MuonUtil.h"

using namespace std;

//______________________________________________________
MuonUnpackPRDF::MuonUnpackPRDF( const char* name ) :
  MuonSubsysReco( name ),
  _flags( NONE ),
  _timer( PHTimeServer::get()->insert_new( name ))
{
   _verbosity = MUTOO::SOME;
   _zero_mode = 0;
}

//_____________________________________________________
MuonUnpackPRDF::~MuonUnpackPRDF( void )
{
 // PHTimeServer* timeserver = PHTimeServer::get();
 // delete timeserver;
 // return;
}


//______________________________________________________
int MuonUnpackPRDF::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );

  if(_verbosity>=MUTOO::ALOT) {
    MUTOO::PRINT( cout, "MuonUnpackPRDF::Init" );
    cout << "flags: " << endl;
    cout << "SKIP_ZERO_SUPPRESSION : " << (get_flag( SKIP_ZERO_SUPPRESSION ) ? "true":"false" ) << endl;
    cout << "SKIP_MUTR             : " << (get_flag( SKIP_MUTR ) ? "true":"false" ) << endl;
    cout << "SKIP_MUID             : " << (get_flag( SKIP_MUID ) ? "true":"false" ) << endl;
    MUTOO::PRINT( cout, "**" ); }
  
  return 0;

}

//______________________________________________________
int MuonUnpackPRDF::InitRun(PHCompositeNode *top_node)
{
  if(_verbosity>=MUTOO::ALOT) {
    MUTOO::PRINT(cout, "MuonUnpackPRDF::InitRun" ); }

  // Create Node Tree
  CreateNodeTree(top_node);

  bool do_mut( !get_flag( SKIP_MUTR ) );
  bool do_mui( !get_flag( SKIP_MUID ) );
  MuonUtil::initialize_database( top_node, do_mut, do_mui );

  return 0;
}

//______________________________________________________
int MuonUnpackPRDF::CreateNodeTree(PHCompositeNode *top_node)
{

  // Instantiate nodes for mutoo containers
  {
    PHNodeIterator nodeItr(top_node);
    mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!mutoo_node &&  !get_flag( SKIP_MUTR ) ){
      mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(mutoo_node);
    }
  }

  // Instantiate nodes for muioo containers
  {
    PHNodeIterator nodeItr(top_node);
    muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!muioo_node && !get_flag( SKIP_MUID ) )
    {
      muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode(muioo_node);
    }
  }

  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if(!dst_node) {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }

  if( !get_flag( SKIP_MUTR ) ) {
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_node(mutoo_node, "TMutHitMap");
    hit_map->make_persistant(dst_node,"TMutHit");
  }
  if( !get_flag( SKIP_MUID ) )
  {
    TMuiHitMapO*	mui_hit_map = TMutNode<TMuiHitMapO>::new_node(muioo_node,"TMuiHitMapO");
    mui_hit_map->make_persistant(dst_node,"TMuiHitO");
  }

  // prdf unpackers
  if( !get_flag( SKIP_MUTR ) ) {

    mMutUnpackPar* mMutUnpack_par = TMutNode<mMutUnpackPar>::new_node(mutoo_node,"mMutUnpackPar");
    mMutZeroSupPar* mMutZeroSup_par = TMutNode<mMutZeroSupPar>::new_node( mutoo_node, "mMutZeroSupPar" );
    if (_zero_mode) mMutZeroSup_par->set_mode( get_zero_mode() ) ;
    mMutCalibratePar* mMutCalibrate_par = TMutNode<mMutCalibratePar>::new_node(mutoo_node,"mMutCalibratePar");

    mMutUnpack_par->set_verbosity( MUTOO::NONE );
    mMutUnpack_par->set_check_user_word( true );
    mMutUnpack_par->set_check_detector_id( true );
    mMutZeroSup_par->set_verbosity( MUTOO::NONE );

    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    {
      mMutUnpack_par->print();
      mMutZeroSup_par->print();
      mMutCalibrate_par->print();
    }

  }

  if( !get_flag( SKIP_MUID ) )
  {

    mMuiRawUnpackPar* mMuiRawUnpack_par = TMutNode<mMuiRawUnpackPar>::new_node(muioo_node,"mMuiRawUnpackPar");
    mMuiRawUnpack_par->set_verbosity( MUIOO::NONE );

  }

  return 0;
}

//______________________________________________________
int MuonUnpackPRDF::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();

  static int counter = 0;

  // Call MUTOO/MUIOO modules for unpacking and calibration
  try {

    // set rotation mode
    if( counter %2 )  MUTOO::set_rotation_mode( MUTOO::CLOCKWISE );
    else MUTOO::set_rotation_mode( MUTOO::COUNTERCLOCKWISE );

    counter++;

    // load vertex
    load_vertex_if_needed( top_node );

    if( !get_flag( SKIP_MUTR ) )
    {
      // upacking
      _mMutUnpack_mod.event(top_node);

      // zero suppression
      if( !get_flag( SKIP_ZERO_SUPPRESSION ) )
      { _mMutZeroSup_mod.event( top_node ); }

      // calibrations
      _mMutCalibrate_mod.event(top_node);

    }

    if( !get_flag( SKIP_MUID ) )
    {

      // unpacking
      _mMuiRawUnpack_mod.event(top_node);

    }

  } catch (std::exception& e) {
    MUTOO::TRACE(e.what());
  }

  //! write persistant nodes
  write_maps_if_needed();

  _timer.get()->stop();

  return 0;
}

//______________________________________________________
int MuonUnpackPRDF::End(PHCompositeNode* top_node)
{

  // print this module timer statistics
//   _timer.get()->print_stat();

  // unpacker summary
  _mMutUnpack_mod.print_summary();

  // zero suppression summary
  if( !get_flag( SKIP_ZERO_SUPPRESSION ) )
  { _mMutZeroSup_mod.print_summary(); }

  return 0;
}
