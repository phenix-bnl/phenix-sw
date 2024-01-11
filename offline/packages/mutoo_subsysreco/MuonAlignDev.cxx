// $Id: MuonAlignDev.cxx,v 1.33 2011/07/14 22:27:09 pinkenbu Exp $
/*!
\file    MuonAlignDev.cxx
\brief   mutoo zerofield data reconstruction event loop for alignment work
\author  MinJung Kweon : _modify MuonDev.cxx  
\version $Revision: 1.33 $
\date    $Date: 2011/07/14 22:27:09 $
*/

#include<EventHeader.h>
#include<mMuiFitRoadO.h>
#include<MutGeom.h>
#include<recoConsts.h>

#include<mMuiFitRoadOPar.h>
#include<mMutMuiRoadPar.h>

#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutErrorStats.h>
#include<TMutDatabaseInit.h>

#include"MuonAlignDev.h"

using namespace std;

//______________________________________________________
MuonAlignDev::MuonAlignDev() : 
  MuonSubsysReco( "MUONALIGNDEV" ),
  _flags( NONE ),
  _mMutStraightFit_par(0),
  _timer( PHTimeServer::get()->insert_new("MUONALIGNDEV") )
{}


//______________________________________________________
int MuonAlignDev::Init(PHCompositeNode *top_node)
{
  
  MuonSubsysReco::Init( top_node );
  
  MUTOO::PRINT( cout, "MuonAlignDev::Init" );
  cout << "flags: " << _flags << endl;
  cout << "MAGNETS_ON      : " << (get_flag( MAGNETS_ON ) ? "true":"false" ) << endl;  
  cout << "SKIP_CLUSTERING : " << (get_flag( SKIP_CLUSTERING ) ? "true":"false" ) << endl;  
  cout << "SKIP_VERTEX_FIT : " << (get_flag( SKIP_VERTEX_FIT ) ? "true":"false" ) << endl;  
  MUTOO::PRINT( cout, "**" );
  return 0;

}

//______________________________________________________
int MuonAlignDev::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT( cout, "MuonAlignDev::InitRun" );
  
  // Create Node Tree
  CreateNodeTree(top_node);
  
  // Do the DB initialization
  TMutDatabaseInit::initialize(top_node);  
  
  return 0;
  
}

//______________________________________________________
int MuonAlignDev::CreateNodeTree(PHCompositeNode *top_node)
{
  
  MUTOO::PRINT( cout, "MuonAlignDev::CreateNodeTree" );
  
  // Instantiate nodes for mutoo containers
  {
    PHNodeIterator nodeItr(top_node);
    _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!_mutoo_node){
      _mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(_mutoo_node);
    }
  }
  
  // Instantiate nodes for muioo containers
  {
    PHNodeIterator nodeItr(top_node);
    _muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
    if(!_muioo_node){
      _muioo_node = new PHCompositeNode("MUIOO");
      top_node->addNode(_muioo_node);
    }
  }
  
  {
    PHNodeIterator nodeItr(top_node);
    _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!_dst_node) {
      _dst_node = new PHCompositeNode("DST");
      top_node->addNode(_dst_node);
    }
  }
  
  // stores local pointer to recoconst singleton
  recoConsts *rc = recoConsts::instance();
  
  // Interface Object Containers (IOCS)
  if( !get_flag( SKIP_CLUSTERING ) )
  { 
    cout << "MuonAlignDev::CreateNodeTree - making persistent maps suited for clustering" << endl;
    TMutNode<TMutClusMap>::new_node(_mutoo_node, "TMutClusMap")->make_persistant(_dst_node,"TMutClus");
    TMutNode<TMutCoordMap>::new_node(_mutoo_node, "TMutCoordMap")->make_persistant(_dst_node,"TMutCoord");
    TMutNode<TMutGapCoordMap>::new_node(_mutoo_node, "TMutGapCoordMap")->make_persistant(_dst_node,"TMutGapCoord");
  }

  TMutNode<TMutStubMap>::new_node(_mutoo_node, "TMutStubMap")->make_persistant(_dst_node,"TMutStub");
  TMutNode<TMutTrkMap>::new_node(_mutoo_node, "TMutTrkMap")->make_persistant(_dst_node,"TMutTrk");
  TMutNode<TMutVtxMap>::new_node(_mutoo_node, "TMutVtxMap")->make_persistant(_dst_node,"TMutVtx");
    
  // Change the default values here.
  mMuiFitRoadOPar* mMuiFitRoadO_par = TMutNode<mMuiFitRoadOPar>::new_node(_muioo_node,"mMuiFitRoadOPar");
  mMuiFitRoadO_par->set_verbosity(MUIOO::SOME); 
  
  // cluster finding
  mMutFindClusPar* mMutFindClus_par = TMutNode<mMutFindClusPar>::new_node(_mutoo_node,"mMutFindClusPar");
  mMutFindClus_par->set_max_cluster_width(25);
  
  // cluster fit
  mMutFitClusPar* mMutFitClus_par = TMutNode<mMutFitClusPar>::new_node(_mutoo_node,"mMutFitClusPar");
  mMutFitClus_par->set_multi_track_fit(true);
  
  // gap coordinates
  TMutNode<mMutFindGapCoordPar>::new_node(_mutoo_node,"mMutFindGapCoordPar");
  
  // stub finding and fit
  TMutNode<mMutFindStubPar>::new_node(_mutoo_node,"mMutFindStubPar");
  TMutNode<mMutStubFitPar>::new_node(_mutoo_node,"mMutStubFitPar");
  
  // track finding
  mMutFindTrackPar* mMutFindTrack_par = TMutNode<mMutFindTrackPar>::new_node(_mutoo_node,"mMutFindTrackPar");
  mMutFindTrack_par->set_use_stub_finder_windows(false);
  
  // in zero field data, use STUBFIT instead of BPFIT
  mMutFindTrack_par->set_window_mode(mMutFindTrackPar::STUBFIT);
  mMutFindTrack_par->set_mode(mMutFindTrackPar::USE_MUID);
  mMutFindTrack_par->set_muid_use_golden(true);
  mMutFindTrack_par->set_do_evaluation(false);
  
  // stubfinder runtime parameters
  TMutStubFinder::set_w_prox_cut(0.5);
  TMutStubFinder::set_dca_cut(0.4);
  TMutStubFinder::set_do_evaluation( false );
  
  TMutStubFinder::set_min_coord_1( 4 );
  TMutStubFinder::set_min_coord_2( 3 );
  TMutStubFinder::set_min_coord_3( 2 );
  TMutStubFinder::set_verbosity( MUTOO::NONE );
  
  // bend plane fit
  TMutNode<mMutBPFitPar>::new_node(_mutoo_node,"mMutBPFitPar");
  
  // ghost rejection
  TMutNode<mMutRejectTrackPar>::new_node(_mutoo_node,"mMutRejectTrackPar");
  
  // track fit
  _mMutStraightFit_par = TMutNode<mMutStraightFitPar>::new_node(_mutoo_node,"mMutStraightFitPar");
  
  // road to track association
  TMutNode<mMutMuiRoadPar>::new_node(_mutoo_node,"mMutMuiRoadPar");
  
  // vertex fit
  mMutFitVtxPar* mMutFitVtx_par = TMutNode<mMutFitVtxPar>::new_node(_mutoo_node,"mMutFitVtxPar"); 
  mMutFitVtx_par->set_single_trk_fit( true );
  
  // configure ErrorStat singleton
  if( rc->get_IntFlag("MUTOO_ERRORSTATS", 0 ) )
    TMutErrorStats::initialize_ntuple();  
  
  // dump parameters if needed.
  if( rc->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) {
    mMuiFitRoadO_par->print();
    mMutFindTrack_par->print();
    _mMutStraightFit_par->print();
    mMutFitVtx_par->print();
  }
  
  // Print the current parameters to cout
  print_parameters();
  MUTOO::PRINT( cout, "**" );
  return 0;
}

//-----------------------------------------------------
// Print the parameters so we know they're set right
void MuonAlignDev::print_parameters() const
{
  mMutFindTrackPar* mMutFindTrack_par = TMutNode<mMutFindTrackPar>::find_node( _mutoo_node, "mMutFindTrackPar" );  
  
  MUTOO::PRINT(cout, "MuonAlignDev Parameters");
  
  cout << "Use stub finder windows: "  << ( mMutFindTrack_par->get_use_stub_finder_windows() ? "true":"false" ) << endl;
  
  cout << "StubFinder min coords for stations 1,2,3: ("
    <<  TMutStubFinder::get_min_coord_1() << ", "
    <<  TMutStubFinder::get_min_coord_2() << ", "
    <<  TMutStubFinder::get_min_coord_3() << ")" << endl;
  
  cout << "Max # of clone tracks in mMutFindTrack: " 
    <<  mMutFindTrack_par->get_max_n_tracks() << endl;
  
  cout << "Max # of stub bifurcations in TMutStubFinder: "
    <<  TMutStubFinder::get_max_n_stubs() << endl;
  MUTOO::PRINT();
}

//______________________________________________________
int MuonAlignDev::process_event(PHCompositeNode *top_node)
{
  
  _timer.get()->restart();
  
  recoConsts *rc = recoConsts::instance();
  TMutErrorStats::clear_event();
  
  // Call MUTOO modules for track momentum reconstruction and vertex finding
  try {
    
    rc->set_IntFlag("DATABASE", 1);
    
    _mMuiFitRoadO_mod.event( _muioo_node );
    
    if( !get_flag( SKIP_CLUSTERING ) )
    {
      _mMutFindClus_mod.event(_mutoo_node);
      _mMutFitClus_mod.event(_mutoo_node);
      _mMutFindGapCoord_mod.event(_mutoo_node);
    }
    
    _mMutFindTrack_mod.event(top_node);
    _mMutBPFit_mod.event(top_node);
    _mMutRejectTrack_mod.event(top_node);
    
    if( !get_flag( MAGNETS_ON ) ) 
    {   
      
      // first straight track fit (without muid)
      _mMutStraightFit_par->set_use_muid( false );
      _mMutStraightFit_mod.event( top_node );
      
      // road to track association
      _mMutMuiRoad_mod.event( top_node );
      
      // second straight track fit (with muid)
      _mMutStraightFit_par->set_use_muid( true );
      _mMutStraightFit_mod.event( top_node );
      
    }
    
    // vertex fit
    if(!get_flag( SKIP_VERTEX_FIT ) )
    { _mMutFitVtx_mod.event( top_node ); }
      
  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }  
  
  write_maps_if_needed();
  TMutErrorStats::set_event_stats(top_node, static_cast<short unsigned int> (_timer.get()->elapsed()) );
  TMutErrorStats::write_event();
  
  _timer.get()->stop();
  return 0;
}

//______________________________________________________
int MuonAlignDev::End(PHCompositeNode* top_node) 
{
  TMutErrorStats::finish();
//   _timer.get()->print_stat();
  
  try {
    
    // write stub evaluation tree if required
    if( TMutStubFinder::get_do_evaluation() )
    { TMutStubFinder::finish_evaluation(); }
    
    // write mMutFindTrack module tree to file, if required
    if( TMutNode<mMutFindTrackPar>::find_node(top_node,"mMutFindTrackPar")->get_do_evaluation() ) 
    { _mMutFindTrack_mod.finish_evaluation(); }
    
  } catch( exception &e ) { cerr << e.what() << endl; }
  
  return 0;
}
