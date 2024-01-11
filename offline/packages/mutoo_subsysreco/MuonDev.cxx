// $Id: MuonDev.cxx,v 1.81 2014/07/03 00:18:51 slash Exp $

/*!
  \file MuonDev.cxx
  \ingroup supermodules
  \brief mutoo reconstruction event loop, twicked to cope with Run4 Au-Au data
  \author Sean Kelly, Hugo Pereira
  \version $Revision: 1.81 $
  \date $Date: 2014/07/03 00:18:51 $
*/

#include <PHCompositeNode.h>
#include <recoConsts.h>

#include <mMutBPFitPar.h>
#include <mMutBPVertexPar.h>
#include <mMutFindClusPar.h>
#include <mMutFindTrackPar.h>
#include <mMutFindVtxPar.h>
#include <mMutFitClusPar.h>
#include <mMutFitVtxPar.h>
#include <mMutMatchCoordPar.h>
#include <mMutMuiRoadPar.h>
#include <mMutRejectTrackPar.h>
#include <mMutStubFitPar.h>

#include <TMutClusterFitEval.h>
#include <TMutErrorStats.h>
#include <TMutGapCoordMap.h>
#include <TMutMathiesonPar.h>
#include <TMutStubMap.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>
#include <PHClassId.hh>

#include "MuonDev.h"

using namespace std;

//______________________________________________________
MuonDev::MuonDev( const char* name ) :
  MuonSubsysReco( name ),
  
  /* 
    "optimal" configuration flags for
    Track finding configuration
    BP fit and cluster fit configuration
  */
  _flags( USE_LOCAL_CLUSTERS | USE_BP_VERTEX_FIXED | USE_CLUS_HYBRID_ERROR | FORCE_THREE_WIDE_CLUS | USE_MUID ),
  
  // timer
  _timer(PHTimeServer::get()->insert_new(name) ),
  
  // evaluation files
  _kal_fit_evaluation_file("")
  
{
   _verbosity = MUTOO::SOME;
}

//______________________________________________________
MuonDev::~MuonDev( void )
{}

//______________________________________________________
int MuonDev::Init(PHCompositeNode *top_node)
{ 

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );
  
  if(_verbosity>=MUTOO::ALOT) {
    MUTOO::PRINT( cout, "MuonDev::Init" );
    cout << "flags: " << _flags << endl;
    cout << "SKIP_KF_ANODES        : " << (get_flag( SKIP_KF_ANODES ) ? "true":"false" ) << endl;
    cout << "USE_LOCAL_CLUSTERS    : " << (get_flag( USE_LOCAL_CLUSTERS ) ? "true":"false" ) << endl;  
    cout << "USE_BP_VERTEX_123     : " << (get_flag( USE_BP_VERTEX_123 ) ? "true":"false" ) << endl;  
    cout << "USE_BP_VERTEX_FIXED   : " << (get_flag( USE_BP_VERTEX_FIXED ) ? "true":"false" ) << endl;  
    cout << "USE_BP_VERTEX_WEIGHTED: " << (get_flag( USE_BP_VERTEX_WEIGHTED) ? "true":"false" ) << endl;  
    cout << "USE_BP_MUID           : " << (get_flag( USE_BP_MUID ) ? "true":"false" ) << endl;  
    cout << "USE_CLUS_HYBRID_ERROR : " << (get_flag( USE_CLUS_HYBRID_ERROR ) ? "true":"false" ) << endl;  
    cout << "FORCE_THREE_WIDE_CLUS : " << (get_flag( FORCE_THREE_WIDE_CLUS ) ? "true":"false" ) << endl;  
    cout << "FORCE_GSL_FIT         : " << (get_flag( FORCE_GSL_FIT ) ? "true":"false" ) << endl;  
    cout << "FORCE_LOOKUP          : " << (get_flag( FORCE_LOOKUP ) ? "true":"false" ) << endl;  
    cout << "NO_CLUSTER_CUTS       : " << (get_flag( NO_CLUSTER_CUTS ) ? "true":"false" ) << endl;  
    cout << "SKIP_CLUSTERING       : " << (get_flag( SKIP_CLUSTERING ) ? "true":"false" ) << endl;  
    cout << "SKIP_TRACK_FINDING    : " << (get_flag( SKIP_TRACK_FINDING ) ? "true":"false" ) << endl;  
    cout << "REMOVE_GHOST_TRACKS   : " << (get_flag( REMOVE_GHOST_TRACKS ) ? "true":"false" ) << endl; 
    cout << "SKIP_VERTEX_FIT       : " << (get_flag( SKIP_VERTEX_FIT ) ? "true":"false" ) << endl;  
    cout << "USE MUID IN TRACKING  : " << (get_flag( USE_MUID ) ? "true":"false" ) << endl;  
    MUTOO::PRINT( cout, "**" ); }
  return 0; 

}

//______________________________________________________
int MuonDev::InitRun(PHCompositeNode *top_node)
{
  if(_verbosity>=MUTOO::ALOT) { MUTOO::PRINT( cout, "MuonDev::InitRun" ); }
  CreateNodeTree(top_node);
  if(_verbosity>=MUTOO::ALOT) { MUTOO::PRINT( cout, "**" ); }
  return 0;
}

//______________________________________________________
int MuonDev::CreateNodeTree(PHCompositeNode *top_node)
{

  // Instantiate nodes for mutoo containers
  {
    PHNodeIterator nodeItr(top_node);
    mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
    if(!mutoo_node){
      mutoo_node = new PHCompositeNode("MUTOO");
      top_node->addNode(mutoo_node);
    }
  }

  {
    PHNodeIterator nodeItr(top_node);
    dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!dst_node) {
      dst_node = new PHCompositeNode("DST");
      top_node->addNode(dst_node);
    }
  }

  // get a pointer to fun4all reconstruction flags
  recoConsts *rc = recoConsts::instance();

  // Interface Object Containers (IOCS)
  if( !get_flag( SKIP_CLUSTERING ) )
  { 
    cout << "MuonDev::CreateNodeTree - making persistent maps suited for clustering" << endl;
    TMutNode<TMutClusMap>::new_node(mutoo_node, "TMutClusMap")->make_persistant(dst_node,"TMutClus");
    TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap")->make_persistant(dst_node,"TMutCoord");
    TMutNode<TMutGapCoordMap>::new_node(mutoo_node, "TMutGapCoordMap")->make_persistant(dst_node,"TMutGapCoord");
  }
  
  TMutNode<TMutStubMap>::new_node(mutoo_node, "TMutStubMap")->make_persistant(dst_node,"TMutStub");
  TMutNode<TMutTrkMap>::new_node(mutoo_node, "TMutTrkMap")->make_persistant(dst_node,"TMutTrk");
  TMutNode<TMutVtxMap>::new_node(mutoo_node, "TMutVtxMap")->make_persistant(dst_node,"TMutVtx");

  // Module parameter tables
  TMutNode<mMutMuiRoadPar>::new_node(mutoo_node,"mMutMuiRoadPar");
  
  // cluster finding 
  mMutFindClusPar* mMutFindClus_par = TMutNode<mMutFindClusPar>::new_node(mutoo_node,"mMutFindClusPar");
  mMutFindClus_par->set_verbosity( MUTOO::NONE );
  mMutFindClus_par->set_max_cluster_width(25);
  
  // decide if cluster cuts are to be applied
  mMutFindClus_par->set_do_cluster_cuts( !get_flag( NO_CLUSTER_CUTS ) );
  
  // cluster fit
  mMutFitClusPar* mMutFitClus_par = TMutNode<mMutFitClusPar>::new_node(mutoo_node,"mMutFitClusPar");
  mMutFitClus_par->set_verbosity( MUTOO::NONE );
  mMutFitClus_par->set_multi_track_fit(true);
  
  // set 2 wide cluster fit to use GSL fit (as opposed to lookup)
  if( get_flag( FORCE_GSL_FIT ) ) 
  {
    mMutFitClus_par->set_single_track_fit_type( TMutClusterFit::GSL_MATHIESON );
    mMutFitClus_par->set_multi_track_fit_type( TMutClusterFit::GSL_MATHIESON );
  } else if( get_flag( FORCE_LOOKUP ) )
  {
    mMutFitClus_par->set_single_track_fit_type( TMutClusterFit::MATHIESON_LOOKUP );
    mMutFitClus_par->set_multi_track_fit_type( TMutClusterFit::MATHIESON_LOOKUP );    
  }
  
  if( get_flag( USE_CLUS_HYBRID_ERROR ) ) TMutClusterFit::set_use_hybridmax_error( true );
  if( get_flag( FORCE_THREE_WIDE_CLUS ) ) TMutClusterFit::set_force_three_wide_clusters( true );

  // gap coordinates finding
  mMutMatchCoordPar* mMutMatchCoord_par = TMutNode<mMutMatchCoordPar>::new_node(mutoo_node,"mMutMatchCoordPar");
  mMutMatchCoord_par->set_verbosity( MUTOO::NONE );
  mMutMatchCoord_par->set_do_evaluation( false );
  mMutMatchCoord_par->set_max_combinations( 0 );
  mMutMatchCoord_par->set_do_refit( false );

  // stub fit
  mMutStubFitPar* mMutStubFit_par = TMutNode<mMutStubFitPar>::new_node(mutoo_node,"mMutStubFitPar");
  mMutStubFit_par->set_use_fast_stub_fit( false );
  mMutStubFit_par->set_use_anodes(false);
  mMutStubFit_par->set_verbosity( MUTOO::NONE );

  // track finding
  mMutFindTrackPar* mMutFindTrack_par = TMutNode<mMutFindTrackPar>::new_node(mutoo_node,"mMutFindTrackPar");
  mMutFindTrack_par->set_verbosity( MUTOO::NONE );
  
  if( get_flag( USE_LOCAL_CLUSTERS ) && !get_flag( SKIP_CLUSTERING ) )
  { mMutFindTrack_par->set_use_local_clusters( true ); }
  
  mMutFindTrack_par->set_use_stub_finder_windows(false);
  mMutFindTrack_par->set_window_mode(mMutFindTrackPar::BPFIT);
  if ( get_flag( USE_MUID ) )
    {
      mMutFindTrack_par->set_mode(mMutFindTrackPar::USE_MUID);
      mMutFindTrack_par->set_muid_use_golden(true);
    }
  else
    {
      mMutFindTrack_par->set_mode(mMutFindTrackPar::NO_MUID);
      mMutFindTrack_par->set_muid_use_golden(false);      
    }
  mMutFindTrack_par->set_do_evaluation(false);
    
  // kalman fit
  mMutKalFitPar* mMutKalFit_par = TMutNode<mMutKalFitPar>::new_node(mutoo_node,"mMutKalFitPar");
  mMutKalFit_par->set_verbosity( MUTOO::NONE );
  mMutKalFit_par->set_use_anodes(mMutStubFit_par->get_use_anodes());
  if( get_flag( SKIP_KF_ANODES ) )
  {
    cout << "MuonDev::CreateNodeTree - skipping KF refit to account for anode corrections" << endl;
    mMutKalFit_par->set_use_anodes( false );
  }

  // track rejection
  mMutRejectTrackPar* mMutRejectTrack_par = TMutNode<mMutRejectTrackPar>::new_node(mutoo_node,"mMutRejectTrackPar");
  mMutRejectTrack_par->set_verbosity( MUTOO::NONE );
  mMutRejectTrack_par->set_remove_rejected( get_flag( REMOVE_GHOST_TRACKS ) );
  
  // bent plane fit
  mMutBPFitPar* mMutBPFit_par = TMutNode<mMutBPFitPar>::new_node(mutoo_node,"mMutBPFitPar");
  
  if( get_flag( USE_BP_VERTEX_123 ) )
  { mMutBPFit_par->set_use_vertex123( true ); }
  
  if( get_flag( USE_BP_VERTEX_FIXED ) || get_flag( USE_BP_VERTEX_WEIGHTED ) )
  {
    mMutBPFit_par->set_use_vertex123( true );
    mMutBPFit_par->set_use_vertex23( true );
  }
  
  if( get_flag( USE_BP_VERTEX_WEIGHTED ) ) mMutBPFit_par->set_use_p_dep_sigma( true );
  
  if( get_flag( USE_BP_MUID ) )
  {
    mMutBPFit_par->set_use_muid_23fit( true );
    mMutBPFit_par->set_use_muid_123fit( true );
  }

  // vertex finding 
  mMutFindVtxPar* mMutFindVtx_par = TMutNode<mMutFindVtxPar>::new_node(mutoo_node,"mMutFindVtxPar");
  mMutFindVtx_par->set_verbosity( MUTOO::NONE );

  // bent plane vertex fit
  mMutBPVertexPar* mMutBPVertex_par = TMutNode<mMutBPVertexPar>::new_node(mutoo_node,"mMutBPVertexPar");
  mMutBPVertex_par->set_verbosity( MUTOO::NONE );

  // vertex single track fit
  mMutFitVtxPar* mMutFitVtx_par = TMutNode<mMutFitVtxPar>::new_node(mutoo_node,"mMutFitVtxPar");
  mMutFitVtx_par->set_verbosity(MUTOO::NONE);
  mMutFitVtx_par->set_single_trk_fit( false );

  // cluster fit evaluator
  TMutClusterFitEval::set_do_evaluation( false );

  // stubfinder runtime parameters
  TMutStubFinder::set_w_prox_cut(0.5);
  TMutStubFinder::set_dca_cut(0.4);
  TMutStubFinder::set_do_evaluation( false );
  TMutStubFinder::set_reverse_algo( true );

  TMutStubFinder::set_min_coord_1( 4 );
  TMutStubFinder::set_min_coord_2( 3 );
  TMutStubFinder::set_min_coord_3( 2 );

  TMutStubFinder::set_verbosity( MUTOO::NONE );

  // configure ErrorStat singleton
  if( rc->get_IntFlag("MUTOO_ERRORSTATS", 0 ) )
  TMutErrorStats::initialize_ntuple();

  // dump parameters if needed.
  if( rc->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) 
  {
    mMutFindClus_par->print();
    mMutFitClus_par->print();
    mMutMatchCoord_par->print();
    mMutStubFit_par->print();
    mMutFindTrack_par->print();
    mMutBPFit_par->print();
    mMutRejectTrack_par->print();
    mMutKalFit_par->print();
    mMutFitVtx_par->print();
    TMutMathiesonPar::print();
    TMutClusterFit::print_parameters();
    TMutStubFinder::print_parameters();
  }

  return 0;
  
}

//______________________________________________________
int MuonDev::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();

  // vertex
  MuonSubsysReco::load_vertex_if_needed( top_node );

  // check SKIP_MUTOO_RECO flag
  if( recoConsts::instance()->get_IntFlag( "SKIP_MUTOO_RECO", 0 ) )
  {
    MUTOO::TRACE( "MuonDev::process_event - SKIP_MUTOO_RECO is true. Skipping module." );
    return 0;
  }

  recoConsts *rc = recoConsts::instance();
  TMutErrorStats::clear_event();

  // Call MUTOO modules for track momentum reconstruction and vertex finding
  try {

    rc->set_IntFlag("DATABASE", 1);

    // track finding/fitting
    if( !( get_flag( USE_LOCAL_CLUSTERS ) || get_flag( SKIP_CLUSTERING ) ) )
    {
      _mMutFindClus_mod.event(mutoo_node);
      _mMutFitClus_mod.event(mutoo_node);
      _mMutMatchCoord_mod.event(mutoo_node);
    }

    if( !get_flag( SKIP_TRACK_FINDING ) ) 
    {
      _mMutFindTrack_mod.event(top_node);
    
      // bend plane fit
      _mMutBPFit_mod.event(top_node);
      
      // first pass track rejection
      _mMutRejectTrack_mod.event(top_node);
      
      // track kalman fit
      _mMutKalFit_mod.event(top_node);
      
      // Chun's MUID road association module
      _mMutMuiRoad_mod.event(top_node);

      // vertex find/fit
      if( !get_flag( SKIP_VERTEX_FIT ) )
      {
        _mMutFindVtx_mod.event(mutoo_node);
        _mMutBPVertex_mod.event(top_node);
        _mMutFitVtx_mod.event(top_node);
      }
      
    }
    
  } catch (exception& e) { MUTOO::TRACE(e.what()); }

  // needed to make sure maps are always written whatever SubsysReco are in the macro
  MuonSubsysReco::write_maps_if_needed();
  
  // errors
  TMutErrorStats::set_event_stats(top_node, static_cast<short unsigned int> (_timer.get()->elapsed()) );
  TMutErrorStats::write_event();

  _timer.get()->stop();

  return 0;
}

//______________________________________________________
int MuonDev::End(PHCompositeNode* top_node)
{
  TMutErrorStats::finish();
  _timer.get()->print_stat();

  try {

    // write fit cluster evaluation
    if( TMutClusterFitEval::get_do_evaluation() )
    TMutClusterFitEval::finish_evaluation();

    // write stub evaluation tree if required
    if( TMutStubFinder::get_do_evaluation() )
    {
      TMutStubFinder::finish_evaluation();
      TMutStubFinder::Stub::finish_evaluation();
    }

    // write fit cluster evaluation
    if( TMutNode<mMutMatchCoordPar>::find_node(top_node,"mMutMatchCoordPar")->get_do_evaluation() )
    _mMutMatchCoord_mod.finish_evaluation();

    // write mMutFindTrack module tree to file, if required
    if( TMutNode<mMutFindTrackPar>::find_node(top_node,"mMutFindTrackPar")->get_do_evaluation() )
    _mMutFindTrack_mod.finish_evaluation();

    // write mMutKalFit module tree to file, if required
    if( TMutNode<mMutKalFitPar>::find_node(top_node,"mMutKalFitPar")->get_evaluation_mode() != mMutKalFitPar::NONE )
    _mMutKalFit_mod.finish_evaluation();

   } catch( exception &e ) { cerr << e.what() << endl; }

  // dump summary for match coord module.
  _mMutMatchCoord_mod.print_summary();
  _mMutRejectTrack_mod.print_summary();

  // stub finder
  TMutStubFinder::print_summary();
  
  // class IDs
  // PHClassId::print();
  
  return 0;
}
