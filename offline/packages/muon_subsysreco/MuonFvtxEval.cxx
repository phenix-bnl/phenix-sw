#include "MuonFvtxEval.h"
#include "MuonUtil.h"

#include <MutStrip.h>

#include <PHTFileServer.h>
#include <PHTimer.h>
#include <PHTimeServer.h>
#include <recoConsts.h>
#include <PHGlobal.h>
#include <PHGeometry.h>

#include<PHLine.h>
#include<PHVector.h>
#include<PHPoint.h>

// ROOT
#include<TNtuple.h>
#include<TTree.h>

// MUIOO IOC includes
#include<TMuiMCHitMapO.h>
#include<TMuiHitMapO.h>
#include<TMuiRoadMapO.h>
#include<TMuiClusterMapO.h>

// MUTOO IOC includes
#include<TMutEvalMap.h>
#include<TMutHitMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutClusMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutVtxMap.h>
#include<TMutGeo.h>
#include<MutGeom.h>
#include<MutCalib.h>
#include<MUTOO_FEM.h>

#include<TFvtxTrkMap.h>
#include <PHTrackIntegratorKF.h>

// SL/BOOST
#include<sstream>
#include<cmath>

// MUTOO Module includes
#include<mMutEval.h>
#include<TMutStubFinder.h>
#include<TMutErrorStats.h>
#include<TMutTrackUtil.h>

// MUIOO Module includes
#include<mMuiEvalO.h>
#include<TMuiChannelId.hh>
#include<TMuiHVMask.h>

// FVTXOO Module includes
#include <TFvtxMCHitMap.h>
#include <TFvtxCoordMap.h>

#include <gsl/gsl_randist.h>

using namespace std;

//_____________________________________
MuonFvtxEval::MuonFvtxEval(
  const char* name,
  const char* filename ):
  MuonSubsysReco( name ),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _top_node( 0 ),
  _mutoo_node(0),
  _muioo_node(0),
  _dst_node(0),
  _signalNodeName("TOP"),
  _signal_top_node(0),
  _filename( filename ),

  // initialize ntuples
  _timing(0),
  _dimu_reco(0),
  _cu_dimu_evt(0),
  _single_reco(0),
  _stub_reco(0),
  _clus_reco(0),
  _effic(0),
  _background(0),
  _cu_mc_trk(0),
  _cu_reco_trk(0),
  _cu_effic_nt(0),
  _event_veto(0),
  _n_evt(0)
{
  _n_road_pl.assign( 0 );
  return ;
}

//_____________________________________
int MuonFvtxEval::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );

  // create needed nodes and return
  return CreateNodeTree(top_node);

}

//_____________________________________
int MuonFvtxEval::InitRun(PHCompositeNode *top_node)
{

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "MuonFvtxEval::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  cout << "MuonFvtxEval::InitRun - _signalNodeName : " << _signalNodeName << endl;

  return 0;


}

//________________________________________________________
int MuonFvtxEval::CreateNodeTree(PHCompositeNode *top_node)
{

  PHNodeIterator nodeItr(top_node);

  // Instantiate nodes for mutoo containers
  _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_mutoo_node){
    _mutoo_node = new PHCompositeNode("MUTOO");
    top_node->addNode(_mutoo_node);
  }

  // Instantiate nodes for muioo containers
  _muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
  if(!_muioo_node){
    _muioo_node = new PHCompositeNode("MUIOO");
    top_node->addNode(_muioo_node);
  }

  _dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!_dst_node) {
    _dst_node = new PHCompositeNode("DST");
    top_node->addNode(_dst_node);
  }

  // Interface Object Containers (IOCS)
  TMutEvalMap* eval_map = TMutNode<TMutEvalMap>::new_node(_mutoo_node, "TMutEvalMap");
  TMuiEvalMap* mui_eval_map = TMutNode<TMuiEvalMap>::new_node(_muioo_node, "TMuiEvalMap");

  // Make IOCs persistent here
  eval_map->make_persistant(_dst_node,"TMutEval");
  mui_eval_map->make_persistant(_dst_node,"TMuiEval");

  // Module parameter tables
  mMutEvalPar* eval_par = TMutNode<mMutEvalPar>::new_node(_mutoo_node,"mMutEvalPar");
  mMuiEvalOPar* mui_eval_par = TMutNode<mMuiEvalOPar>::new_node(_muioo_node,"mMuiEvalOPar");

  // copy MCTrkMap from signal node to mutoo_node


  // Default parameters are defined in parameter table constructor initialization list.
  // Change the default values here.
  eval_par->set_verbosity(MUTOO::NONE);
  eval_par->set_pr_mode(mMutEvalPar::NORMAL);

  mui_eval_par->set_verbosity(MUIOO::NONE);
  mui_eval_par->set_pr_mode(mMuiEvalOPar::NORMAL);

  return 0;
}

//________________________________________________________
int MuonFvtxEval::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  try {

    // get local pointers to nodes
    set_interface_ptrs( top_node );

    // Run the mutoo evaluation module
    //_mMutEval_mod.event(_signal_top_node, top_node);

    // Run the muioo evaluation
    //_mMuiEval_mod.event(_signal_top_node, top_node);

    //trk_iter = _trk_map->range();
    //while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    //{
    //TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
    //cout << "L214 MuonFvtxEval::write_single_reco_ntuple - there are = " << mc_trk_iter.count()  << " associated tracks!" << endl;
    //}

    //kWrite some output ntuples
    write_ntuples();

    // Stores event statistics
    store_muid_stat(top_node);


  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }

  write_maps_if_needed();
  _timer.get()->stop();

  //  Return current state of event veto and reset
  //  for next event
  return get_event_veto_reset();
}

//________________________________________________________
void MuonFvtxEval::set_interface_ptrs( PHCompositeNode *top_node )
{

  // store top_node locally
  _top_node = top_node;
  _signal_top_node = Fun4AllServer::instance()->topNode( _signalNodeName );

  //! try retrieve PHGlobal node
  try { _global = TMutNode<PHGlobal>::find_io_node( top_node, "PHGlobal" ); }
  catch( exception& e ) { cout << e.what() << endl; _global = 0; }

  // no node argument is passed to load_map for TMutMCTrkMap
  // this allows to load mc_trk_map any existing top node, if any
  // which in turn is necessary when running embedding.
  try { _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node( _signal_top_node, "TMutMCTrkMap"); }
  catch( exception& e ) {cout << e.what() << endl;  _mc_trk_map = 0; }

  try { _stub_map = TMutNode<TMutStubMap>::find_node(_mutoo_node,"TMutStubMap"); }
  catch( exception& e ) { cout << e.what() << endl; _stub_map = 0; }

  try { _trk_map = TMutNode<TMutTrkMap>::find_node( _mutoo_node,"TMutTrkMap"); }
  catch( exception& e ) { cout << e.what() << endl; _trk_map = 0; }

  try { _clus_map = TMutNode<TMutClusMap>::find_node( _mutoo_node,"TMutClusMap"); }
  catch( exception& e ) { cout << e.what() << endl; _clus_map = 0; }

  try { _vtx_map = TMutNode<TMutVtxMap>::find_node( _mutoo_node,"TMutVtxMap"); }
  catch( exception& e ) { cout << e.what() << endl; _vtx_map = 0; }

  try { _hit_map = TMutNode<TMutHitMap>::find_node( _mutoo_node,"TMutHitMap"); }
  catch( exception& e ) { cout << e.what() << endl; _hit_map = 0; }

  try { _mui_hit_map = TMutNode<TMuiHitMapO>::find_node( _muioo_node,"TMuiHitMapO"); }
  catch( exception& e ) { cout << e.what() << endl; _mui_hit_map = 0; }

  try { _mui_road_map = TMutNode<TMuiRoadMapO>::find_node( _muioo_node,"TMuiRoadMapO"); }
  catch( exception& e ) { cout << e.what() << endl; _mui_road_map = 0; }

  try { _eval_map = TMutNode<TMutEvalMap>::find_node( _mutoo_node,"TMutEvalMap"); }
  catch( exception& e ) { cout << e.what() << endl; _eval_map = 0; }

  try { _mui_eval_map = TMutNode<TMuiEvalMap>::find_node( _muioo_node,"TMuiEvalMap"); }
  catch( exception& e ) { cout << e.what() << endl; _stub_map = 0; }

};

//________________________________________________________
int MuonFvtxEval::End(PHCompositeNode* top_node)
{

  // stub finder evaluation
  if( TMutStubFinder::get_do_evaluation() ) TMutStubFinder::finish_evaluation();

  // write ntuples
  PHTFileServer::get().write( _filename );

  // dump timer for this supermodule
  _timer.get()->print_stat();

  // dump event statistics
  print_muid_stat();

  return 0;
}

//________________________________________________________
void MuonFvtxEval::write_ntuples()
{
  // initialize ntuples
  static bool init_done __attribute__ ((unused)) = initialize_ntuples();

  if(!init_done) throw runtime_error(DESCRIPTION("MuonFvtxEval::write_ntuples: failed to initialize ntuples"));

  write_timing_ntuple();
  write_single_reco_ntuple();

  // Flush out ntuple every 100 events.
  static ULong_t auto_save=0;
  if(auto_save++%100 == 0)
  {

    if( _timing ) _timing->AutoSave();
    PHTFileServer::get().flush( _filename );

  }

}

//________________________________________________________
void MuonFvtxEval::write_timing_ntuple()
{
  // reset timings
  _time.assign(0);

  PHTimeServer::iterator iter = PHTimeServer::get()->range();
  while( PHTimeServer::timer *timer = iter.next() ){
    if( timer->get_uid() < n_timers ) _time[timer->get_uid()] = timer->get()->elapsed();
  }
  if( _timing ) _timing->Fill();
}

//________________________________________________________
bool MuonFvtxEval::initialize_ntuples()
{   
  MUTOO::PRINT( cout, "MuonFvtxEval::initialize_ntuples" );

  // Output file
  PHTFileServer::get().open( _filename );
  cout << "writing to " << _filename << endl;
    
  // Timing information
    ostringstream o; o << "Time[" << _time.size() << "]/D";
    _timing = new TTree( "timing", "timing information for reconstruction modules" );
    _timing->Branch( "Time", &_time[0], o.str().c_str() );
    cout << "timing ntuple booked" << endl;

  //Reconstructed single tracks
    _single_reco = new TNtuple(
      "single_reco","single_reco",
      "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
      "r_gap0:road_depth:ntrks:ievent:run:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
      "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
      "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus:reco_px:reco_py:reco_pz:"
      "reco_ptot:prnt_trkid:prnt_px:prnt_py:prnt_pz:fvtx_assoc:fvtx_assoc_trid:fvtx_exist:fvtx_exist_trid:fvtx_match:"
      "fvtx_x0reco:fvtx_y0reco:fvtx_z0reco:fvtx_px0reco:fvtx_py0reco:fvtx_pz0reco:"
      "fvtx_x4mc:fvtx_y4mc:fvtx_z4mc:fvtx_px4mc:fvtx_py4mc:fvtx_pz4mc:"
      "mut_x1mc:mut_y1mc:mut_z1mc:mut_px1mc:mut_py1mc:mut_pz1mc:"
      "fvtx_mut_match_dx:fvtx_mut_match_dy:fvtx_mut_match_dpx:fvtx_mut_match_dpy:fvtx_mut_match_dpz") ;
    cout << "single_reco ntuple booked" << endl;

  MUTOO::PRINT( cout, "**" );
    
  return true;
}

//________________________________________________________
void MuonFvtxEval::write_single_reco_ntuple()
{

  static int ievent=0;
  
  ievent++;
  
  // Reconstructed single-muon ntuple
  // also includ associated muID road and MC track information
  // the best maching is still on hold right now ....  MXL 05/01/2004
  //
  
  if( !_trk_map ) return;
  float nt_vars[100] = {0};
  
  TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  
  //  "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
  //  "r_gap0:road_depth:ntrks:ievent:run:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
  //  "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
  //  "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus") ;

  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
  {

    nt_vars[0] = trk_ptr->get()->get_arm();
    nt_vars[1] = trk_ptr->get()->get_trk_par_vtx()->get_px();
    nt_vars[2] = trk_ptr->get()->get_trk_par_vtx()->get_py();
    nt_vars[3] = trk_ptr->get()->get_trk_par_vtx()->get_pz();
    nt_vars[4] = trk_ptr->get()->get_trk_par_vtx()->get_ptot();
    nt_vars[5] = trk_ptr->get()->get_trk_par_vtx()->get_charge();
    nt_vars[6] = trk_ptr->get()->get_trk_par_vtx()->get_chi_square();
    nt_vars[7] = trk_ptr->get()->get_trk_par_vtx()->get_z();
    nt_vars[8] = trk_ptr->get()->get_ghost();
    nt_vars[9] = trk_ptr->get()->get_reco_success();

    //! this was the road proximity. Made obsolete since we now use MuiOO
    nt_vars[10] = 0;
    nt_vars[11] = get_max_road_depth( trk_ptr );
    nt_vars[12] = trk_iter.count();
    nt_vars[13] = (float)ievent;
    nt_vars[14] = -99; //run


    nt_vars[37] = trk_ptr->get()->get_trk_par()->get_px();
    nt_vars[38] = trk_ptr->get()->get_trk_par()->get_py();
    nt_vars[39] = trk_ptr->get()->get_trk_par()->get_pz();
    nt_vars[40] = trk_ptr->get()->get_trk_par()->get_ptot();

    //---------------------------
    // Loop over associated reconstructed roads which are associated with this reconstructed track
    //---------------------------
    TMuiRoadMapO::const_key_iterator reco_road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();

    // hopefully <= 3
    float       n_reco_roads = reco_road_iter.count();
    if( n_reco_roads > 3 )
    { cout << "MuonFvtxEval::write_single_reco_ntuple - more than three roads associated to this track!!!" << endl; }

    cout << "MuonFvtxEval::write_single_reco_ntuple - there are = " << n_reco_roads << " associated muID roads!" << endl;

    float deepest_depth =0, deepest_chisq =0;
    float gap0_x=-999,gap0_y=-999;

    while( TMuiRoadMapO::const_pointer reco_road_ptr = reco_road_iter.next() )
    {

      Short_t current_depth = reco_road_ptr->get()->get_depth();

      if( current_depth > deepest_depth )
      {
        deepest_depth = reco_road_ptr->get()->get_depth();
        deepest_chisq = reco_road_ptr->get()->get_const_fitpar()->get_chi_square();
      }

      // could also store information from the best road...in terms of matching

      gap0_x = reco_road_ptr->get()->get_gap0_point().getX();
      gap0_y = reco_road_ptr->get()->get_gap0_point().getY();

      //muID variables
      nt_vars[15] = n_reco_roads;
      nt_vars[16] = deepest_depth;
      nt_vars[17] = deepest_chisq;
      nt_vars[18] = gap0_x;
      nt_vars[19] = gap0_y;
    }  // end of road loop

    //----------------
    //get associated MC track information
    //----------------
    TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();

    // How many associated MCTracks are there for this reco_trk?
    float       n_mc_tracks = mc_trk_iter.count();

    cout << "MuonFvtxEval::write_single_reco_ntuple - there are = " << n_mc_tracks << " associated tracks!" << endl;

    // Loop over associated MCTracks
    //  mc_trk_iter.reset();
    while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {

      //  "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
      //  "r_gap0:road_depth:ntrks:ievent:ptotus:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
      //  "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
      //  "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus") ;

      //  _single_reco = new TNtuple("single_reco","single_reco",
      //             "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
      //             "r_gap0:road_depth:ntrks:ievent:run:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
      //             "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
      //             "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus:reco_px:reco_py:reco_pz:"
      //             "reco_ptot:prnt_trkid:prnt_px:prnt_py:prnt_pz") ;

      //MC information variables
      //          float   arm = mc_trk_ptr->get()->get_arm();
      nt_vars[20] = n_mc_tracks;
      nt_vars[21] = mc_trk_ptr->get()->get_pid();
      nt_vars[22] = mc_trk_ptr->get()->get_parent_id();
      nt_vars[23] = mc_trk_ptr->get()->get_parent_track_id();
      nt_vars[23] = -99; // prnt_ptot
      nt_vars[24] = get_mc_trk_depth( mc_trk_ptr );
      nt_vars[25] = mc_trk_ptr->get()->get_px_orig();
      nt_vars[26] = mc_trk_ptr->get()->get_py_orig();
      nt_vars[27] = mc_trk_ptr->get()->get_pz_orig();
      nt_vars[28] = mc_trk_ptr->get()->get_x_orig();
      nt_vars[29] = mc_trk_ptr->get()->get_y_orig();


      nt_vars[30] = mc_trk_ptr->get()->get_z_orig();
      nt_vars[31] = mc_trk_ptr->get()->get_charge();
      nt_vars[32] = mc_trk_ptr->get()->get_ptot_us_gap();
      nt_vars[33] = (float)mc_trk_ptr->get()->get_track_id();
      nt_vars[34] = mc_trk_ptr->get()->get_px_us_gap();
      nt_vars[35] = mc_trk_ptr->get()->get_py_us_gap();
      nt_vars[36] = mc_trk_ptr->get()->get_pz_us_gap();



      //
      //get 1st parent information - PID, P, charge etc.
      //
      Int_t prnt_trk_ID = mc_trk_ptr->get()->get_parent_track_id();
      cout
        << "MuonFvtxEval::write_single_reco_ntuple - looking for particle "
        <<  mc_trk_ptr->get()->get_pid()
        << " 's  parent == "
        <<  mc_trk_ptr->get()->get_parent_id()
        << "  prnt_track_ID =" << prnt_trk_ID << endl;
      Int_t trk_IDx = -99;


      Float_t prnt_px=-999, prnt_py=-999,prnt_pz=-999,prnt_ptot=-999;

      if( !_mc_trk_map ) return;
      TMutMCTrkMap::const_iterator mc_trk_iter_1p = _mc_trk_map->range();

      while (TMutMCTrkMap::const_pointer mc_trk_ptr_1p = mc_trk_iter_1p.next())
      {

        //get the track id index
        // trk_id ==0 for the very primary particle
        trk_IDx = mc_trk_ptr_1p->get()->get_track_id() ;
        cout << "MuonFvtxEval::write_single_reco_ntuple - in the MCbank,  MC track id trk_IDx == " << trk_IDx <<  endl;

        if ( abs(prnt_trk_ID) == abs(trk_IDx) )
        {

          // found the parent MC track
          Float_t prnt_IDx = mc_trk_ptr_1p->get()->get_parent_id();
          cout << "MuonFvtxEval::write_single_reco_ntuple - found parent particle == parent = " << prnt_IDx << endl;

          prnt_px = mc_trk_ptr_1p->get()->get_px_orig();
          prnt_py = mc_trk_ptr_1p->get()->get_py_orig();
          prnt_pz = mc_trk_ptr_1p->get()->get_pz_orig();

          prnt_ptot = sqrt(prnt_px*prnt_px + prnt_py*prnt_py + prnt_pz*prnt_pz);
          nt_vars[23] = prnt_ptot;
          // prnt_ptot

          nt_vars[42] = prnt_px; //parent track px
          nt_vars[43] = prnt_py; //parent track py
          nt_vars[44] = prnt_pz; //parent track pz


        } // found parent track ID

        nt_vars[41] = prnt_trk_ID; //parent track ID

      } // loop MC track bank for the 1st parent

      TFvtxTrkMap::key_iterator fvtx_trk_iter = mc_trk_ptr->get()->get_associated<TFvtxTrk>();
      cout << "MuonFvtxEval::write_single_reco_ntuple - mc_trk associated TFvtxTrk count = " << fvtx_trk_iter.count() << endl; 
      if (fvtx_trk_iter.count() > 0) nt_vars[47] = 1;
      else nt_vars[47] = 0;
      nt_vars[48] = mc_trk_ptr->get()->get_track_id();

      float fvtx_x0reco = -9999, fvtx_y0reco = -9999, fvtx_z0reco = -9999;
      float fvtx_px0reco = -9999, fvtx_py0reco = -9999, fvtx_pz0reco = -9999;

      while( TFvtxTrkMap::const_pointer fvtx_trk_ptr = fvtx_trk_iter.next() ) {
        fvtx_x0reco = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_x();
        fvtx_y0reco = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_y();
        fvtx_z0reco = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_z();
        fvtx_px0reco = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_px();
        fvtx_py0reco = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_py();
        fvtx_pz0reco = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_pz();
      }
      nt_vars[50] = fvtx_x0reco;
      nt_vars[51] = fvtx_y0reco;
      nt_vars[52] = fvtx_z0reco;
      nt_vars[53] = fvtx_px0reco;
      nt_vars[54] = fvtx_py0reco;
      nt_vars[55] = fvtx_pz0reco;

      float fvtx_x4mc = 0, fvtx_y4mc = 0, fvtx_z4mc = 0;
      float fvtx_px4mc = 0, fvtx_py4mc = 0, fvtx_pz4mc = 0;

      TFvtxMCHitMap::key_iterator fvtx_mc_hit_iter = mc_trk_ptr->get()->get_associated<TFvtxMCHit>();
      while ( TFvtxMCHitMap::const_pointer fvtx_mc_hit_ptr = fvtx_mc_hit_iter.next() )
      {
        if ( mc_trk_ptr->get()->get_track_id() != fvtx_mc_hit_ptr->get()->get_track_id() ) continue;

        // Check to see that a coordinate was actually formed by this MC hit:
        TFvtxCoordMap::key_iterator mc_coord_iter = fvtx_mc_hit_ptr->get()->get_associated<TFvtxCoord>();
        if (mc_coord_iter.count() > 0)
        {
          if (fvtx_mc_hit_ptr->get()->get_station()==FVTXOO::Station4){
            fvtx_x4mc = fvtx_mc_hit_ptr->get()->get_x();
            fvtx_y4mc = fvtx_mc_hit_ptr->get()->get_y();
            fvtx_z4mc = fvtx_mc_hit_ptr->get()->get_z(); 
            fvtx_px4mc = fvtx_mc_hit_ptr->get()->get_px();
            fvtx_py4mc = fvtx_mc_hit_ptr->get()->get_py();
            fvtx_pz4mc = fvtx_mc_hit_ptr->get()->get_pz();
          }
        }
      }
      nt_vars[56] = fvtx_x4mc;
      nt_vars[57] = fvtx_y4mc;
      nt_vars[58] = fvtx_z4mc;
      nt_vars[59] = fvtx_px4mc;
      nt_vars[60] = fvtx_py4mc;
      nt_vars[61] = fvtx_pz4mc;

      cout << "MuonFvtxEval::Fvtx St4 Position ( " << nt_vars[56] << ", " << nt_vars[57] << ", " << nt_vars[58] << ") " 
           << " Momentum ( " << nt_vars[59] << ", " << nt_vars[60] << ", " << nt_vars[61] << ") " << endl;

      float mut_x1mc = 0, mut_y1mc = 0, mut_z1mc = 0;
      float mut_px1mc = 0, mut_py1mc = 0, mut_pz1mc = 0;

      TMutMCHitMap::key_iterator mut_mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
      while ( TMutMCHitMap::const_pointer mut_mc_hit_ptr = mut_mc_hit_iter.next() )
      {
        if ( mc_trk_ptr->get()->get_track_id() != mut_mc_hit_ptr->get()->get_track_id() ) continue;
         
        if ( mut_mc_hit_ptr->get()->get_station() == 1 ) {
          mut_x1mc = mut_mc_hit_ptr->get()->get_x();
          mut_y1mc = mut_mc_hit_ptr->get()->get_y();
          mut_z1mc = mut_mc_hit_ptr->get()->get_z();
          mut_px1mc = mut_mc_hit_ptr->get()->get_px();
          mut_py1mc = mut_mc_hit_ptr->get()->get_py();
          mut_pz1mc = mut_mc_hit_ptr->get()->get_pz();    
        }
      }
      nt_vars[62] = mut_x1mc;
      nt_vars[63] = mut_y1mc;
      nt_vars[64] = mut_z1mc;
      nt_vars[65] = mut_px1mc;
      nt_vars[66] = mut_py1mc;
      nt_vars[67] = mut_pz1mc;

      cout << "begin extrapolation from MUT st1 z = " << mut_z1mc << " to  " << fvtx_z4mc << endl;
      if (fvtx_pz4mc == 0 || mut_pz1mc == 0) continue;
      TMutTrkPar trk_par(
        mut_x1mc, mut_y1mc, mut_z1mc,
        mut_px1mc, mut_py1mc, mut_pz1mc,
        mc_trk_ptr->get()->get_charge(), 0); 
      for( int i=0; i<5; i++)
      {
        for( int j=0; j<5; j++ )
        {
          trk_par.set_covar(i,j, trk_ptr->get()->get_trk_par_vtx()->get_covar(i, j) ); 
        }
      }

      PHTrackIntegratorKF integrator;
      integrator.initialize( trk_par );
      integrator.extrapolate( fvtx_z4mc );
      if( integrator.get_error() )
      {
        cout << "MutFvtxEval - extrapolation failed." << endl;
      }

      // extrapolation finished
      // update track parameters at vertex
      integrator.finish( trk_par );

      // some dump (for debugging)
      verbosity = 1;
      if( verbosity )
      {
        cout << "MutFvtxEval::extrapolate_track -"
          << " position= ("
          << trk_par.get_x() << ","
          << trk_par.get_y() << ","
          << trk_par.get_z() << ")"
          << " momentum = ("
          << trk_par.get_px() << ","
          << trk_par.get_py() << ","
          << trk_par.get_pz() << ")"
          << endl;
      }

      float fvtx_mut_match_dx = trk_par.get_x() - fvtx_x4mc;
      float fvtx_mut_match_dy = trk_par.get_y() - fvtx_y4mc;

      float fvtx_mut_match_dpx = trk_par.get_px() - fvtx_px4mc;
      float fvtx_mut_match_dpy = trk_par.get_py() - fvtx_py4mc;
      float fvtx_mut_match_dpz = trk_par.get_pz() - fvtx_pz4mc;

      nt_vars[68] = fvtx_mut_match_dx;
      nt_vars[69] = fvtx_mut_match_dy;
      nt_vars[70] = fvtx_mut_match_dpx;
      nt_vars[71] = fvtx_mut_match_dpy;
      nt_vars[72] = fvtx_mut_match_dpz;

    }//loop MC track back for the reco track

    // See if there are any FVTX tracks associated with this track:

    TFvtxTrkMap::key_iterator fvtx_trk_iter = trk_ptr->get()->get_associated<TFvtxTrk>();
    if (fvtx_trk_iter.count() > 0) nt_vars[45] = 1;
    else nt_vars[45] = 0;
    while( TFvtxTrkMap::const_pointer fvtx_trk_ptr = fvtx_trk_iter.next() ) {
      TMutMCTrkMap::key_iterator fvtx_mc_trk_iter = fvtx_trk_ptr->get()->get_associated<TMutMCTrk>();
      while( TMutMCTrkMap::const_pointer fvtx_mc_trk_ptr = fvtx_mc_trk_iter.next() ) {
        nt_vars[46] = fvtx_mc_trk_ptr->get()->get_track_id();
      }
    }

    if (nt_vars[45] != 0 && nt_vars[47] != 0 && nt_vars[46] == nt_vars[48]) nt_vars[49] = 1;  // both fvtx and mutr trk exist and matched (same mc_trk_id)
    else if (nt_vars[45] == 0 && nt_vars[47] != 0 && nt_vars[46] == nt_vars[48]) nt_vars[49] = 2;  // fvtx trk not associated but exist (reco-ed)
    else nt_vars[49] = 0;  // no fvtx trk
    //cout << "fvtx_assoc = " << nt_vars[45] << " fvtx_assoc_trid = " << nt_vars[46] << " fvtx_exist = " << nt_vars[47] << " fvtx_exist_trid = " << nt_vars[48]  << " fvtx_match = " << nt_vars[49] << endl;

    // ---- write out ntuple ---
    _single_reco->Fill(nt_vars);
   
  }  // Loop over tracks

  if( verbosity >= 1 ) cout << "MuonFvtxEval::write_single_reco_ntuple" << endl;
}

//________________________________________________________
void MuonFvtxEval::store_muid_stat( PHCompositeNode* top_node )
{
  static bool init_done = false;
  if( !init_done ) {

    _n_evt = 0;
    _n_road_pl.assign(0);
    init_done = true;
  }

  try {
    _n_evt++;

    if( !_mui_road_map ) return;
    TMuiRoadMapO::iterator road_iter = _mui_road_map->range();
    while( TMuiRoadMapO::pointer road_ptr = road_iter.next() )
    { _n_road_pl[road_ptr->get()->get_depth()]++; }

  } catch( exception &e ) { cout << e.what() << endl;}
}

//________________________________________________________
void MuonFvtxEval::print_muid_stat( ostream &out )
{
  if( !_n_evt ) return;
  MUTOO::PRINT( out, "muid statistics");
  for( UShort_t gap=0; gap<MUIOO::MAX_PLANE; gap++ )
  { 
    out << "  muid road/event ending in gap " << gap << " : " << double(_n_road_pl[gap])/_n_evt << endl;
  } 
  MUTOO::PRINT( out, "**" );
}

//________________________________________________________
double MuonFvtxEval::rapidity(double px, double py, double pz, double mass)
{
  double ptot = sqrt(px*px + py*py + pz*pz);
  double E = sqrt(ptot*ptot + mass*mass);
  return 0.5*log((E+pz)/(E-pz));
}

//________________________________________________________
double MuonFvtxEval::pt(double px, double py)
{
  return sqrt(px*px + py*py);
}

//____________________________________________________________
int MuonFvtxEval::get_mc_trk_depth( TMutMCTrkMap::const_pointer mc_trk_ptr )
{
  // get associated TMuiMCHitO
  int out( 0 );
  TMuiMCHitMapO::const_key_iterator mc_hit_iter( mc_trk_ptr->get()->get_associated<TMuiMCHitO>() );
  while( TMuiMCHitMapO::const_pointer mc_hit_ptr = mc_hit_iter.next() )
  out = max( out, static_cast<int>(mc_hit_ptr->get()->get_plane() ) );
  return out;
}

//____________________________________________________________
int MuonFvtxEval::get_max_road_depth( TMutTrkMap::const_pointer trk_ptr )
{

  int out( 0 );
  TMuiRoadMapO::const_key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );
  while( TMuiRoadMapO::const_pointer road_ptr = road_iter.next() )
  out = max( out, static_cast<int>(road_ptr->get()->get_depth() ) );
  return out;
}

