// $Id: MuonEval.cxx,v 1.14 2017/07/15 04:17:17 phnxbld Exp $
#include "MuonEval.h"
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

#include<TFvtxMCHitMap.h>
#include<TFvtxCoordMap.h>
#include<TFvtxSvxClusterMap.h>

#include<TMutExtVtx.h>
#include<PHTrackIntegratorKF.h>
#include<mMutFitVtx.h>

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

#include <gsl/gsl_randist.h>

using namespace std;

//_____________________________________
MuonEval::MuonEval(
  const char* name,
  const char* filename ):
  MuonSubsysReco( name ),
  _flags( ALL ),
  _timer( PHTimeServer::get()->insert_new(name) ),
  _top_node( 0 ),
  _mutoo_node(0),
  _muioo_node(0),
  _fvtxoo_node(0),
  _dst_node(0),
  _signalNodeName("SIGNAL"), // _signalNodeName("TOP"),
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
int MuonEval::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );

  // create needed nodes and return
  return CreateNodeTree(top_node);

}

//_____________________________________
int MuonEval::InitRun(PHCompositeNode *top_node)
{

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "MuonEval::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  cout << "MuonEval::InitRun - _signalNodeName : " << _signalNodeName << endl;

  return 0;


}

//________________________________________________________
int MuonEval::CreateNodeTree(PHCompositeNode *top_node)
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

  // Instantiate nodes for mutoo containers
  _fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "FVTXOO"));
  if(!_fvtxoo_node){
    _fvtxoo_node = new PHCompositeNode("FVTXOO");
    top_node->addNode(_fvtxoo_node);
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
int MuonEval::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();

  try {

    // get local pointers to nodes
    set_interface_ptrs( top_node );

    // Run the mutoo evaluation module
    //_mMutEval_mod.event(_signal_top_node, top_node);
    _mMutEval_mod.event(_signal_top_node, _mutoo_node);

    // Run the muioo evaluation
    _mMuiEval_mod.event(_signal_top_node, top_node);

    // Write some output ntuples
    if( get_flags() != NONE ) write_ntuples();

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
void MuonEval::set_interface_ptrs( PHCompositeNode *top_node )
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

  try { _fvtx_trk_map = TMutNode<TFvtxTrkMap>::find_node( _fvtxoo_node,"TFvtxTrkMap"); }
  catch( exception& e ) { cout << e.what() << endl; _fvtx_trk_map = 0; }

};

//________________________________________________________
int MuonEval::End(PHCompositeNode* top_node)
{

  // stub finder evaluation
  if( TMutStubFinder::get_do_evaluation() ) TMutStubFinder::finish_evaluation();

  // write ntuples
  if( get_flags() != NONE ) PHTFileServer::get().write( _filename );

  // dump timer for this supermodule
  _timer.get()->print_stat();

  // dump event statistics
  print_muid_stat();

  return 0;
}

//________________________________________________________
void MuonEval::write_ntuples()
{
  // initialize ntuples
  static bool init_done __attribute__ ((unused)) = initialize_ntuples();

  if(!init_done) throw runtime_error(DESCRIPTION("MuonEval::write_ntuples: failed to initialize ntuples"));

  if( get_flag( TIMING ) ) write_timing_ntuple();
  if( get_flag( RECO ) ) write_reco_ntuple();
  if( get_flag( SINGLE_RECO ) ) write_single_reco_ntuple();
  if( get_flag( CLUS_RECO ) ) write_clus_ntuple();
  if( get_flag( STUB_RECO ) ) write_stub_reco_ntuple();
  if( get_flag( EFFIC ) ) write_effic_ntuple();
  if( get_flag( BENT_PLANE ) )write_bend_plane_ntuple();
  if( get_flag( BACKGROUND ) ) write_bg_ntuple();
  if( get_flag( MUIOO ) ) write_muioo_ntuple();
  if( get_flag( CU_SINGLE_MU ) ) write_cu_single_muon_ntuple();
  if( get_flag( CU_EFFIC ) ) write_cu_effic_ntuple();

  // Flush out ntuple every 100 events.
  static ULong_t auto_save=0;
  if(auto_save++%100 == 0)
  {

    if( _timing ) _timing->AutoSave();
    if( _dimu_reco ) _dimu_reco->AutoSave();
    if( _cu_dimu_evt ) _cu_dimu_evt->AutoSave();
    if( _single_reco ) _single_reco->AutoSave();
    if( _stub_reco ) _stub_reco->AutoSave();
    if( _clus_reco ) _clus_reco->AutoSave();
    if( _cu_mc_trk ) _cu_mc_trk->AutoSave();
    if( _cu_reco_trk ) _cu_reco_trk->AutoSave();
    if( _effic ) _effic->AutoSave();
    if( _background ) _background->AutoSave();
    PHTFileServer::get().flush( _filename );

  }

}

//________________________________________________________
void MuonEval::write_timing_ntuple()
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
void MuonEval::write_stub_reco_ntuple()
{

  static int ievent=0;
  int arm, station, octant;

  ievent++;

  // Reconstructed di-muons ntuple
  if( !_stub_map ) return;

  for (arm = 0; arm < 2; arm++)
  for (station = 0; station < 3; station++)
  for (octant = 0; octant < 8; octant++){
    TMutStubMap::iterator stub_iter = _stub_map->get(arm,station,octant);
    while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
      float nt_vars[100] = {0};
      nt_vars[0] = stub_ptr->get()->get_arm();
      nt_vars[1] = stub_ptr->get()->get_station();
      nt_vars[2] = stub_ptr->get()->get_octant();
      nt_vars[3] = stub_ptr->get()->get_w_chi_square();
      nt_vars[4] = stub_ptr->get()->get_theta();
      nt_vars[5] = stub_ptr->get()->get_phi();
      nt_vars[6] = (float)ievent;
      TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
      nt_vars[7] = coord_iter.count();
      float w[6] = {0};
      int index;
      while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
        index = 2*coord_ptr->get()->get_gap() + coord_ptr->get()->get_cathode();
        w[index] = coord_ptr->get()->get_w_absolute();
      }
      if (w[0]!=0 && w[2] !=0) nt_vars[14] = w[0] - w[2];
      else nt_vars[14] = -999;

      if (w[2]!=0 && w[4] !=0) nt_vars[15] = w[2] - w[4];
      else nt_vars[15] = -999;

      if (w[0]!=0 && w[4] !=0) nt_vars[16] = w[0] - w[4];
      else nt_vars[16] = -999;

      if (w[1]!=0 && w[3] !=0) nt_vars[17] = w[1] - w[3];
      else nt_vars[17] = -999;

      if (w[3]!=0 && w[5] !=0) nt_vars[18] = w[3] - w[5];
      else nt_vars[18] = -999;

      if (w[1]!=0 && w[5] !=0) nt_vars[19] = w[1] - w[5];
      else nt_vars[19] = -999;

      TMutGapCoordMap::key_iterator gap_iter = stub_ptr->get()->get_associated<TMutGapCoord>();
      nt_vars[8] = gap_iter.count();
      nt_vars[9] = stub_ptr->get()->get_fit_par()->get_x();
      nt_vars[10] = stub_ptr->get()->get_fit_par()->get_y();
      nt_vars[11] = stub_ptr->get()->get_fit_par()->get_z();
      nt_vars[12] = stub_ptr->get()->get_fit_par()->get_dxdz();
      nt_vars[13] = stub_ptr->get()->get_fit_par()->get_dydz();
      _stub_reco->Fill(nt_vars);
    }
  }

  if( verbosity >= 1 ) cout << "MuonEval::write_stub_reco_ntuple.\n";
}

//________________________________________________________
void MuonEval::write_single_reco_ntuple()
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

  if (!_fvtx_trk_map ) {nt_vars[56] = 0;}
  else {
    TFvtxTrkMap::const_iterator fvtx_trk_iter_full = _fvtx_trk_map->range();
    nt_vars[56] = fvtx_trk_iter_full.count(); 
  }

  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
  {

    nt_vars[0] = trk_ptr->get()->get_arm();
    nt_vars[1] = trk_ptr->get()->get_trk_par_vtx()->get_px();
    nt_vars[2] = trk_ptr->get()->get_trk_par_vtx()->get_py();
    nt_vars[3] = trk_ptr->get()->get_trk_par_vtx()->get_pz();
    nt_vars[4] = trk_ptr->get()->get_trk_par_vtx()->get_ptot();
    nt_vars[5] = trk_ptr->get()->get_trk_par_vtx()->get_charge();
    nt_vars[6] = trk_ptr->get()->get_w_chi_square_pdf();
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
    float	n_reco_roads = reco_road_iter.count();
    if ( verbosity >= MUTOO::ALOT )
      {
      if( n_reco_roads > 3 )
         cout << "MuonEval::write_single_reco_ntuple - more than three roads associated to this track!!!" << endl; 

    //cout << "MuonEval::write_single_reco_ntuple - there are = " << n_reco_roads << " associated muID roads!" << endl;
      }

    float deepest_depth =0, deepest_chisq =0;
    float gap0_x=-999,gap0_y=-999;
    nt_vars[15] = 0;
    nt_vars[16] = -999;
    nt_vars[17] = -999;
    nt_vars[18] = -999;
    nt_vars[19] = -999;

    Float_t DG0 = 1000, DG0_temp = 0;
    Float_t DDG0 = 1000;
    Float_t scalar;
    Float_t p_mut, slope_mui;
    Float_t x3, y3, z3;
    Float_t dxdz3, dydz3;
    PHPoint gap0;
    PHPoint gap0_temp;
    Float_t x0_temp, y0_temp, z0_temp;
    Float_t dxdz0_temp, dydz0_temp, dxdz0, dydz0;


    nt_vars[63] = -999;
    nt_vars[64] = -999;

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

      // Calculate DG0, DDG0 for best road match:

      x3 = trk_ptr->get()->get_trk_par_station(2)->get_x();
      y3 = trk_ptr->get()->get_trk_par_station(2)->get_y();
      z3 = trk_ptr->get()->get_trk_par_station(2)->get_z();
      dxdz3 = trk_ptr->get()->get_trk_par_station(2)->get_px()/trk_ptr->get()->get_trk_par_station(2)->get_pz();
      dydz3 = trk_ptr->get()->get_trk_par_station(2)->get_py()/trk_ptr->get()->get_trk_par_station(2)->get_pz();
      p_mut = sqrt(MUTOO::SQUARE(trk_ptr->get()->get_trk_par_station(2)->get_px()) +
                   MUTOO::SQUARE(trk_ptr->get()->get_trk_par_station(2)->get_py()) +
                   MUTOO::SQUARE(trk_ptr->get()->get_trk_par_station(2)->get_pz()));
      gap0_temp = reco_road_ptr->get()->get_gap0_point();
      x0_temp = gap0_temp.getX();
      y0_temp = gap0_temp.getY();
      z0_temp = gap0_temp.getZ();
      dxdz0_temp = reco_road_ptr->get()->get_const_fitpar()->get_dxdz();
      dydz0_temp = reco_road_ptr->get()->get_const_fitpar()->get_dydz();
      slope_mui = sqrt(MUTOO::SQUARE(dxdz0_temp) + MUTOO::SQUARE(dydz0_temp) + 1);

      DG0_temp = sqrt( pow(x0_temp-x3-dxdz3*(z0_temp-z3),2) + pow(y0_temp-y3-dydz3*(z0_temp-z3),2) );

      if( DG0_temp < DG0 )
      {
        gap0 = gap0_temp;
        dxdz0 = dxdz0_temp;
        dydz0 = dydz0_temp;
        DG0 = DG0_temp;
        
        Float_t signz;
        signz = (trk_ptr->get()->get_trk_par_station(2)->get_pz() < 0) ? -1.0 : 1.0;
        scalar = (trk_ptr->get()->get_trk_par_station(2)->get_px()*dxdz0*signz + 
                  trk_ptr->get()->get_trk_par_station(2)->get_py()*dydz0*signz + 
                  trk_ptr->get()->get_trk_par_station(2)->get_pz()*signz)/(p_mut*slope_mui);
        if (scalar >= 1) scalar = 1;
        DDG0 = MUTOO::RAD_TO_DEG*acos( scalar );
        nt_vars[63] = DG0; 
        nt_vars[64] = DDG0;

      }

    }  // end of road loop

    //----------------
    //get associated MC track information
    //----------------
    TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();

    // How many associated MCTracks are there for this reco_trk?
    float	n_mc_tracks = mc_trk_iter.count();

    if ( verbosity >= MUTOO::ALOT )
      cout << "MuonEval::write_single_reco_ntuple - there are = " << n_mc_tracks << " associated tracks!" << endl;

    // Loop over associated MCTracks
    //	mc_trk_iter.reset();

    nt_vars[45] = 0;
    nt_vars[50] = 0;
    nt_vars[51] = 0;
    nt_vars[52] = 0;
    nt_vars[53] = -999.;
    nt_vars[54] = -999.;
    nt_vars[60] = -999;
    nt_vars[61] = -999;
    nt_vars[62] = -999;
    while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {

      // Record how many MuTr hits were made by this track:
      TMutMCHitMap::const_key_iterator mut_mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
      nt_vars[51] = mut_mc_hit_iter.count();

      // See if there are any found FVTX tracks associated with this MC track:
      TFvtxTrkMap::key_iterator mc_fvtx_trk_iter = mc_trk_ptr->get()->get_associated<TFvtxTrk>();
      nt_vars[52] = mc_fvtx_trk_iter.count();

      // Calculate matching distance for the tracks which should have been associated:

      while( TFvtxTrkMap::const_pointer fvtx_trk_ptr = mc_fvtx_trk_iter.next() ) {

        if (!fvtx_trk_ptr->get()->get_reco_success()) continue;
        if (!trk_ptr->get()->get_reco_success()) continue;

        nt_vars[60] = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_px();
        nt_vars[61] = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_py();
        nt_vars[62] = fvtx_trk_ptr->get()->get_trk_par_vtx()->get_pz();

        //Calculate matching parameters for FVTX-MuTr tracks:
        PHTrackIntegratorKF integrator;
        float zref = 40.0*(2*trk_ptr->get()->get_arm() - 1 );
        if (fvtx_trk_ptr->get()->get_reco_success()){
          integrator.initialize( fvtx_trk_ptr->get()->get_trk_par_list()->back() );
          integrator.extrapolate ( zref );
          TMutTrkPar fvtx_extrap_trk_par;
          if (!integrator.get_error()){
            integrator.finish( fvtx_extrap_trk_par );
          }

          TMutTrkPar trk_par_st1(*trk_ptr->get()->get_trk_par_station(MUTOO::Station1));

          bool error( false );
          PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
          if( !error )
          {
            // fit together with the vertex
            mMutFitVtx::Fitter vertex_fitter;
            vertex_fitter.add_track( trk_ptr );
  
            // disable usage of X and Y
            vtx.setX(0);
            vtx.setY(0);
  
            // set vertex errors manually
            PHPoint vtx_error( 0.1, 0.1, 0.5 );
            //PHPoint vtx_error( 0.5, 0.5, 2.5 );
  
            // add vertex information to fitter and do the fit
            vertex_fitter.add_vertex( vtx, vtx_error );
            vertex_fitter.fit();
  
            //cout << "covar = " << sqrt(trk_par.get_covar(0,0)) << endl;
  
            // store fit result in trk_par
            trk_par_st1 = TMutTrkPar(
              vertex_fitter.get_vtx_x(),
              vertex_fitter.get_vtx_y(),
              vertex_fitter.get_vtx_z(),
              vertex_fitter.get_px(0),
              vertex_fitter.get_py(0),
              vertex_fitter.get_pz(0),
              static_cast<int>(trk_ptr->get()->get_charge() ),
              vertex_fitter.get_chisquare() );
          }
          integrator.initialize( trk_par_st1 );
          integrator.extrapolate ( zref );

          TMutTrkPar extrap_trk_par;
          if (integrator.get_error()){
            cout << "in mFvtxKalFitWithSiliReal extraploation to FVTX failed" << endl;
            extrap_trk_par = trk_par_st1;
          } else {
            integrator.finish( extrap_trk_par );
          }

          PHVector direction_fvtx(fvtx_extrap_trk_par.get_px(), fvtx_extrap_trk_par.get_py(), fvtx_extrap_trk_par.get_pz());

          PHVector direction_extrap(extrap_trk_par.get_px(), extrap_trk_par.get_py(), extrap_trk_par.get_pz());
          double dangle = direction_fvtx.angle( direction_extrap );

          double distance = sqrt( FVTXOO::SQUARE(extrap_trk_par.get_x() - fvtx_extrap_trk_par.get_x()) +
            FVTXOO::SQUARE(extrap_trk_par.get_y() - fvtx_extrap_trk_par.get_y()));
          nt_vars[53] = (float)distance;
          nt_vars[54] = (float)dangle;
        }
      }

      // Record how many FVTX hits were made by this track:
      TFvtxMCHitMap::key_iterator fvtx_mchit_iter = mc_trk_ptr->get()->get_associated<TFvtxMCHit>();
      nt_vars[50] = fvtx_mchit_iter.count();

      //  "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
      //  "r_gap0:road_depth:ntrks:ievent:ptotus:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
      //  "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
      //  "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus") ;

      //  _single_reco = new TNtuple("single_reco","single_reco",
      //	     "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
      //	     "r_gap0:road_depth:ntrks:ievent:run:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
      //	     "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
      //	     "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus:reco_px:reco_py:reco_pz:"
      //	     "reco_ptot:prnt_trkid:prnt_px:prnt_py:prnt_pz") ;

      //MC information variables
      //	  float   arm = mc_trk_ptr->get()->get_arm();
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
      if ( verbosity >= MUTOO::ALOT ) 
        cout
          << "MuonEval::write_single_reco_ntuple - looking for particle "
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
        if ( verbosity >= MUTOO::ALOT )
          cout << "MuonEval::write_single_reco_ntuple - in the MCbank,  MC track id trk_IDx == " << trk_IDx <<  endl;

        if ( abs(prnt_trk_ID) == abs(trk_IDx) )
        {

          // found the parent MC track
          Float_t prnt_IDx = mc_trk_ptr_1p->get()->get_parent_id();
          if ( verbosity >= MUTOO::ALOT )
            cout << "MuonEval::write_single_reco_ntuple - found parent particle == parent = " << prnt_IDx << endl;

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

    }//loop MC track back for the reco track

    // See if there are any FVTX tracks associated with this track:

    TFvtxTrkMap::key_iterator fvtx_trk_iter = trk_ptr->get()->get_associated<TFvtxTrk>();
    nt_vars[57] = -999;
    nt_vars[58] = -999;
    nt_vars[59] = -999;
    if (fvtx_trk_iter.count() > 0) {
      // Got a matching track
      nt_vars[45] = 1;

      // Retrieve track params:
      nt_vars[47] = -999.;
      nt_vars[48] = -999.;
      nt_vars[49] = -999.;
      nt_vars[67] = -999.;
      while( TFvtxTrkMap::const_pointer fvtx_trk_ptr = fvtx_trk_iter.next() ) {

        const TMutTrkPar* fvtxmutr_trk_par = fvtx_trk_ptr->get()->get_trk_par_mutr();

        nt_vars[47] = fvtxmutr_trk_par->get_chi_square();

        nt_vars[57] = fvtxmutr_trk_par->get_px();
        nt_vars[58] = fvtxmutr_trk_par->get_py();
        nt_vars[59] = fvtxmutr_trk_par->get_pz();

        nt_vars[67] = fvtx_trk_ptr->get()->get_w_chi_square_pdf();

        //Calculate matching parameters for FVTX-MuTr tracks:
        PHTrackIntegratorKF integrator;
        float zref = 40.0*(2*trk_ptr->get()->get_arm() - 1 );
        if (fvtx_trk_ptr->get()->get_reco_success()){
          integrator.initialize( fvtx_trk_ptr->get()->get_trk_par_list()->back() );
          integrator.extrapolate ( zref );
          TMutTrkPar fvtx_extrap_trk_par;
          if (!integrator.get_error()){
            integrator.finish( fvtx_extrap_trk_par );
          }

          TMutTrkPar trk_par_st1(*trk_ptr->get()->get_trk_par_station(MUTOO::Station1));

          bool error( false );
          PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
          if( !error )
          {
            // fit together with the vertex
            mMutFitVtx::Fitter vertex_fitter;
            vertex_fitter.add_track( trk_ptr );
  
            // disable usage of X and Y
            vtx.setX(0);
            vtx.setY(0);
  
            // set vertex errors manually
            PHPoint vtx_error( 0.1, 0.1, 0.5 );
            //PHPoint vtx_error( 0.5, 0.5, 2.5 );
  
            // add vertex information to fitter and do the fit
            vertex_fitter.add_vertex( vtx, vtx_error );
            vertex_fitter.fit();
  
            //cout << "covar = " << sqrt(trk_par.get_covar(0,0)) << endl;
  
            // store fit result in trk_par
            trk_par_st1 = TMutTrkPar(
              vertex_fitter.get_vtx_x(),
              vertex_fitter.get_vtx_y(),
              vertex_fitter.get_vtx_z(),
              vertex_fitter.get_px(0),
              vertex_fitter.get_py(0),
              vertex_fitter.get_pz(0),
              static_cast<int>(trk_ptr->get()->get_charge() ),
              vertex_fitter.get_chisquare() );
          }
          integrator.initialize( trk_par_st1 );
          integrator.extrapolate ( zref );

          TMutTrkPar extrap_trk_par;
          if (integrator.get_error()){
            cout << "in mFvtxKalFitWithSiliReal extraploation to FVTX failed" << endl;
            extrap_trk_par = trk_par_st1;
          } else {
            integrator.finish( extrap_trk_par );
          }

          PHVector direction_fvtx(fvtx_extrap_trk_par.get_px(), fvtx_extrap_trk_par.get_py(), fvtx_extrap_trk_par.get_pz());

          PHVector direction_extrap(extrap_trk_par.get_px(), extrap_trk_par.get_py(), extrap_trk_par.get_pz());
          double dangle = direction_fvtx.angle( direction_extrap );

          double distance = sqrt( FVTXOO::SQUARE(extrap_trk_par.get_x() - fvtx_extrap_trk_par.get_x()) +
            FVTXOO::SQUARE(extrap_trk_par.get_y() - fvtx_extrap_trk_par.get_y()));
          nt_vars[48] = (float)distance;
          nt_vars[49] = (float)dangle;
        }

        // Retrieve number of hits on this track:
        TFvtxCoordMap::key_iterator trk_coord_iter = fvtx_trk_ptr->get()->get_associated<TFvtxCoord>();
        TFvtxSvxClusterMap::key_iterator trk_svx_iter = fvtx_trk_ptr->get()->get_associated<TFvtxSvxCluster>();

        nt_vars[65] = trk_coord_iter.count();
        nt_vars[66] = trk_svx_iter.count();


        // Retrieve MC info for this track, if there is any
        TMutMCTrkMap::key_iterator fvtx_mc_trk_iter = fvtx_trk_ptr->get()->get_associated<TMutMCTrk>();
        nt_vars[46] = 0;
        while( TMutMCTrkMap::const_pointer fvtx_mc_trk_ptr = fvtx_mc_trk_iter.next() ) {
          nt_vars[46] = fvtx_mc_trk_ptr->get()->get_track_id();
        }
      }
      
    }
    else {
      nt_vars[45] = 0;
      nt_vars[46] = -999;
      nt_vars[47] = -999;
      nt_vars[48] = -999;
      nt_vars[49] = -999;
    }

    // ---- write out ntuple ---
    _single_reco->Fill(nt_vars);

    if ( verbosity >= MUTOO::ALOT )
      cout << "MuonEval::write_single_reco_ntuple - there are = " << mc_trk_iter.count() << " associated tracks!" << endl;

  }  // Loop over tracks

  if( verbosity >= 1 ) cout << "MuonEval::write_single_reco_ntuple" << endl;
}

//________________________________________________________
void MuonEval::write_clus_ntuple()
{
  static int EventNum = 1;
  MutCalibStrip *CalibPointer = MutCalib();
  float nt_vars[100] = {0};

  // Reconstructed cluster ntuple
  //

  if( !_clus_map ) return;
  TMutClusMap::iterator clus_iter = _clus_map->range();
  while(TMutClusMap::pointer clus_ptr = clus_iter.next()) {
    vector<UShort_t> MCTracks;

    Int_t arm     = clus_ptr->get()->get_arm();
    Int_t station = clus_ptr->get()->get_station();
    Int_t octant  = clus_ptr->get()->get_octant();
    Int_t half    = clus_ptr->get()->get_half_octant();
    Int_t gap     = clus_ptr->get()->get_gap();
    Int_t   cathode = clus_ptr->get()->get_cathode();
    TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
    Float_t nhits   = hit_iter.count();

    // double check if associated TMutHits are ordered left to right...???? ( )  check when confirmed july 3, 2004 j.nagle

    Int_t BadMiddleStrip=0;
    Int_t BadEdgeStrip=0;
    bool  LeftEdgeDetector = false;
    bool  RightEdgeDetector = false;
    Int_t Saturation = 0;
    Int_t AttenStrip = 0;
    Int_t BadCalib = 0;
    Int_t NoCalib = 0;

    Int_t   Strip_number[8] = {0,0,0,0,0,0,0,0};
    Float_t Strip_q[8] = {0,0,0,0,0,0,0,0};
    Float_t Strip_q_err[8] = {0,0,0,0,0,0,0,0};
    Float_t qpeak = 0;

    Int_t index = 0;
    // Loop over all strips associated with the cluster
    while(TMutHitMap::pointer hit_ptr = hit_iter.next()) {

      // only store up to 8 values
      if (index == 8) break;

      Strip_number[index] = hit_ptr->get()->get_strip();
      Strip_q[index] = hit_ptr->get()->get_q();
      Strip_q_err[index] = hit_ptr->get()->get_error_q();
      qpeak = (Strip_q[index]>qpeak)? Strip_q[index]:qpeak;

      Int_t strip = hit_ptr->get()->get_strip();
      // For each strip check a number of issues - go through list below

      // (1) is there a bad (q_error) middle strip? - if it is one strip wide then both an edge and middle!
      if (hit_iter.count()==1 || !(index==0 || index==static_cast<int>(hit_iter.count())-1)) {
  if (hit_ptr->get()->get_error_q() > 20.0) {
    BadMiddleStrip++;
  }
      }

      // (2) is there a bad (q_error) edge strip?
      if (index==0 || index==static_cast<int>(hit_iter.count())-1) {   // double check this logic count-1
  if (hit_ptr->get()->get_error_q() > 20.0) {
    BadEdgeStrip++;
  }
      }

      // (3) is the cluster at the edge of the detector (left edge),(right edge)?
      Int_t NumberOfStrips =  TMutGeo::get_n_strip(arm, station, octant, half, gap, cathode) - 1;
      if (index==0) {
  if (strip == 1) LeftEdgeDetector = true;
      }
      if (index==static_cast<int>(hit_iter.count())-1) {
  if (strip == NumberOfStrips) RightEdgeDetector = true;
      }

      // (3 1/2) is there a bad calibration
      // set calib pointer
      const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip(arm, station, octant, half, gap, cathode, strip);
      if (StripCalib) {
  if (! (StripCalib->isValid()) ) {
    BadCalib++;
  }
      } else {
  NoCalib++;
      }

      // (4) is there an attenuated strip? - should we add which one? (edge or middle?????)
      MutStrip* strip_ptr;
      strip_ptr = TMutGeo::get_strip_geom(arm, station, octant, half, gap, cathode, strip);
      if (strip_ptr) {
  if (strip_ptr->UseAttenuation()) AttenStrip++;
      }

      // (5) is there a saturated strip?
      Int_t DACSaturation, ADCSaturation;
      bool this_strip_saturation = false;
      if (StripCalib) {
  Float_t gain = StripCalib->get_gain();
  // Float_t rms = StripCalib->get_rms();
  DACSaturation = StripCalib->getSaturation();
  ADCSaturation = StripCalib->getAdc(DACSaturation);
  // loop over all ADC samples and see if any are saturated...
  for (UShort_t j=0; j<4; ++j){
    UShort_t adc = hit_ptr->get()->get_adc(j);
    // set saturation bool if adc is out of range or gain is below
    // minimum value.  (gain conditions should be handled by bad
    // channel map)
    //
    /*
      if (adc==MUTOO_FEM::ADC_MIN || gain < _mod_par->get_min_gain()
      || (adc<=ADCSaturation && adc<=MUTOO_FEM::SATURATED_ADC_ERROR)
      || !StripCalib->isValid() ){
    */
    // I believe the mMutCalibratePar.h sets min_gain(0) by default and is never reset
    if (adc==MUTOO_FEM::ADC_MIN || gain < 0 // hard code this value until you get _mod_par working :::  _mod_par->get_min_gain()
        || (adc<=ADCSaturation && adc<=MUTOO_FEM::SATURATED_ADC_ERROR)
        || !StripCalib->isValid() ){
      this_strip_saturation = true;
    }

  }
      }
      if (this_strip_saturation) Saturation++;

      index++;  // increment to next strip

    } // end loop over all hits in this cluster

      // loop over all coordinates associated with the cluster
    TMutCoordMap::key_iterator coord_iter = clus_ptr->get()->get_associated<TMutCoord>();
    Float_t num_coord =  coord_iter.count();  // number of coordinates fit for this cluster
    Float_t clus_fit_chisq = clus_ptr->get()->get_chi_square();

    Int_t ncoord  = 0;
    Int_t nmchits = 0;

    Float_t w_value[4] = {0,0,0,0};     // store up to four w fit values
    UShort_t peak_value[4] = {0,0,0,0};     // store up to four w fit values
    Float_t w_value_err[4] = {0,0,0,0}; // store all four even though they all get the same error

    Float_t w_true[4] = {0,0,0,0};
    Float_t w_trueg[4] = {0,0,0,0};

    while(TMutCoordMap::pointer coord_ptr = coord_iter.next())
    {

      if (ncoord < 4)
      {
        w_value[ncoord] = coord_ptr->get()->get_w();
        peak_value[ncoord] = coord_ptr->get()->get_peak_strip();
        w_value_err[ncoord] = coord_ptr->get()->get_error();
        nt_vars[58+ncoord] = coord_ptr->get()->get_q_tot();
      }

      TMutMCHitMap::key_iterator mc_hit_iter = coord_ptr->get()->get_associated<TMutMCHit>();
      while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next())
      {
        bool found = false;
        vector<UShort_t>::iterator MCTracks_iter = MCTracks.begin();
        for (MCTracks_iter=MCTracks.begin();MCTracks_iter!=MCTracks.end();++MCTracks_iter)
        {
          if (mc_hit_ptr->get()->get_track_id()==(*MCTracks_iter))
          {
            found = true;
            break;
          }
        }
        if (!found){
          MCTracks.push_back(mc_hit_ptr->get()->get_track_id());
          nmchits++;
        }

        // only store up to 4 values
        if (nmchits < 4) {
          MutStrip* strip_array[2] = {0};
          double strip_ip_array[2] = {0};
          MutWire* wire_array[1] = {0};
          double wire_ip=0;
          float ReadoutSpacing;

          w_true[nmchits-1] = mc_hit_ptr->get()->get_w_true((UShort_t)cathode);
          PHPoint hit_point(mc_hit_ptr->get()->get_x(),
            mc_hit_ptr->get()->get_y(),
            mc_hit_ptr->get()->get_z());
          MutArm* geometry = (clus_ptr->get()->get_arm()== MUTOO::South) ? SouthArm():NorthArm();
          geometry->convertPisaHitToChamber(hit_point,
            wire_array,
            wire_ip,
            strip_array,
            strip_ip_array);
          int icath = clus_ptr->get()->get_cathode();
          if (strip_array[icath]){
            int icath = clus_ptr->get()->get_cathode();
            ReadoutSpacing = (float)(strip_array[icath]->getStripSpacing());
            w_trueg[nmchits-1] = (float)strip_array[icath]->getStrip()
              + strip_ip_array[icath]/ReadoutSpacing;

          } else {
            w_trueg[nmchits-1] = -999;
          }

        }
      }

      ncoord++;

    }

    Float_t num_mc_contrib = nmchits;

    // now fill the ntuple entries ------------------------------------------------------

    nt_vars[0] = (float) EventNum;
    nt_vars[1] = arm;
    nt_vars[2] = station;
    nt_vars[3] = octant;
    nt_vars[4] = half;

    nt_vars[5] = gap;
    nt_vars[6] = cathode;
    nt_vars[7] = nhits;   // same as the width of the cluster
    nt_vars[8] = clus_fit_chisq;   // double check that this is per dof ( )
    nt_vars[9] = num_coord;   // number of coordinates fit to the cluster

    nt_vars[10] = num_mc_contrib;  // number of mc hits contributing to the cluster
    nt_vars[11] = BadMiddleStrip;
    nt_vars[12] = BadEdgeStrip;
    nt_vars[13] = LeftEdgeDetector;
    nt_vars[14] = RightEdgeDetector;

    nt_vars[15] = Saturation;
    nt_vars[16] = AttenStrip;
    nt_vars[17] = BadCalib;
    nt_vars[18] = NoCalib;

    // store 8 strip values, 8 q values, 8 qerror values
    for (Int_t k=0;k<8;k++) nt_vars[19+k] = Strip_number[k];
    for (Int_t k=0;k<8;k++) nt_vars[27+k] = Strip_q[k];
    for (Int_t k=0;k<8;k++) nt_vars[35+k] = Strip_q_err[k];

    // store 4 w values, 4 w_error values, 4 w_true values
    for (Int_t k=0;k<4;k++) nt_vars[43+k] = w_value[k];
    for (Int_t k=0;k<4;k++) nt_vars[47+k] = w_value_err[k];
    for (Int_t k=0;k<4;k++) nt_vars[51+k] = w_true[k];
    for (Int_t k=0;k<4;k++) nt_vars[61+k] = peak_value[k] + w_value[k];
    nt_vars[65] = qpeak;
    for (Int_t k=0;k<4;k++) nt_vars[66+k] = w_trueg[k];

    vector<UShort_t>::iterator MCTracks_iter = MCTracks.begin();
    int itrack = 0;
    for (MCTracks_iter=MCTracks.begin();MCTracks_iter!=MCTracks.end();++MCTracks_iter){
      if (itrack<4) nt_vars[55+itrack] = (*MCTracks_iter);
      else break;
      itrack++;
    }

    _clus_reco->Fill(nt_vars);

  } // end loop over clusters

  EventNum++;

}


//________________________________________________________
void  MuonEval::write_reco_ntuple()
{
  static int evt_num = 1;

  bool good_pos_north=false;
  bool good_pos_south=false;
  bool good_neg_north=false;
  bool good_neg_south=false;

  // Looping over Monte Carlo tracks to determine if this event has a "good" positive and negative
  // muon that leaves mc hits in stations 1,2,3 of mutr (good_pos_mutr_south for example) and
  // if the muon leaves mc hits in at least gaps 0,1,2 of muid (good_pos_muid_south for example)
  // Note that this just checks for MC hits, and not a reconstructed hit !

  // check MC trk map
  if( !_mc_trk_map ) return;

  TMutMCTrkMap::const_iterator mc_trk_iter = _mc_trk_map->range();
  while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {

    // only consider muons - pid = 5,6
    if (! ((mc_trk_ptr->get()->get_pid()==5) || (mc_trk_ptr->get()->get_pid()==6)) ) continue;

    // check if monte carlo muon penetrates through MUID gaps 0,1,2
    bool good_muid = false;
    if ( get_mc_trk_depth( mc_trk_ptr ) < 2) good_muid = true;

    // check if monte carlo muon leaves hits in stations 1,2,3
    TMutMCHitMap::const_key_iterator hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
    bool st1 = false; bool st2 = false; bool st3 = false;
    // TMutMCHit also has get_x, get_y, get_z calls
    while (TMutMCHitMap::const_pointer hit_ptr = hit_iter.next()) {
      if (hit_ptr->get()->get_station() == MUTOO::Station1) st1 = true;
      if (hit_ptr->get()->get_station() == MUTOO::Station2) st2 = true;
      if (hit_ptr->get()->get_station() == MUTOO::Station3) st3 = true;
    }

    // good monte carlo muon all the way through muid gap 2
    if ((good_muid) && (st1 && st2 && st3) && (mc_trk_ptr->get()->get_arm()==0) && (mc_trk_ptr->get()->get_charge()>0)) good_pos_south=true;
    if ((good_muid) && (st1 && st2 && st3) && (mc_trk_ptr->get()->get_arm()==0) && (mc_trk_ptr->get()->get_charge()<0)) good_neg_south=true;
    if ((good_muid) && (st1 && st2 && st3) && (mc_trk_ptr->get()->get_arm()==1) && (mc_trk_ptr->get()->get_charge()>0)) good_pos_north=true;
    if ((good_muid) && (st1 && st2 && st3) && (mc_trk_ptr->get()->get_arm()==1) && (mc_trk_ptr->get()->get_charge()<0)) good_neg_north=true;


  } // end loop over MC tracks

  //--------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------

  // Only do study for dimuon "accepted" events as defined above
  if ((good_pos_south && good_neg_south) || (good_pos_north && good_neg_north)) {

    // Loop over all reconstructed vertices (dimuons)
    if( !_vtx_map ) return;
    TMutVtxMap::iterator vtx_iter = _vtx_map->range();

    // Count the number events with both tracks associated with a TMutMCTrk
    //
    UShort_t nvtx=0;
    while(TMutVtxMap::pointer vtx_ptr = vtx_iter.next()){
      bool got_positive=false;
      bool got_negative=false;
      TMutTrkMap::key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
      while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
      {
        TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
        if(!mc_trk_iter.at_end() && mc_trk_iter->get()->get_charge() == 1.0) got_positive = true;
        if(!mc_trk_iter.at_end() && mc_trk_iter->get()->get_charge() == -1.0) got_negative = true;
      }
      if(got_positive && got_negative) ++nvtx;
    }

    // Reset the iterator
    //
    vtx_iter.reset();
    while(TMutVtxMap::pointer vtx_ptr = vtx_iter.next())
    {

      float ntvar[100] = {0};

      // Capture iterators to associated MC tracks if they exist
      //
      TMutMCTrkMap::value_type mc_pos_ptr;
      TMutMCTrkMap::value_type mc_neg_ptr;
      TMutTrkMap::value_type pos_ptr;
      TMutTrkMap::value_type neg_ptr;

      // Loop over reconstructed tracks associated with vertex
      TMutTrkMap::key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
      while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
      {

        // determine which is the positive and negative track
        if (trk_ptr->get()->get_charge() ==  1.0) pos_ptr = *trk_ptr;
        if (trk_ptr->get()->get_charge() == -1.0) neg_ptr = *trk_ptr;

        // determine the associated Monte Carlo track
        TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
        if(!mc_trk_iter.at_end() && mc_trk_iter->get()->get_charge() == 1.0 && mc_trk_iter->get()->get_pid()==5) {
          mc_pos_ptr = *mc_trk_iter;
          pos_ptr = *trk_ptr;
        }
        if(!mc_trk_iter.at_end() && mc_trk_iter->get()->get_charge() == -1.0 && mc_trk_iter->get()->get_pid()==6) {
          mc_neg_ptr = *mc_trk_iter;
          neg_ptr = *trk_ptr;
        }
      }

      // assuming we have both a positive and negative muon monte carlo association
      if(mc_pos_ptr && mc_neg_ptr) {

        // positive track
        //
        ntvar[0] = mc_pos_ptr->get_arm();                   // arm
        ntvar[1] = mc_pos_ptr->get_ptot_us_gap();           // ptot
        ntvar[2] = mc_pos_ptr->get_px_us_gap();             // px
        ntvar[3] = mc_pos_ptr->get_py_us_gap();             // py
        ntvar[4] = mc_pos_ptr->get_pz_us_gap();             // pz
        ntvar[5] = mc_pos_ptr->get_associated<TMutTrk>().count();
        ntvar[50] = get_mc_trk_depth( &mc_pos_ptr );         // depth of penetration for monte carlo muon in muid

        // negative track
        ntvar[10] = mc_neg_ptr->get_arm();                   // arm
        ntvar[11] = mc_neg_ptr->get_ptot_us_gap();           // ptot
        ntvar[12] = mc_neg_ptr->get_px_us_gap();             // px
        ntvar[13] = mc_neg_ptr->get_py_us_gap();             // py
        ntvar[14] = mc_neg_ptr->get_pz_us_gap();             // pz
        ntvar[15] = mc_neg_ptr->get_associated<TMutTrk>().count();
        ntvar[51] = get_mc_trk_depth( &mc_neg_ptr );

        // fill some vertex information
        //
        ntvar[20] = 0;                                              // not a ghost
        ntvar[21] = vtx_iter.count();
        ntvar[22] = vtx_ptr->get()->get_mass();
        ntvar[23] = vtx_ptr->get()->get_sign();
        ntvar[24] = nvtx;     // total number of vertices for this event

      }  else {

        // case where both reconstructed tracks not associated with MC tracks

        ntvar[20] = ( mc_neg_ptr || mc_pos_ptr ) ? 1 : 2;           // ghost
        ntvar[21] = vtx_iter.count();
        ntvar[22] = vtx_ptr->get()->get_mass();
        ntvar[23] = vtx_ptr->get()->get_sign();
        ntvar[24] = 0;  // not so relevant for case of ghost > 0

      }

      // Write the hit multiplicity to the ntuple
      if( !_hit_map ) return;
      ntvar[25] = _hit_map->get(MUTOO::North,MUTOO::Station1).count() +     // number of mutr north hits
        _hit_map->get(MUTOO::North,MUTOO::Station2).count() +
        _hit_map->get(MUTOO::North,MUTOO::Station3).count();
      ntvar[26] = _hit_map->get(MUTOO::South,MUTOO::Station1).count() +     // number of mutr south hits
        _hit_map->get(MUTOO::South,MUTOO::Station2).count() +
        _hit_map->get(MUTOO::South,MUTOO::Station3).count();

      ntvar[27] = TMutErrorStats::is_error();
      ntvar[42] = TMutErrorStats::is_error(TMutErrorStats::CLONE_TRACK);       // track clone errors
      ntvar[43] = TMutErrorStats::is_error(TMutErrorStats::STUB_BIFURCATE);    // stub bifurcation errors

      ntvar[28] = evt_num;
      ntvar[29] = vtx_ptr->get()->get_chi_square();

      // reconstructed ptot and pt
      ntvar[60] = vtx_ptr->get()->get_ptot();
      ntvar[61] = vtx_ptr->get()->get_pt();

      ntvar[62] = vtx_ptr->get()->get_x();
      ntvar[63] = vtx_ptr->get()->get_y();
      ntvar[64] = vtx_ptr->get()->get_z();
      ntvar[65] = vtx_ptr->get()->get_ndf();
      ntvar[66] = gsl_ran_chisq_pdf( vtx_ptr->get()->get_chi_square(), vtx_ptr->get()->get_ndf() );
      ntvar[66] = -1;

      // positive track info
      if (pos_ptr) {

        ntvar[0] = pos_ptr->get_arm();                   // arm
        ntvar[6] = pos_ptr->get_trk_par()->get_ptot();
        ntvar[7] = pos_ptr->get_trk_par()->get_px();
        ntvar[8] = pos_ptr->get_trk_par()->get_py();
        ntvar[9] = pos_ptr->get_trk_par()->get_pz();
        ntvar[30] = pos_ptr->get_w_chi_square();
        ntvar[31] = pos_ptr->get_ndf();
        ntvar[32] = pos_ptr->get_n_coord();

        // Loop over all associated MUID roads
        // fill information for deepest road and "best" road (as determined below)
        TMuiRoadMapO::key_iterator road_iter = pos_ptr->get_associated<TMuiRoadO>();
        UShort_t tempdepth = 0;
        UShort_t numroads = 0;
        float    bestroadchi = 1e6;

        // loop over roads and capture data of deepest one
        while( TMuiRoadMapO::pointer road_ptr = road_iter.next() ) {
          numroads++;
          UShort_t depth = road_ptr->get()->get_depth();
          // pull out some track information
          const TMutTrkPar* st3_trk_par = pos_ptr->get_trk_par_station(2);  // double check this indexing method???
          const TMutFitPar* trk_fit_par = new TMutFitPar(st3_trk_par->get_x(),
            st3_trk_par->get_y(),
            st3_trk_par->get_z(),
            (st3_trk_par->get_px())/(st3_trk_par->get_pz()),
            (st3_trk_par->get_py())/(st3_trk_par->get_pz()),
            0);
          PHPoint road_point = road_ptr->get()->get_gap0_point();
          const TMutFitPar* road_fit_par = road_ptr->get()->get_const_fitpar();
          PHPoint trk_point  = TMutTrackUtil::linear_track_model(trk_fit_par,
            road_point.getZ());
          float distance = PHGeometry::distancePointToPoint(road_point,trk_point);
          float r_trk  = sqrt((trk_fit_par->get_dxdz())*(trk_fit_par->get_dxdz())+(trk_fit_par->get_dydz())*(trk_fit_par->get_dydz())+1);
          float r_muid = sqrt((road_fit_par->get_dxdz())*(road_fit_par->get_dxdz())+(road_fit_par->get_dydz())*(road_fit_par->get_dydz())+1);

          const PHVector trk_vect(trk_fit_par->get_dxdz()/r_trk,
            trk_fit_par->get_dydz()/r_trk,
            1/r_trk);
          const PHVector muid_vect(road_fit_par->get_dxdz()/r_muid,
            road_fit_par->get_dydz()/r_muid,
            1/r_muid);
          float delta_theta = acos(trk_vect.dot(muid_vect));
          float chi = sqrt(distance*distance/((18.0*18.0))+
            delta_theta*delta_theta/((0.18*0.18)));
          // fill distance, delta_theta, and chi into ntuple...
          if( depth > tempdepth) {
            tempdepth = road_ptr->get()->get_depth();
            ntvar[36] = depth;
            ntvar[37] = road_ptr->get()->get_nhit();
            ntvar[38] = road_ptr->get()->get_const_fitpar()->get_chi_square();
            ntvar[44] = distance;
            ntvar[45] = delta_theta;
            ntvar[46] = chi;
          }
          if (chi < bestroadchi) {
            bestroadchi = chi;
            ntvar[54] = depth;
            ntvar[55] = road_ptr->get()->get_nhit();
            ntvar[56] = road_ptr->get()->get_const_fitpar()->get_chi_square();
            ntvar[57] = distance;
            ntvar[58] = delta_theta;
            ntvar[59] = chi;
          }
        }
        ntvar[52] = numroads;

      }

      // negative track info
      if (neg_ptr) {

        ntvar[10] = neg_ptr->get_arm();                   // arm
        ntvar[16] = neg_ptr->get_trk_par()->get_ptot();
        ntvar[17] = neg_ptr->get_trk_par()->get_px();
        ntvar[18] = neg_ptr->get_trk_par()->get_py();
        ntvar[19] = neg_ptr->get_trk_par()->get_pz();
        ntvar[33] = neg_ptr->get_w_chi_square();
        ntvar[34] = neg_ptr->get_ndf();
        ntvar[35] = neg_ptr->get_n_coord();

        // reset testing variables
        TMuiRoadMapO::key_iterator road_iter = neg_ptr->get_associated<TMuiRoadO>();
        // just keeping the information for the deepest road (these are reconstructed roads)
        UShort_t tempdepth = 0;
        UShort_t numroads = 0;
        // loop over roads for negative track
        while( TMuiRoadMapO::pointer road_ptr = road_iter.next() ) {
          numroads++;
          UShort_t depth = road_ptr->get()->get_depth();
          // only keeping the deepest road....
          if( depth > tempdepth) {
            ntvar[39] = depth;
            ntvar[40] = road_ptr->get()->get_nhit();
            ntvar[41] = road_ptr->get()->get_const_fitpar()->get_chi_square();

            // can we add the spatial proximity value and the angle match value mutr-muid...????
            // should these only be added for the reconstructed case??? - not a bad start...
            tempdepth = road_ptr->get()->get_depth();

            // pull out some track information
            const TMutTrkPar* st3_trk_par = neg_ptr->get_trk_par_station(2);  // double check this indexing method???
            const TMutFitPar* trk_fit_par = new TMutFitPar(st3_trk_par->get_x(),
              st3_trk_par->get_y(),
              st3_trk_par->get_z(),
              (st3_trk_par->get_px())/(st3_trk_par->get_pz()),
              (st3_trk_par->get_py())/(st3_trk_par->get_pz()),
              0);
            PHPoint road_point = road_ptr->get()->get_gap0_point();
            const TMutFitPar* road_fit_par = road_ptr->get()->get_const_fitpar();
            PHPoint trk_point  = TMutTrackUtil::linear_track_model(trk_fit_par,
              road_point.getZ());
            float distance = PHGeometry::distancePointToPoint(road_point,trk_point);

            float r_trk  = sqrt((trk_fit_par->get_dxdz())*(trk_fit_par->get_dxdz())+(trk_fit_par->get_dydz())*(trk_fit_par->get_dydz())+1);
            float r_muid = sqrt((road_fit_par->get_dxdz())*(road_fit_par->get_dxdz())+(road_fit_par->get_dydz())*(road_fit_par->get_dydz())+1);

            const PHVector trk_vect(trk_fit_par->get_dxdz()/r_trk,
              trk_fit_par->get_dydz()/r_trk,
              1/r_trk);
            const PHVector muid_vect(road_fit_par->get_dxdz()/r_muid,
              road_fit_par->get_dydz()/r_muid,
              1/r_muid);
            float delta_theta = acos(trk_vect.dot(muid_vect));
            float chi = sqrt(distance*distance/((18.0*18.0))+
              delta_theta*delta_theta/((0.18*0.18)));
            // fill distance, delta_theta, and chi into ntuple...
            ntvar[47] = distance;
            ntvar[48] = delta_theta;
            ntvar[49] = chi;

          }
        }
        ntvar[53] = numroads;
      }
      _dimu_reco->Fill(ntvar);

    } // end of loop over reconstructed vertices

    // Write the hit multiplicity of these events to a separate per event ntuple
    float ntvar[100] = {0};
    if( !_hit_map ) return;
    ntvar[0] = evt_num;
    ntvar[1] = _hit_map->get(MUTOO::North,MUTOO::Station1).count() +
      _hit_map->get(MUTOO::North,MUTOO::Station2).count() +
      _hit_map->get(MUTOO::North,MUTOO::Station3).count();
    ntvar[2] = _hit_map->get(MUTOO::South,MUTOO::Station1).count() +
      _hit_map->get(MUTOO::South,MUTOO::Station2).count() +
      _hit_map->get(MUTOO::South,MUTOO::Station3).count();

    if( !_mui_hit_map ) return;
    // loop over panels and orientations and add to ntuple number of hits per arm,plane
    for (int iarm=0; iarm<2; iarm++) {
      for (int ipl=0; ipl<6; ipl++) {
        int temp_hits = 0;
        // for this arm and plane - loop over all panels and orientations
        for (int ii=0; ii<6; ii++) {
          for (int jj=0; jj<2; jj++) {
            temp_hits +=   _mui_hit_map->get(iarm,ipl,ii,jj).count();
          }
        }
        ntvar[3 + iarm*5 + ipl] = temp_hits;
      }
    }

    // fill an event number, the nhit south and nhit north
    _cu_dimu_evt->Fill(ntvar);

    ++evt_num;

  } // end requirement on "accepted" dimuon event

}

//________________________________________________________
void MuonEval::write_effic_ntuple() {

  static int ievent=0;
  int nstrips[6] = {0};
  int nhitsmax = 0;

  ++ievent;

  // Don't save the event (below we clear this if the event is "missed")
  //
  //set_event_veto();

  // Count the total number of active cathodes in each station
  if( !_hit_map ) return;
  TMutHitMap::iterator hit_iter = _hit_map->range();
  while(TMutHitMap::pointer hit_ptr = hit_iter.next()){
    nstrips[3*hit_ptr->get()->get_arm() + hit_ptr->get()->get_station()]++;
  }

  // Loop over monte-carlo tracks
  //  Loop over TMutEval
  //    fill ntuple
  //  ]
  // ]

  if( ! _mc_trk_map ) return;
  TMutMCTrkMap::iterator mc_trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){

    TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();

    // Skip tracks that jump octants
    //
    set<int> octant_set;
    while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){
      octant_set.insert(mc_hit_ptr->get()->get_octant());
    }
    if(octant_set.size() != 1) continue;

    // Ntuple variables
    //
    //"arm:charge:ptotus_true:found:charge_reco:"
    //"ptotus_reco:nhitsf:nhitst:track_id:nhits1:nhits2:nhits3:"
    //"nfound:nstrS1:nstrS2:nstrS3:nstrN1:nstrN2:nstrN3:event:"
    //"pxt:pyt:pzt:xvt:yvt:zvt:recosuc:ghost:found1:found2:found3"
    //":nhitsf1:nhitsf2:nhitsf3:nhits:road_depth:no_est:lo_mom"
    //:mc_pid:prnt_pid:px_reco:py_reco:pz_reco:ptot_reco:prnt_px:prnt_py:prnt_pz:prnt_ptot");


    float ntvar[100] = {0};
    ntvar[0] = mc_trk_ptr->get()->get_arm();
    ntvar[1] = mc_trk_ptr->get()->get_charge();
    ntvar[2] = mc_trk_ptr->get()->get_ptot_us_gap();
    ntvar[8] = (float)mc_trk_ptr->get()->get_track_id();
    ntvar[20] = mc_trk_ptr->get()->get_px_us_gap();
    ntvar[21] = mc_trk_ptr->get()->get_py_us_gap();
    ntvar[22] = mc_trk_ptr->get()->get_pz_us_gap();
    ntvar[23] = mc_trk_ptr->get()->get_x_orig();
    ntvar[24] = mc_trk_ptr->get()->get_y_orig();
    ntvar[25] = mc_trk_ptr->get()->get_z_orig();

    //get pid/parent_pid
    ntvar[42] = mc_trk_ptr->get()->get_pid();
    ntvar[43] = mc_trk_ptr->get()->get_parent_id();

    //just a palce holder right now ...
    //    ntvar[48] =  mc_trk_ptr->get()->get_px();
    //   ntvar[49] =  mc_trk_ptr->get()->get_py();
    //  ntvar[50] =  mc_trk_ptr->get()->get_pz();
    //  ntvar[51] =  mc_trk_ptr->get()->get_ptot();


    //get associated track
    ntvar[12] = (float)mc_trk_ptr->get()->get_associated<TMutTrk>().count();


    for (int i=0; i<6; i++) ntvar[13+i] = (float)nstrips[0+i];
    ntvar[19] = (float)ievent;

    // Get the associated TMutEval(s)
    //
    nhitsmax = 0;
    TMutEvalMap::key_iterator eval_iter = mc_trk_ptr->get()->get_associated<TMutEval>();
    while(TMutEvalMap::pointer eval_ptr = eval_iter.next()){

      ntvar[9] = eval_ptr->get()->get_trk_eval()->get_station_true_hits(0);
      ntvar[10] = eval_ptr->get()->get_trk_eval()->get_station_true_hits(1);
      ntvar[11] = eval_ptr->get()->get_trk_eval()->get_station_true_hits(2);
      ntvar[7] = eval_ptr->get()->get_trk_eval()->get_n_total_true_hits();

      ntvar[57] = eval_ptr->get()->get_trk_eval()->get_station_true_gaps(0);
      ntvar[58] = eval_ptr->get()->get_trk_eval()->get_station_true_gaps(1);
      ntvar[59] = eval_ptr->get()->get_trk_eval()->get_station_true_gaps(2);

      // store info on track that has the most number of correct hits:

      if (eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits() > nhitsmax)
      {

        nhitsmax = eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits();

        TMutTrkMap::key_iterator trk_iter = eval_ptr->get()->get_associated<TMutTrk>();
        if(!trk_iter.at_end()) {
          ntvar[3] = 1;   // at least one track "found"

          // Get the associated TMutEval
          ntvar[4] = trk_iter->get()->get_charge();
          ntvar[5] = trk_iter->get()->get_trk_par()->get_ptot();
          ntvar[56] = sqrt(
            MUTOO::SQUARE(trk_iter->get()->get_bp_par()->get_px_st1()) +
            MUTOO::SQUARE(trk_iter->get()->get_bp_par()->get_py_st1()) +
            MUTOO::SQUARE(trk_iter->get()->get_bp_par()->get_pz_st1()));

          //get reco momentum at IP
          ntvar[44] = trk_iter->get()->get_trk_par_vtx()->get_px();
          ntvar[45] = trk_iter->get()->get_trk_par_vtx()->get_py();
          ntvar[46] = trk_iter->get()->get_trk_par_vtx()->get_pz();
          ntvar[47] = trk_iter->get()->get_trk_par_vtx()->get_ptot();

          ntvar[52] = trk_iter->get()->get_trk_par_vtx()->get_chi_square();


          //
          ntvar[26] = trk_iter->get()->get_reco_success();
          ntvar[27] = trk_iter->get()->get_ghost();
          if (trk_iter->get()->has_stub(0))
            ntvar[28] = 1;
          if (trk_iter->get()->has_stub(1))
            ntvar[29] = 1;
          if (trk_iter->get()->has_stub(2))
            ntvar[30] = 1;
          TMutStubMap::key_iterator stub_iter = trk_iter->get()->get_associated<TMutStub>();
          int i = 0;
          while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
            ntvar[53+i] = stub_ptr->get()->get_theta();
            ntvar[60+i] = stub_ptr->get()->get_phi();
            i++;
          }

          ntvar[6] = eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits();
          ntvar[31] = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(0);
          ntvar[32] = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(1);
          ntvar[33] = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(2);
          ntvar[34] = (float)trk_iter->get()->get_n_coord();
          ntvar[35] = (float)get_max_road_depth( trk_iter.current() );
          ntvar[36] = (float)trk_iter->get()->get_no_estimate();
          ntvar[37] = (float)trk_iter->get()->get_low_mom();
          ntvar[38] = trk_iter->get()->get_bp_par()->get_chi_sq();
        }
      }
    } // loop over eval objects associated with MC track object

    // If this event was not "found" unset event veto
    if(ntvar[3]==0) unset_event_veto();
    if (_global) ntvar[39] = _global->getBbcZVertex();

    // there was supposed to have one working for run4 and one for run3
    ntvar[40] = MuonUtil::get_centrality( _top_node );
    ntvar[41] = 0;

    _effic->Fill(ntvar);
  }

}

//________________________________________________________
bool MuonEval::initialize_ntuples()
{
  MUTOO::PRINT( cout, "MuonEval::initialize_ntuples" );

  // Output file
  PHTFileServer::get().open( _filename );
  cout << "writing to " << _filename << endl;

  // Timing information
  if( get_flag( TIMING ) )
  {
    ostringstream o; o << "Time[" << _time.size() << "]/D";
    _timing = new TTree( "timing", "timing information for reconstruction modules" );
    _timing->Branch( "Time", &_time[0], o.str().c_str() );
    cout << "timing ntuple booked" << endl;
  }

  /*
    IN FOLLOWING NTUPLE DECLARATION, I TRIED TO PUT 10 VARS DECLARED/LINE TO MAKE COUNTING EASIER WHEN ONE WANTS
    TO KNOW WHICH VAR INDEX CORRESPONDS TO WHICH VAR NAME. PLEASE STICK TO THIS TO MAKE EVERYONE'S LIFE EASIER
  */

  // Reconstructed dimuons ntuple
  if( get_flag( RECO ) )
  {
    _dimu_reco = new TNtuple(
      "dimu_reco","dimu_reco",
      "armp:ptotp:pxp:pyp:pzp:pcnt:rptotp:rpxp:rpyp:rpzp:"
      "armn:ptotn:pxn:pyn:pzn:ncnt:rptotn:rpxn:rpyn:rpzn:"
      "ghost:vcnt:rmass:rsign:nvtx:nhit_north:nhit_south:error:evt_num:vtx_chisq:"
      "pos_chisq:pos_ndf:pos_ncoords:neg_chisq:neg_ndf:neg_ncoords:pos_road_depth:pos_road_nhit:pos_road_chisq:neg_road_depth:"
      "neg_road_nhit:neg_road_chisq:errorclone:errorbifur:pos_road_prox:pos_road_angm:pos_road_chim:neg_road_prox:neg_road_angm:neg_road_chim:"
      "pos_mcmuid_depth:neg_mcmuid_depth:pos_road_mult:neg_road_mult:best_pos_depth:best_pos_nhit:best_pos_chisq:best_pos_prox:best_pos_angm:best_pos_chim:"
      "vtx_ptot:vtx_pt:vtx_rx:vtx_ry:vtx_rz:vtx_ndf:vrx_prob");
    cout << "dimu_reco ntuple booked" << endl;

    _cu_dimu_evt = new TNtuple(
      "cu_dimu_evt","cu_dimu_evt",
      "evt:nhit_south:nhit_north:muis0:muis1:muis2:muis3:muis4:muin0:muin1:"
      "muin2:muin3:muin4");
    cout << "cu_dimu_evt ntuple booked" << endl;

  }


  //Reconstructed single tracks
  if( get_flag( SINGLE_RECO ) )
  {
    _single_reco = new TNtuple(
      "single_reco","single_reco",
      "arm:px:py:pz:ptot:charge:chisq:trkz:ghost:recosuc:"
      "r_gap0:road_depth:ntrks:ievent:run:n_reco_roads:muid_d_depth:muid_d_chisq:gap0_x:gap0_y:"
      "n_mc_trks:mc_pid:prnt_pid:prnt_ptot:mc_muid_depth:mc_px:mc_py:mc_pz:mc_vx:mc_vy:"
      "mc_vz:mc_charge:mc_ptotus:mc_trk_id:mc_pxus:mc_pyus:mc_pzus:reco_px:reco_py:reco_pz:"
      "reco_ptot:prnt_trkid:prnt_px:prnt_py:prnt_pz:fvtxmatch:fvtx_trid:fvtx_mutr_chi:distance:"
      "dangle:nhitsfvtx:nhitsmut:nfvtxtrks:distance_nomatch:dangle_nomatch:nhitsfvtxf:nfvtxtrks_tot:"
      "fvtx_px:fvtx_py:fvtx_pz:fvtx_px_mc:fvtx_py_mc:fvtx_pz_mc:DG0:DDG0:fvtxhits_match:vtxhits_match:"
      "fvtx_chi");

    cout << "single_reco ntuple booked" << endl;
  }

  //Reconstructed Clusters
  if( get_flag( CLUS_RECO ) )
  {
    _clus_reco = new TNtuple(
      "clus_reco","clus_reco",
      "event:arm:station:octant:half:"
      "gap:cath:cluswid:chi:nfit:"
      "nhits:badmiddlestrip:badedgestrip:leftedgedet:rightedgedet:"
      "saturation:atten:badcalib:nocalib:"
      "strip0:strip1:strip2:strip3:strip4:strip5:strip6:strip7:"
      "q1:q2:q3:q4:q5:q6:q7:q8:"
      "qe1:qe2:qe3:qe4:qe5:qe6:qe7:qe8:"
      "xfit:xfit2:xfit3:xfit4:"
      "res:we2:we3:we4:"
      "xtrue:xtrue2:xtrue3:xtrue4:"
      "mc1:mc2:mc3:qfit1:qfit2:qfit3:"
      "xfitg:xfit2g:xfit3g:xfit4g:qpeak:"
      "xtrueg:xtrue2g:xtrue3g:xtrue4g");
    cout << "clus_reco ntuple booked" << endl;
  }

  //Reconstructed Stubs
  if( get_flag( STUB_RECO ) )
  {
    _stub_reco = new TNtuple(
      "stub_reco","stub_reco",
      "arm:station:octant:chi:theta:phi:event:ncoords:ngaps:"
      "x:y:z:dxdz:dydz:dw1:dw2:dw3:dw4:dw5:dw6");
    cout << "stub_reco ntuple booked" << endl;
  }

  // Stub Finder
  if( get_flag( STUB_EVAL ) )
  {
    TMutStubFinder::set_do_evaluation( true );
    TMutStubFinder::set_evaluation_filename( _filename );
    cout << "stub finder evaluation enabled" << endl;
  }

  // Reconstruction efficiency
  if( get_flag( EFFIC ) )
  {
    _effic = new TNtuple(
      "Effic","Effic",
      "arm:charge:ptotus_true:found:charge_reco:ptotus_reco:nhitsf:nhitst:track_id:nhits1:"
      "nhits2:nhits3:nfound:nstrS1:nstrS2:nstrS3:nstrN1:nstrN2:nstrN3:event:"
      "pxt:pyt:pzt:xvt:yvt:zvt:recosuc:ghost:found1:found2:"
      "found3:nhitsf1:nhitsf2:nhitsf3:nhits:road_depth:no_est:lo_mom:chi_bp:zvertex:"
      "centc:centp:mc_pid:prnt_pid:px_reco:py_reco:pz_reco:ptot_reco:prnt_px:prnt_py:"
      "prnt_pz:prnt_ptot:chi2:theta1:theta2:theta3:ptotus_recobp:"
      "ngaps1:ngaps2:ngaps3:phi1:phi2:phi3");
    cout << "Effic ntuple booked" << endl;
  }

  if( get_flag( BENT_PLANE ) )
  {
    _bend_plane = new TNtuple(
      "bend_plane","bend_plane",
      "arm:pxus_t:pyus_t:pzus_t:ptotus_t:chrg_t:xst1_t:yst1_t:xst2_t:yst2_t:"
      "pxus_r:pyus_r:pzus_r:ptotus_r:chrg_t:xst1_r:yst1_r:xst2_r:yst2_r");
    cout << "bent_plane ntuple booked" << endl;
  }

  // muid background estimate
  if( get_flag( BACKGROUND ) )
  {
    _background = new TNtuple(
      "bg","bg",
      "arm:ntrk:nroad:ngold:nshallow:nsheep:ndeep:nroad_nt:ngold_nt:nshallow_nt:"
      "nsheep_nt:ndeep_nt");
    cout << "bg ntuple booked" << endl;
  }

  // Muioo
  if( get_flag( MUIOO ) )
  {

    _muioo = new TNtuple(
      "muioo","muioo",
      "arm:px:py:pz:y:pt:true_depth:p0:p1:p2:"
      "p3:p4:p5:p6:p7:p8:p9:pan0:pan1:pan2:"
      "pan3:pan4:pan5:pan6:pan7:pan8:pan9:ch0:ch1:ch2:"
      "ch3:ch4:ch5:ch6:ch7:ch8:ch9:xtrk:ytrk:run:"
      "found:true_twopack");
    cout << "muioo ntuple booked" << endl;
  }

  // CU single muon tests
  if( get_flag( CU_SINGLE_MU ) )
  {
    _cu_mc_trk = new TNtuple(
      "single_muon_mc","single_muon_mc",
      "event_num:n_mc_tracks:mc_pid:mc_px:mc_py:mc_pz:mc_depth:"
      "mc_hit_bit:arm:n_reco_trks:n_reco_roads:gap0_x:gap0_y"
      );
    cout << "single_muon_mc ntuple booked" << endl;


    _cu_reco_trk = new TNtuple(
      "cu_reco_trk","cu_reco_trk",
      "event_num:mc_depth:mc_hit_bit:deepest_depth:deepest_chisq:deep_hit_bit:deep_mc_hit_bit:sheep_hit_bit:sheep_mc_hit_bit:"
      "gap4_deltax:gap4_deltay:gap0_x:gap0_y"
      );
    cout << "cu_reco_trk ntuple booked" << endl;
  }

  if( get_flag( CU_EFFIC ) )
  {
    _cu_effic_nt = new TNtuple("cu_effic", "cu_effic",
      "event_num:n_reco_trks:reco_flag:centrality:mc_pid:"
      "mc_parent_pid:mc_charge:mc_track_id:mc_true_hits:mc_true_hits_st1:"
      "mc_true_hits_st2:mc_true_hits_st3:mc_ptot:mc_px:mc_py:"
      "mc_pz:mc_depth:reco_true_hits:reco_true_hits_st1:reco_true_hits_st2:"
      "reco_true_hits_st3:reco_true_rank:reco_hits_st1:reco_hits_st2:reco_hits_st3:"
      "reco_clus_multi_coord:reco_charge:reco_ptot:reco_px:"
      "reco_py:reco_pz:reco_chi2:DG0_old:DDG0:"
      "idquad:idchi2:idhits:reco_depth:mc_clus_multi_coord:"
      "mutr_hits:reco_clus_multi_coord_st1:reco_clus_multi_coord_st2:reco_clus_multi_coord_st3:"
      "x_st3:y_st3:z_st3:px_st3:py_st3:pz_st3:dxdz_st3:dydz_st3:"
      "MUIDx_old:MUIDy_old:MUIDz_old:MUIDx_new:MUIDy_new:MUIDz_new:MUIDdxdz_old:"
      "MUIDdydz_old:DG0x_old:DG0y_old:x_kf:y_kf:z_kf:px_kf:py_kf:pz_kf:"
      "dxdz_kf:dydz_kf:DG0x_new:DG0y_new:DG0_new");
    cout << "cu_effic ntuple booked" << endl;
  }

  MUTOO::PRINT( cout, "**" );

  return true;
}

//________________________________________________________
void MuonEval::store_muid_stat( PHCompositeNode* top_node )
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
void MuonEval::write_bend_plane_ntuple()
{

  // At this point in the game TMutMCTrk - TMutEval - TMutTrk are all associated.
  // That is each TMutMCTrk is associated with a TMutEval object and if the
  // reconstruction succeeded there will also be an associated TMutTrk

  // Loop over TMutEval objects
  //   Write truth data
  //   If there is an associated TMutTrk
  //     Write the bend plane data
  //   ]
  // ]

  if( !_eval_map ) return;
  TMutEvalMap::const_iterator eval_iter = _eval_map->range();
  while(TMutEvalMap::const_pointer eval_ptr = eval_iter.next()){

    float ntvar[19] = {0};

    // Truth data from eval object
    //
    ntvar[0] = eval_ptr->get()->get_arm();
    ntvar[1] = eval_ptr->get()->get_trk_eval()->get_px_true_us();
    ntvar[2] = eval_ptr->get()->get_trk_eval()->get_py_true_us();
    ntvar[3] = eval_ptr->get()->get_trk_eval()->get_pz_true_us();
    ntvar[4] = eval_ptr->get()->get_trk_eval()->get_ptot_true_us();
    ntvar[5] = eval_ptr->get()->get_trk_eval()->get_charge_true();

    // Get the positions at each of the stations from associated TMutMCHit
    //
    TMutMCTrkMap::const_key_iterator mctrk_iter = eval_ptr->get()->get_associated<TMutMCTrk>();
    if(!mctrk_iter.at_end()){
      TMutMCHitMap::const_key_iterator mchit_iter = mctrk_iter->get()->get_associated<TMutMCHit>();
      while(TMutMCHitMap::const_pointer mchit_ptr = mchit_iter.next()){
  // true x and y at gap 0 station 1
  //
  if(mchit_ptr->get()->get_station() == MUTOO::Station1 &&  mchit_ptr->get()->get_gap() == 0) {
    ntvar[6] = mchit_ptr->get()->get_x();
    ntvar[7] = mchit_ptr->get()->get_y();
  }
  // true x and y at gap 0 station 2
  //
  if(mchit_ptr->get()->get_station() == MUTOO::Station2 &&  mchit_ptr->get()->get_gap() == 0) {
    ntvar[8] = mchit_ptr->get()->get_x();
    ntvar[9] = mchit_ptr->get()->get_y();
  }
      }
    }

    // If associated TMutTrk object exists then get BP reco data
    //
    TMutTrkMap::const_key_iterator trk_iter = eval_ptr->get()->get_associated<TMutTrk>();

    // Write the station 1 bend plane reco data
    //
    if(!trk_iter.at_end()){
      ntvar[10] = trk_iter->get()->get_bp_par()->get_px_st1();
      ntvar[11] = trk_iter->get()->get_bp_par()->get_py_st1();
      ntvar[12] = trk_iter->get()->get_bp_par()->get_pz_st1();
      ntvar[13] = sqrt(MUTOO::SQUARE(ntvar[6]) +
          MUTOO::SQUARE(ntvar[7]) +
          MUTOO::SQUARE(ntvar[8]));
      ntvar[14] = trk_iter->get()->get_bp_par()->get_charge();

      // Put the bend plane projections points hereå
      //
      ntvar[15] = 0;
      ntvar[16] = 0;
      ntvar[17] = 0;
      ntvar[18] = 0;
    }
    _bend_plane->Fill(ntvar);
  }

  if( verbosity >= 1 ) cout << "MuonEval::write_bend_plane_ntuple.\n";

}

//________________________________________________________
void MuonEval::write_bg_ntuple() {

  if( !_trk_map ) return;
  for(int arm=0; arm<2; ++arm) {
    // Fill fields for roads with associated track
    //
    UShort_t nroad=0, ngold=0, nshallow=0,nsheep=0,ndeep=0;
    TMutTrkMap::const_iterator trk_iter = _trk_map->get(arm);
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
      TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
      while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next()){
  ++nroad;
  if(road_ptr->get()->get_golden()) ++ngold;
  if(road_ptr->get()->get_depth() == 2) ++nshallow;
  if(road_ptr->get()->get_depth() == 3) ++nsheep;
  if(road_ptr->get()->get_depth() == 4) ++ndeep;
      }
    }
    float nt_var[12] = {0};
    nt_var[0] = arm;
    nt_var[1] = trk_iter.count();
    nt_var[2] = nroad;
    nt_var[3] = ngold;
    nt_var[4] = nshallow;
    nt_var[5] = nsheep;
    nt_var[6] = ndeep;

    // Fill fields for roads with no associated track
    //
    UShort_t nroad_nt=0, ngold_nt=0, nshallow_nt=0,nsheep_nt=0,ndeep_nt=0;
    if( !_mui_road_map ) return;
    TMuiRoadMapO::const_iterator road_iter = _mui_road_map->get(arm);
    while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next()){
      if(road_ptr->get()->get_associated<TMutTrk>().count()) continue;
      nt_var[7] = nroad_nt;
      nt_var[8] = ngold_nt;
      nt_var[9] = nshallow_nt;
      nt_var[10] = nsheep_nt;
      nt_var[11] = ndeep_nt;
    }
    _background->Fill(nt_var);
  }

  if( verbosity >= 1 ) cout << "MuonEval::write_bg_ntuple.\n";

}

//________________________________________________________
void MuonEval::write_muioo_ntuple() {

  // This ntuple is used to calculate the panel and hv chain efficiency
  // of the MUID detector based on a particular HV mask configuration.
  // There is a field for each plane (0-9) in the MUID.  The plane state
  // is either 1 (hit) 0 (masked) or (-1 no true hit in this plane). For
  // each plane we also store the panel(0-5) and hv chain group (0-2). The
  // combination of plane, panel and hv chain group index are enough to
  // uniquely identify which HV chain is associated with given hit.  In
  // addition we store some kinematic data associated with the monte-carlo
  // track.

  // Loop over eval object
  //  Access road eval object
  //  Loop over planes
  //    Accumulate track kinematics and per plane hit data
  //  ]
  // ]

  if( !_mui_eval_map ) return;
  TMuiEvalMap::const_iterator eval_iter = _mui_eval_map->range();
  while(TMuiEvalMap::const_pointer eval_ptr = eval_iter.next()){
    float nt_var[100] = {0};
    const TMuiRoadEval* road_eval = eval_ptr->get()->get_road_eval();
    nt_var[0] = eval_ptr->get()->get_arm();
    nt_var[1] = road_eval->get_px_true_vx();
    nt_var[2] = road_eval->get_py_true_vx();
    nt_var[3] = road_eval->get_pz_true_vx();

    nt_var[4] = rapidity(road_eval->get_px_true_vx(),
       road_eval->get_py_true_vx(),
       road_eval->get_pz_true_vx(),
       MUTOO::MASS_MUON);

    nt_var[5] = pt(road_eval->get_px_true_vx(),
       road_eval->get_py_true_vx());

    // depth 0-4 (4 deep) (2 shallow)
    //
    nt_var[6]  = ceil(road_eval->get_n_true_hits()/2.0) - 1;

    // each plane is either hit - masked or has no true hit
    //
    for(int i=0; i<10;++i){
      if(road_eval->get_is_plane_true_hit(i)){
  nt_var[7+i] = 1;
      } else if(road_eval->get_is_plane_masked_hit(i)) {
  nt_var[7+i] = 0;
      }	else {
  nt_var[7+i] = -1;
      }
    }

    boost::array<float,10> hit_panel;
    boost::array<float,10> hit_chain;
    hit_panel.assign(-1.0);
    hit_chain.assign(-1.0);

    // Need the two pack list, not currently stored in eval object so we go to the
    // associated TMutMCTrk and trace the association hierarchy until we find the
    // panel, hv channel data we need.
    //
    TMutMCTrkMap::const_key_iterator mc_trk_iter = eval_ptr->get()->get_associated<TMutMCTrk>();
    if(!mc_trk_iter.at_end()) {
      TMuiMCHitMapO::const_key_iterator mc_hit_iter = mc_trk_iter->get()->get_associated<TMuiMCHitO>();
      while(TMuiMCHitMapO::const_pointer mc_hit_ptr = mc_hit_iter.next()){

  UShort_t arm = mc_hit_ptr->get()->get_arm();
  UShort_t plane = mc_hit_ptr->get()->get_plane();
  // Write the true spacepoint at the first gap
  //
  if(plane==0){
    nt_var[37] = mc_hit_ptr->get()->get_x();
    nt_var[38] = mc_hit_ptr->get()->get_y();
  }

  // Loop over TMuiMCHitO's twopack list and fetch other locators
  //
  const TMuiMCHitO::twopack_list* twopack_list = mc_hit_ptr->get()->get_twopack_list();
  TMuiMCHitO::twopack_iterator twopack_iter = twopack_list->begin();
  for(;twopack_iter!=twopack_list->end(); ++twopack_iter){
    EOrient_t orient = twopack_iter->get_orient()==0 ? kHORIZ : kVERT;
    UShort_t panel = twopack_iter->get_panel();
    UShort_t index = twopack_iter->get_twopack_index();
    UShort_t plane_index = plane*2 + twopack_iter->get_orient();
    TMuiChannelId channel_id(arm,plane,panel,orient,index);
    hit_panel[plane_index] = panel;
    hit_chain[plane_index] = channel_id.get_HV_chain_group();
  }
      }
    }

    for(size_t i=0; i<hit_panel.size(); ++i){
      nt_var[17+i] = hit_panel[i];
    }
    for(size_t i=0; i<hit_chain.size(); ++i){
      nt_var[27+i] = hit_chain[i];
    }
    nt_var[39] = recoConsts::instance()->get_IntFlag("RUNNUMBER", 0);
    nt_var[40] = eval_ptr->get()->get_associated<TMuiRoadO>().count();
    //    nt_var[41] = mc_trk_iter->get()->get_n_twopack();
    _muioo->Fill(nt_var);
  }

  if( verbosity >= 1 ) cout << "MuonEval::write_muioo_ntuple.\n";

}

//________________________________________________________
void MuonEval::print_muid_stat( ostream &out )
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
double MuonEval::rapidity(double px, double py, double pz, double mass)
{
  double ptot = sqrt(px*px + py*py + pz*pz);
  double E = sqrt(ptot*ptot + mass*mass);
  return 0.5*log((E+pz)/(E-pz));
}

//________________________________________________________
double MuonEval::pt(double px, double py)
{
  return sqrt(px*px + py*py);
}

//________________________________________________________
void MuonEval::write_cu_single_muon_ntuple()
{

  static int evt_num = 1;

  Short_t n_mc_tracks = 0;
  Short_t mc_pid = 0;
  Short_t mc_depth = 0;

  Float_t mc_px = 0;
  Float_t mc_py = 0;
  Float_t mc_pz = 0;

  UShort_t mc_hit_bit = 0; //bits for whether a plane was hit

  Short_t arm = -1;
  Short_t n_reco_trks = 0;
  Short_t n_reco_roads = 0;

  Float_t mc_nt[100] = {0};
  Float_t track_nt[100] = {0};

  // Find all Monte Carlo tracks
  if( !_mc_trk_map ) return;
  TMutMCTrkMap::const_iterator mc_trk_iter = _mc_trk_map->range();

  // How many are there in this event? (hopefully 1 or 0 in our oscar single muon input case)
  n_mc_tracks = mc_trk_iter.count();

  // Loop over MCTracks
  mc_trk_iter.reset();
  while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {

    // Get:
    //  1. particle ID number
    //  2. depth of track in MUID
    //  3. momentum-components
    //
    mc_pid = mc_trk_ptr->get()->get_pid();
    mc_depth = get_mc_trk_depth( mc_trk_ptr );
    mc_px = mc_trk_ptr->get()->get_px_orig();
    mc_py = mc_trk_ptr->get()->get_py_orig();
    mc_pz = mc_trk_ptr->get()->get_pz_orig();
    arm = mc_trk_ptr->get()->get_arm();

    // Loop over MCHits associated with the MCTrack
    //  -> output which gaps were hit in the MUID
    //
    TMuiMCHitMapO::const_key_iterator mc_muihit_iter = mc_trk_ptr->get()->get_associated<TMuiMCHitO>();
    while( TMuiMCHitMapO::const_pointer mc_hit_ptr = mc_muihit_iter.next() ) {
      // mc_hit_bit has bits 0-4 corresponding to gaps 0-4
      mc_hit_bit |= 1<<mc_hit_ptr->get()->get_plane();
    }
    // Loop over reconstructed tracks...
    //
    TMutTrkMap::const_key_iterator reco_trk_iter = mc_trk_ptr->get()->get_associated<TMutTrk>();
    n_reco_trks = reco_trk_iter.count();
    while( TMutTrkMap::const_pointer reco_trk_ptr = reco_trk_iter.next()) {

      Short_t deepest_depth = -1;
      Float_t deepest_chisq = 1e6;
      UShort_t deep_hit_bit = 0;
      UShort_t deep_mc_hit_bit = 0;
      UShort_t sheep_hit_bit = 0;
      UShort_t sheep_mc_hit_bit = 0;
      Float_t gap4_deltax = 1e6;
      Float_t gap4_deltay = 1e6;
      Float_t gap0_x = 1e6;
      Float_t gap0_y = 1e6;

      // Loop over associated reconstructed roads which are associated with reconstructed tracks
      //
      TMuiRoadMapO::const_key_iterator reco_road_iter = reco_trk_ptr->get()->get_associated<TMuiRoadO>();
      n_reco_roads = reco_road_iter.count();  // hopefully <= 3
      if ( verbosity >= MUTOO::ALOT )
        {
          if( n_reco_roads > 3 )
            cout << "More than three roads associated to this track!!!" << endl;
        }

      while( TMuiRoadMapO::const_pointer reco_road_ptr = reco_road_iter.next() ) {

  Short_t current_depth = reco_road_ptr->get()->get_depth();
  if( current_depth > deepest_depth ) {
    deepest_depth = reco_road_ptr->get()->get_depth();
    deepest_chisq = reco_road_ptr->get()->get_const_fitpar()->get_chi_square();
  }
  // could also store information from the best road...in terms of matching

  gap0_x = reco_road_ptr->get()->get_gap0_point().getX();
  gap0_y = reco_road_ptr->get()->get_gap0_point().getY();

  // only if the reconstructed road depth = 4 continue
  if( current_depth == 4 ) {

    // loop over all associated clusters
    TMuiClusterMapO::const_key_iterator clus_iter = reco_road_ptr->get()->get_associated<TMuiClusterO>();
    while(TMuiClusterMapO::const_pointer clus_ptr = clus_iter.next() ) {

      // loop over all associated hits - we have to do this since it is the hits which have monte carlo associations !
      TMuiHitMapO::const_key_iterator hit_iter = clus_ptr->get()->get_associated<TMuiHitO>();
      while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next() ) {

        // store which MUID planes were hit (0-9) = (2*plane + orientation)
        deep_hit_bit |= 1 << (2*hit_ptr->get()->get_plane() + clus_ptr->get()->get_orientation());

        // store which MUID planes were hit with Monte Carlo contributing
        if( hit_ptr->get()->get_associated<TMuiMCHitO>().count() )
    deep_mc_hit_bit |= 1 << (2*hit_ptr->get()->get_plane() + clus_ptr->get()->get_orientation());

        // determine the distance from the road fit to the gap 4 hits (either horizontal or vertical or both)
        if (hit_ptr->get()->get_plane() == 4) {
    float cluster_z = clus_ptr->get()->get_centroidpos().getZ();
    if( clus_ptr->get()->get_orientation() == 1 ){  // if vertical tube, then it has delta-X
      float cluster_x = clus_ptr->get()->get_centroidpos().getX();
      const PHPoint road_point = TMutTrackUtil::linear_track_model(reco_road_ptr->get()->get_const_fitpar(), cluster_z);
      gap4_deltax = fabs( cluster_x - road_point.getX() );
    } else {     // else horizontal tube, then it has delta-Y
      float cluster_y = clus_ptr->get()->get_centroidpos().getY();
      const PHPoint road_point = TMutTrackUtil::linear_track_model(reco_road_ptr->get()->get_const_fitpar(), cluster_z);
      gap4_deltay = fabs( cluster_y - road_point.getY() );
    }
        }

      } // end loop over mui-hits
    } // end loop over mui-clusters
  } // if current_depth == 4

  // now do the case for reconstructed road depth = 3
  if( current_depth == 3 ) {
    TMuiClusterMapO::const_key_iterator clus_iter = reco_road_ptr->get()->get_associated<TMuiClusterO>();
    while(TMuiClusterMapO::const_pointer clus_ptr = clus_iter.next() ) {
      TMuiHitMapO::const_key_iterator hit_iter = clus_ptr->get()->get_associated<TMuiHitO>();
      while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next() ) {

        // store which MUID planes were hit (0-9)
        sheep_hit_bit |= 1 << (2*hit_ptr->get()->get_plane() + clus_ptr->get()->get_orientation());

        // we want to know which hits are associated with the MonteCarlo Muon track...
        // if there is one association, then hit is from Monte Carlo
        if( hit_ptr->get()->get_associated<TMuiMCHitO>().count() )
    		sheep_mc_hit_bit |= 1 << (2*hit_ptr->get()->get_plane() + clus_ptr->get()->get_orientation());

      }

    }
  }

      } // end loop over roads

      // fill the ntuple...
      int j = 0;
      track_nt[j++] = evt_num;
      track_nt[j++] = mc_depth;
      track_nt[j++] = mc_hit_bit;
      track_nt[j++] = deepest_depth;
      track_nt[j++] = deepest_chisq;
      track_nt[j++] = deep_hit_bit;
      track_nt[j++] = deep_mc_hit_bit;
      track_nt[j++] = sheep_hit_bit;
      track_nt[j++] = sheep_mc_hit_bit;
      track_nt[j++] = gap4_deltax;
      track_nt[j++] = gap4_deltay;
      track_nt[j++] = gap0_x;
      track_nt[j++] = gap0_y;

      _cu_reco_trk->Fill(track_nt);

    } // end loop over tracks

    // enter just some monte carlo data into ntuple...
    int i = 0;
    mc_nt[i++] = evt_num;
    mc_nt[i++] = n_mc_tracks;
    mc_nt[i++] = mc_pid;
    mc_nt[i++] = mc_px;
    mc_nt[i++] = mc_py;
    mc_nt[i++] = mc_pz;
    mc_nt[i++] = mc_depth;
    mc_nt[i++] = mc_hit_bit;
    mc_nt[i++] = arm;
    mc_nt[i++] = n_reco_trks;
    mc_nt[i++] = n_reco_roads;

    _cu_mc_trk->Fill(mc_nt);
  }

  ++evt_num;

  if( verbosity >= 1 ) cout << "MuonEval::write_cu_single_muon_ntuple.\n";

}

//________________________________________________________
void MuonEval::write_cu_effic_ntuple()
{
  // loop over mc tracks
  //   get mc track info
  //
  //   loop over reco tracks
  //     make ordered list of tracks by how many reco true hits from this mc track
  //
  //   loop over associated reco tracks again
  //     fill mc + reco info
  //
  //   if mc track wasn't reconstructed:
  //     fill mc info
  //
  // loop over all reco tracks
  //   if track doesn't have associated MC track
  //     fill info for track

  typedef pair<TMutTrkMap::const_pointer, Int_t> trk_pair;
  list< trk_pair > ordered_trk_list;
  list< trk_pair >::iterator list_iter;

  static Int_t event_number = 1;
  Int_t n_reco_trks;
  Int_t reco_flag;           // 0=MC, 1=RECO, 2=MC+RECO
  Int_t mc_pid;
  Int_t mc_parent_pid;
  Int_t mc_charge;
  Int_t mc_track_id;
  Int_t mc_true_hits = 0;
  Int_t mc_true_hits_st1 = 0;
  Int_t mc_true_hits_st2 = 0;
  Int_t mc_true_hits_st3 = 0;
  Float_t mc_ptot = 0;
  Float_t mc_px = 0;
  Float_t mc_py = 0;
  Float_t mc_pz = 0;
  Float_t mc_depth;
  Int_t reco_true_hits = 0;
  Int_t reco_true_rank = 0;
  Int_t reco_true_hits_st1 = 0;
  Int_t reco_true_hits_st2 = 0;
  Int_t reco_true_hits_st3 = 0;
  Int_t reco_hits_st1;
  Int_t reco_hits_st2;
  Int_t reco_hits_st3;
  Int_t reco_clus_multi_coord= 0;
  Int_t reco_clus_multi_coord_st1 = 0;
  Int_t reco_clus_multi_coord_st2 = 0;
  Int_t reco_clus_multi_coord_st3 = 0;
  Int_t mc_clus_multi_coord = 0;
  Int_t reco_charge;
  Float_t reco_ptot;
  Float_t reco_px;
  Float_t reco_py;
  Float_t reco_pz;
  Float_t reco_chi2;
  Float_t DG0;
  Float_t DDG0;
  Int_t idquad;
  Float_t idchi2;
  Int_t idhits;
  Int_t reco_depth;
  Float_t centrality;
  Float_t mutr_hits;

  // variables needed for calculations but not in ntuple
  Float_t x3;
  Float_t y3;
  Float_t z3;
  Float_t px3;
  Float_t py3;
  Float_t pz3;
  Float_t dxdz3;
  Float_t dydz3;
  Float_t x0;
  Float_t y0;
  Float_t z0;
  Float_t x0_new;
  Float_t y0_new;
  Float_t z0_new;
  Float_t dxdz0;
  Float_t dydz0;
  PHPoint gap0;
  PHPoint gap0_temp;
  Float_t x0_temp;
  Float_t y0_temp;
  Float_t z0_temp;
  Float_t dxdz0_temp;
  Float_t dydz0_temp;
  Float_t DG0_temp;
  Float_t DG0x;
  Float_t DG0y;
  Float_t x_kal;
  Float_t y_kal;
  Float_t z_kal;
  Float_t px_kal = 0;
  Float_t py_kal = 0;
  Float_t pz_kal = 0;
  Float_t dxdz_kal = 0;
  Float_t dydz_kal = 0;
  Float_t DG0x_kal = 0;
  Float_t DG0y_kal = 0;
  Float_t DG0_kal = 0;
  Float_t DG0_kal_temp = 0;
  int j = 0;
  Float_t nt_vars[100] = {0};

  centrality = 0.0; // need to pull this out later

  if( !_hit_map ) return;
  mutr_hits = _hit_map->size();

  if( !_trk_map ) return;
  if( !_mc_trk_map ) return;
  TMutMCTrkMap::const_iterator mc_trk_iter = _mc_trk_map->range();
  while( TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next() )
  {

    TMutTrkMap::const_key_iterator trk_iter = mc_trk_ptr->get()->get_associated<TMutTrk>();

    // MC track info:
    n_reco_trks = trk_iter.count();
    reco_flag = (n_reco_trks>0) ? 2 : 0;
    mc_pid = mc_trk_ptr->get()->get_pid();
    mc_parent_pid = mc_trk_ptr->get()->get_parent_id();
    mc_charge = (Int_t) mc_trk_ptr->get()->get_charge();
    mc_depth = get_mc_trk_depth( mc_trk_ptr );
    mc_track_id = mc_trk_ptr->get()->get_track_id();

    // loop over reconstructed tracks...
    ordered_trk_list.clear();
    while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
    {

      // check if track is acceptable
      if( trk_ptr->get()->get_ghost() || !trk_ptr->get()->get_reco_success() ) continue;

      // count the reconstructed MC hits in the track
      TMutEvalMap::const_key_iterator eval_iter = trk_ptr->get()->get_associated<TMutEval>();
      if( TMutEvalMap::const_pointer eval_ptr = eval_iter.next() )
      { reco_true_hits = eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits(); }

      trk_pair reco_trk_pair(trk_ptr, reco_true_hits);

      // insert (track*, reco_true_hits) into list at correct position
      if( !ordered_trk_list.size() )
      { ordered_trk_list.push_front( reco_trk_pair ); }
      else
      {
        for( list_iter = ordered_trk_list.begin(); list_iter != ordered_trk_list.end(); ++list_iter )
        {
          if( reco_true_hits > list_iter->second )
          {
            ordered_trk_list.insert( list_iter, reco_trk_pair );
            break;
          }
        }
      }
    } // end TMutTrk loop

    if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end track loop" << endl;

    // Here's where we actually fill the reco track data...
    Int_t rank = 1;
    for( list_iter = ordered_trk_list.begin(); list_iter != ordered_trk_list.end(); ++list_iter )
    {
      TMutTrkMap::const_pointer trk_ptr = list_iter->first;
      reco_true_hits = list_iter->second;
      reco_true_rank = rank++;

      TMutEvalMap::const_key_iterator eval_iter = trk_ptr->get()->get_associated<TMutEval>();
      while( TMutEvalMap::const_pointer eval_ptr = eval_iter.next() )
      {
        reco_true_hits_st1 = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(0);
        reco_true_hits_st2 = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(1);
        reco_true_hits_st3 = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(2);
        mc_true_hits = eval_ptr->get()->get_trk_eval()->get_n_total_true_hits();
        mc_true_hits_st1 = eval_ptr->get()->get_trk_eval()->get_station_true_hits(0);
        mc_true_hits_st2 = eval_ptr->get()->get_trk_eval()->get_station_true_hits(1);
        mc_true_hits_st3 = eval_ptr->get()->get_trk_eval()->get_station_true_hits(2);
        mc_ptot = eval_ptr->get()->get_trk_eval()->get_ptot_true_vx();
        mc_px = eval_ptr->get()->get_trk_eval()->get_px_true_vx();
        mc_py = eval_ptr->get()->get_trk_eval()->get_py_true_vx();
        mc_pz = eval_ptr->get()->get_trk_eval()->get_pz_true_vx();
      }

      if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end eval loop" << endl;

      reco_hits_st1 = 0;
      reco_hits_st2 = 0;
      reco_hits_st3 = 0;
      reco_clus_multi_coord = 0;
      reco_clus_multi_coord_st1 = 0;
      reco_clus_multi_coord_st2 = 0;
      reco_clus_multi_coord_st3 = 0;

      TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
      while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
        TMutClusMap::const_key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
        while( TMutClusMap::const_pointer clus_ptr = clus_iter.next() )
        {
          if( clus_ptr->get()->get_station() == 0 )         ++reco_hits_st1;
          else if( clus_ptr->get()->get_station() == 1 )    ++reco_hits_st2;
          else                                              ++reco_hits_st3;

          if( (clus_ptr->get()->get_associated<TMutCoord>()).count() > 1 )
          {
            ++reco_clus_multi_coord;
            if (clus_ptr->get()->get_station() == 0) ++reco_clus_multi_coord_st1;
            if (clus_ptr->get()->get_station() == 1) ++reco_clus_multi_coord_st2;
            if (clus_ptr->get()->get_station() == 2) ++reco_clus_multi_coord_st3;
          }

        }

        if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end cluster loop" << endl;

      }

      if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end coord loop" << endl;

      reco_charge = (Int_t) trk_ptr->get()->get_charge();
      reco_ptot = trk_ptr->get()->get_trk_par_vtx()->get_ptot();
      reco_px = trk_ptr->get()->get_trk_par_vtx()->get_px();
      reco_py = trk_ptr->get()->get_trk_par_vtx()->get_py();
      reco_pz = trk_ptr->get()->get_trk_par_vtx()->get_pz();
      reco_chi2 = trk_ptr->get()->get_w_chi_square_pdf();

      DG0 = 1000;
      x3 = 0;
      y3 = 0;
      z3 = 0;
      dxdz3 = 0;
      dydz3 = 0;
      x0 = 0;
      y0 = 0;
      z0 = 0;
      dxdz0 = 0;
      dydz0 = 0;
      x0_temp = 0;
      y0_temp = 0;
      z0_temp = 0;
      dxdz0_temp = 0;
      dydz0_temp = 0;
      DG0_temp = 100;

      // current algorithm (pro.55) takes the road with smallest DG0:
      TMuiRoadMapO::pointer road_ptr(0);

      TMuiRoadMapO::key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();

      while( TMuiRoadMapO::pointer temp_road_ptr = road_iter.next() )
      {
        x3 = trk_ptr->get()->get_trk_par_station(2)->get_x();
        y3 = trk_ptr->get()->get_trk_par_station(2)->get_y();
        z3 = trk_ptr->get()->get_trk_par_station(2)->get_z();
        dxdz3 = trk_ptr->get()->get_trk_par_station(2)->get_px()/trk_ptr->get()->get_trk_par_station(2)->get_pz();
        dydz3 = trk_ptr->get()->get_trk_par_station(2)->get_py()/trk_ptr->get()->get_trk_par_station(2)->get_pz();
        gap0_temp = temp_road_ptr->get()->get_gap0_point();
        x0_temp = gap0_temp.getX();
        y0_temp = gap0_temp.getY();
        z0_temp = gap0_temp.getZ();
        dxdz0_temp = temp_road_ptr->get()->get_const_fitpar()->get_dxdz();
        dydz0_temp = temp_road_ptr->get()->get_const_fitpar()->get_dydz();
        DG0_temp = sqrt( pow(x0_temp-x3-dxdz3*(z0_temp-z3),2) + pow(y0_temp-y3-dydz3*(z0_temp-z3),2) );

        if( DG0_temp < DG0 )
        {
          road_ptr = temp_road_ptr;
          gap0 = gap0_temp;
          x0 = x0_temp;
          y0 = y0_temp;
          z0 = z0_temp;
          dxdz0 = dxdz0_temp;
          dydz0 = dydz0_temp;
          DG0 = DG0_temp;
        }
      }

      if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end road loop" << endl;

      if( road_ptr )
      {
        idquad = (x0 > 0) + 2*(y0 < 0);
        idchi2 = road_ptr->get()->get_const_fitpar()->get_chi_square();
        idhits = road_ptr->get()->get_gapbit();
        reco_depth = road_ptr->get()->get_depth();

        DDG0 = MUTOO::RAD_TO_DEG*acos( (dxdz3*dxdz0 + dydz3*dydz0 + 1)/
          (dxdz3*dxdz3 + dydz3*dydz3 + 1)/
          (dxdz0*dxdz0 + dydz0*dydz0 + 1));
        if( DDG0 > 90 )  DDG0= 180-DDG0;

      } else {

        idquad = 0;
        idchi2 = 0;
        idhits = 0;
        reco_depth = 0;
        DDG0 = 0;
      }

      if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end best road" << endl;

      j = 0;
      nt_vars[j]   = event_number;
      nt_vars[++j] = n_reco_trks;
      nt_vars[++j] = reco_flag;
      nt_vars[++j] = centrality;
      nt_vars[++j] = mc_pid;
      nt_vars[++j] = mc_parent_pid;
      nt_vars[++j] = mc_charge;
      nt_vars[++j] = mc_track_id;
      nt_vars[++j] = mc_true_hits;
      nt_vars[++j] = mc_true_hits_st1;
      nt_vars[++j] = mc_true_hits_st2;
      nt_vars[++j] = mc_true_hits_st3;
      nt_vars[++j] = mc_ptot;
      nt_vars[++j] = mc_px;
      nt_vars[++j] = mc_py;
      nt_vars[++j] = mc_pz;
      nt_vars[++j] = mc_depth;
      nt_vars[++j] = reco_true_hits;
      nt_vars[++j] = reco_true_hits_st1;
      nt_vars[++j] = reco_true_hits_st2;
      nt_vars[++j] = reco_true_hits_st3;
      nt_vars[++j] = reco_true_rank;
      nt_vars[++j] = reco_hits_st1;
      nt_vars[++j] = reco_hits_st2;
      nt_vars[++j] = reco_hits_st3;
      nt_vars[++j] = reco_clus_multi_coord;
      nt_vars[++j] = reco_charge;
      nt_vars[++j] = reco_ptot;
      nt_vars[++j] = reco_px;
      nt_vars[++j] = reco_py;
      nt_vars[++j] = reco_pz;
      nt_vars[++j] = reco_chi2;
      nt_vars[++j] = DG0;
      nt_vars[++j] = DDG0;
      nt_vars[++j] = idquad;
      nt_vars[++j] = idchi2;
      nt_vars[++j] = idhits;
      nt_vars[++j] = reco_depth;
      nt_vars[++j] = mc_clus_multi_coord;
      nt_vars[++j] = mutr_hits;
      nt_vars[++j] = reco_clus_multi_coord_st1;
      nt_vars[++j] = reco_clus_multi_coord_st2;
      nt_vars[++j] = reco_clus_multi_coord_st3;

      _cu_effic_nt->Fill(nt_vars);

    }

    if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end second track look" << endl;

    // now do mc track that wasn't reconstructed, in the odd case that this happened
    if( !n_reco_trks )
    {
      TMutEvalMap::const_key_iterator eval_iter = mc_trk_ptr->get()->get_associated<TMutEval>();
      if( TMutEvalMap::const_pointer eval_ptr = eval_iter.next() )
      {
        reco_true_hits = eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits();
        reco_true_hits_st1 = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(0);
        reco_true_hits_st2 = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(1);
        reco_true_hits_st3 = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(2);
        mc_true_hits = eval_ptr->get()->get_trk_eval()->get_n_total_true_hits();
        mc_ptot = eval_ptr->get()->get_trk_eval()->get_ptot_true_vx();
        mc_px = eval_ptr->get()->get_trk_eval()->get_px_true_vx();
        mc_py = eval_ptr->get()->get_trk_eval()->get_py_true_vx();
        mc_pz = eval_ptr->get()->get_trk_eval()->get_pz_true_vx();
      }

      j = 0;
      nt_vars[j]   = event_number;
      nt_vars[++j] = n_reco_trks;
      nt_vars[++j] = reco_flag;
      nt_vars[++j] = centrality;
      nt_vars[++j] = mc_pid;
      nt_vars[++j] = mc_parent_pid;
      nt_vars[++j] = mc_charge;
      nt_vars[++j] = mc_track_id;
      nt_vars[++j] = mc_true_hits;
      nt_vars[++j] = mc_true_hits_st1;
      nt_vars[++j] = mc_true_hits_st2;
      nt_vars[++j] = mc_true_hits_st3;
      nt_vars[++j] = mc_ptot;
      nt_vars[++j] = mc_px;
      nt_vars[++j] = mc_py;
      nt_vars[++j] = mc_pz;
      nt_vars[++j] = mc_depth;
      nt_vars[++j] = reco_true_hits;
      nt_vars[++j] = reco_true_hits_st1;
      nt_vars[++j] = reco_true_hits_st2;
      nt_vars[++j] = reco_true_hits_st3;
      nt_vars[++j] = 0; //reco_true_rank;
      nt_vars[++j] = 0; //reco_hits_st1;
      nt_vars[++j] = 0; //reco_hits_st2;
      nt_vars[++j] = 0; //reco_hits_st3;
      nt_vars[++j] = 0; //reco_clus_multi_coord;
      nt_vars[++j] = 0; //reco_charge;
      nt_vars[++j] = 0; //reco_ptot;
      nt_vars[++j] = 0; //reco_px;
      nt_vars[++j] = 0; //reco_py;
      nt_vars[++j] = 0; //reco_pz;
      nt_vars[++j] = 0; //reco_chi2;
      nt_vars[++j] = 0; //DG0;
      nt_vars[++j] = 0; //DDG0;
      nt_vars[++j] = 0; //idquad;
      nt_vars[++j] = 0; //idchi2;
      nt_vars[++j] = 0; //idhits;
      nt_vars[++j] = 0; //reco_depth;
      nt_vars[++j] = mc_clus_multi_coord;
      nt_vars[++j] = mutr_hits;
      nt_vars[++j] = 0; //reco_clus_multi_coord_st1;
      nt_vars[++j] = 0; //reco_clus_multi_coord_st2;
      nt_vars[++j] = 0; //reco_clus_multi_coord_st3;

      _cu_effic_nt->Fill(nt_vars);
    }
  }

  if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end MC track loop" << endl;

  // Now let's fill the reconstructed tracks that aren't associated with a MC track...
  TMutTrkMap::const_iterator second_trk_iter = _trk_map->range();
  while( TMutTrkMap::const_pointer second_trk_ptr = second_trk_iter.next() )
  {

    // check if track is acceptable
    if( second_trk_ptr->get()->get_ghost() || !second_trk_ptr->get()->get_reco_success() ) continue;

    if( (second_trk_ptr->get()->get_associated<TMutMCTrk>()).count() ==0 )
    {

      n_reco_trks = 0;
      reco_flag = 1;

      reco_hits_st1 = 0;
      reco_hits_st2 = 0;
      reco_hits_st3 = 0;
      reco_clus_multi_coord = 0;
      TMutCoordMap::const_key_iterator coord_iter = second_trk_ptr->get()->get_associated<TMutCoord>();
      while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
        TMutClusMap::const_key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
        while( TMutClusMap::const_pointer clus_ptr = clus_iter.next() )
        {
          if( clus_ptr->get()->get_station() == 0 )         ++reco_hits_st1;
          else if( clus_ptr->get()->get_station() == 1 )    ++reco_hits_st2;
          else                                              ++reco_hits_st3;

          if( (clus_ptr->get()->get_associated<TMutCoord>()).count() > 1 )
          {
            ++reco_clus_multi_coord;
            if (clus_ptr->get()->get_station() == 0) ++reco_clus_multi_coord_st1;
            if (clus_ptr->get()->get_station() == 1) ++reco_clus_multi_coord_st2;
            if (clus_ptr->get()->get_station() == 2) ++reco_clus_multi_coord_st3;
          }
        }
      }

      if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end coord map loop (second)" << endl;

      reco_charge = (Int_t) second_trk_ptr->get()->get_charge();
      reco_ptot = second_trk_ptr->get()->get_trk_par_vtx()->get_ptot();
      reco_px = second_trk_ptr->get()->get_trk_par_vtx()->get_px();
      reco_py = second_trk_ptr->get()->get_trk_par_vtx()->get_py();
      reco_pz = second_trk_ptr->get()->get_trk_par_vtx()->get_pz();
      reco_chi2 = second_trk_ptr->get()->get_w_chi_square_pdf();

      // Fill MUID variables:
      //
      DG0 = 1000;
      x3 = 0;
      y3 = 0;
      z3 = 0;
      px3 = 0;
      py3 = 0;
      pz3 = 0;
      dxdz3 = 0;
      dydz3 = 0;
      x0 = 0;
      y0 = 0;
      z0 = 0;
      x0_new = 0;
      y0_new = 0;
      z0_new = 0;
      dxdz0 = 0;
      dydz0 = 0;
      x0_temp = 0;
      y0_temp = 0;
      z0_temp = 0;
      dxdz0_temp = 0;
      dydz0_temp = 0;
      DG0_temp = 100;
      DG0x = 0;
      DG0y = 0;
      x_kal = 0;
      y_kal = 0;
      z_kal = 0;
      px_kal = 0;
      py_kal = 0;
      pz_kal = 0;
      dxdz_kal = 0;
      dydz_kal = 0;
      DG0x_kal = 0;
      DG0y_kal = 0;
      DG0_kal = 100;
      DG0_kal_temp = 1000;

      // current algorithm (pro.55) takes the road with smallest DG0:
      TMuiRoadMapO::pointer second_road_ptr( 0 );

      TMuiRoadMapO::key_iterator second_road_iter = second_trk_ptr->get()->get_associated<TMuiRoadO>();
      while( TMuiRoadMapO::pointer temp_road_ptr2 = second_road_iter.next() )
      {
        cout << "new road" << endl;
        x3 = second_trk_ptr->get()->get_trk_par_station(2)->get_x();
        y3 = second_trk_ptr->get()->get_trk_par_station(2)->get_y();
        z3 = second_trk_ptr->get()->get_trk_par_station(2)->get_z();
        px3 = second_trk_ptr->get()->get_trk_par_station(2)->get_px();
        py3 = second_trk_ptr->get()->get_trk_par_station(2)->get_py();
        pz3 = second_trk_ptr->get()->get_trk_par_station(2)->get_pz();
        dxdz3 = px3/pz3;
        dydz3 = py3/pz3;

        x_kal = (second_trk_ptr->get()->get_trk_par_list()->back()).get_x();
        y_kal = (second_trk_ptr->get()->get_trk_par_list()->back()).get_y();
        z_kal = (second_trk_ptr->get()->get_trk_par_list()->back()).get_z();
        px_kal = (second_trk_ptr->get()->get_trk_par_list()->back()).get_px();
        py_kal = (second_trk_ptr->get()->get_trk_par_list()->back()).get_py();
        pz_kal = (second_trk_ptr->get()->get_trk_par_list()->back()).get_pz();
        dxdz_kal = px_kal/pz_kal;
        dydz_kal = py_kal/pz_kal;

        gap0_temp = temp_road_ptr2->get()->get_gap0_point();
        x0_temp = gap0_temp.getX();
        y0_temp = gap0_temp.getY();
        z0_temp = gap0_temp.getZ();
        dxdz0_temp = temp_road_ptr2->get()->get_const_fitpar()->get_dxdz();
        dydz0_temp = temp_road_ptr2->get()->get_const_fitpar()->get_dydz();
        DG0_temp = sqrt( pow(x0_temp-x3-dxdz3*(z0_temp-z3),2) + pow(y0_temp-y3-dydz3*(z0_temp-z3),2) );
        DG0_kal_temp = sqrt( pow(x0_temp-x_kal-dxdz_kal*(z0_temp-z_kal),2) + pow(y0_temp-y_kal-dydz_kal*(z0_temp-z_kal),2) );

        if( DG0_temp < DG0 )
        {
          second_road_ptr = temp_road_ptr2;
          gap0 = gap0_temp;
          x0 = x0_temp;
          y0 = y0_temp;
          z0 = z0_temp;
          dxdz0 = dxdz0_temp;
          dydz0 = dydz0_temp;
          DG0 = DG0_temp;
          DG0x = x0_temp-x3-dxdz3*(z0_temp-z3);
          DG0y = y0_temp-y3-dydz3*(z0_temp-z3);
        }

        if( DG0_kal_temp < DG0_kal )
        {
          x0_new = x0_temp;
          y0_new = y0_temp;
          z0_new = z0_temp;
          DG0x_kal = x0_temp-x_kal-dxdz_kal*(z0_temp-z_kal);
          DG0y_kal = y0_temp-y_kal-dydz_kal*(z0_temp-z_kal);
          DG0_kal = DG0_kal_temp;
        }
      }

      if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple - end road map loop (second)" << endl;

      if( second_road_ptr != NULL)
      {
        idquad = (x0 > 0) + 2*(y0 < 0);
        idchi2 = second_road_ptr->get()->get_const_fitpar()->get_chi_square();
        idhits = second_road_ptr->get()->get_gapbit();
        reco_depth = second_road_ptr->get()->get_depth();

        DDG0 = MUTOO::RAD_TO_DEG*acos( (dxdz3*dxdz0 + dydz3*dydz0 + 1)/
          (dxdz3*dxdz3 + dydz3*dydz3 + 1)/
          (dxdz0*dxdz0 + dydz0*dydz0 + 1));
        if( DDG0 > 90 )  DDG0 = 180-DDG0;

      } else {
        idquad = 0;
        idchi2 = 0;
        idhits = 0;
        reco_depth = 0;
        DDG0 = 0;
      }

      // fill ntuple
      j = 0;
      nt_vars[j]   = event_number;
      nt_vars[++j] = n_reco_trks;
      nt_vars[++j] = reco_flag;
      nt_vars[++j] = centrality;
      nt_vars[++j] = 0; //mc_pid;
      nt_vars[++j] = 0; //mc_parent_pid;
      nt_vars[++j] = 0; //mc_charge;
      nt_vars[++j] = 0; //mc_track_id;
      nt_vars[++j] = 0; //mc_true_hits;
      nt_vars[++j] = 0; //mc_true_hits_st1;
      nt_vars[++j] = 0; //mc_true_hits_st2;
      nt_vars[++j] = 0; //mc_true_hits_st3;
      nt_vars[++j] = 0; //mc_ptot;
      nt_vars[++j] = 0; //mc_px;
      nt_vars[++j] = 0; //mc_py;
      nt_vars[++j] = 0; //mc_pz;
      nt_vars[++j] = 0; //mc_depth;
      nt_vars[++j] = 0; //reco_true_hits;
      nt_vars[++j] = 0; //reco_true_hits_st1;
      nt_vars[++j] = 0; //reco_true_hits_st2;
      nt_vars[++j] = 0; //reco_true_hits_st3;
      nt_vars[++j] = 0; //reco_true_rank;
      nt_vars[++j] = reco_hits_st1;
      nt_vars[++j] = reco_hits_st2;
      nt_vars[++j] = reco_hits_st3;
      nt_vars[++j] = reco_clus_multi_coord;
      nt_vars[++j] = reco_charge;
      nt_vars[++j] = reco_ptot;
      nt_vars[++j] = reco_px;
      nt_vars[++j] = reco_py;
      nt_vars[++j] = reco_pz;
      nt_vars[++j] = reco_chi2;
      nt_vars[++j] = DG0;
      nt_vars[++j] = DDG0;
      nt_vars[++j] = idquad;
      nt_vars[++j] = idchi2;
      nt_vars[++j] = idhits;
      nt_vars[++j] = reco_depth;
      nt_vars[++j] = 0;  // no mc information in this case - mc_clus_multi_coord;
      nt_vars[++j] = mutr_hits;
      nt_vars[++j] = reco_clus_multi_coord_st1;
      nt_vars[++j] = reco_clus_multi_coord_st2;
      nt_vars[++j] = reco_clus_multi_coord_st3;
      nt_vars[++j] = x3;
      nt_vars[++j] = y3;
      nt_vars[++j] = z3;
      nt_vars[++j] = px3;
      nt_vars[++j] = py3;
      nt_vars[++j] = pz3;
      nt_vars[++j] = dxdz3;
      nt_vars[++j] = dydz3;
      nt_vars[++j] = x0;
      nt_vars[++j] = y0;
      nt_vars[++j] = z0;
      nt_vars[++j] = x0_new;
      nt_vars[++j] = y0_new;
      nt_vars[++j] = z0_new;
      nt_vars[++j] = dxdz0;
      nt_vars[++j] = dydz0;
      nt_vars[++j] = DG0x;
      nt_vars[++j] = DG0y;
      nt_vars[++j] = x_kal;
      nt_vars[++j] = y_kal;
      nt_vars[++j] = z_kal;
      nt_vars[++j] = px_kal;
      nt_vars[++j] = py_kal;
      nt_vars[++j] = pz_kal;
      nt_vars[++j] = dxdz_kal;
      nt_vars[++j] = dydz_kal;
      nt_vars[++j] = DG0x_kal;
      nt_vars[++j] = DG0y_kal;
      nt_vars[++j] = DG0_kal;

      _cu_effic_nt->Fill(nt_vars);

    } // end if statement
  }   // end track loop

  ++event_number;

  if( verbosity >= 1 ) cout << "MuonEval::write_cu_effic_ntuple.\n";
}

//____________________________________________________________
int MuonEval::get_mc_trk_depth( TMutMCTrkMap::const_pointer mc_trk_ptr )
{
  // get associated TMuiMCHitO
  int out( 0 );
  TMuiMCHitMapO::const_key_iterator mc_hit_iter( mc_trk_ptr->get()->get_associated<TMuiMCHitO>() );
  while( TMuiMCHitMapO::const_pointer mc_hit_ptr = mc_hit_iter.next() )
  out = max( out, static_cast<int>(mc_hit_ptr->get()->get_plane() ) );
  return out;
}

//____________________________________________________________
int MuonEval::get_max_road_depth( TMutTrkMap::const_pointer trk_ptr )
{

  int out( 0 );
  TMuiRoadMapO::const_key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );
  while( TMuiRoadMapO::const_pointer road_ptr = road_iter.next() )
  out = max( out, static_cast<int>(road_ptr->get()->get_depth() ) );
  return out;
}
