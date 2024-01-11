/*! ingroup analysis */
/*! \file mutoo_dst.cxx 
  This routine is a dreco style analysis event loop for
  doing MUTOO re-analysis from DST format data format data.  
*/

#include <iostream>
#include "MUTOO.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "PHIODataNode.h"
#include "PHNodeReset.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TSystem.h"
#include "PhenixRun.hh"
#include "PhCanvas.hh"
#include "PhDchDisplay.hh"
#include "PhPadDisplay.hh"
#include "PhTecDisplay.hh"
#include "PhMuiDisplay.hh"
#include "PhMutDisplay.hh"
#include "PhEventDisplay.hh"
#include "TMutTrackUtil.h"
#include "bend_plane.h"
#include "PHTimer.h"
#include "TMutLocalDBInit.h"

PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
static bool init_done = false;

// MUTOO module pointers
//
mMutFindClus* mMutFindClusMod;
mMutFitClus* mMutFitClusMod;
mMutFindStub* mMutFindStubMod;
mMutStubFit* mMutStubFitMod;
mMutFindGapCoord* mMutFindGapCoordMod;
mMutStartTracks* mMutStartTracksMod;
mMutFindTrackMC* mMutFindTrackMCMod;
mMutFindTrack* mMutFindTrackMod;
mMutTrackFit* mMutTrackFitMod;
mMutFindVtx* mMutFindVtxMod;
mMutFitVtx* mMutFitVtxMod;
mMutKalFit* mMutKalFitMod;
mMutResponse* mMutResponseMod;
mMutEval* mMutEvalMod;
mMutBPFit* mMutBPFitMod;
mMutBPVertex* mMutBPVertexMod;

// IO manager for output DST file
//
PHNodeIOManager *dstout = 0;
PHCompositeNode *dstout_node=0;
PhMutooDisplay *display = 0;
PHTimer cumulative_timer("BEND PLANE");

// Ntuple output
//
TFile*   ana_file;
TNtuple* nt1;
TNtuple* nt2;

int process_event (PHCompositeNode *dst_node)
{
  
  static size_t ievent=0;
  static size_t iwrite=0;
  std::cout << ++ ievent << std::endl;
  try {
    if(!init_done) { 
      // Creates mutoo_node and top_node
      //
      setup_all(dst_node);
      // Append mutoo_node and dst_node to new top_node
      //
      top_node->addNode(mutoo_node);
      top_node->addNode(dst_node);
      top_node->print();
      ULong_t default_run = 69522;
      TMutLocalDBInit::initialize(top_node, default_run);
      init_done=true;
    }
    
    // Clear first for event display
    //
    PHMapManager::clear();
    
    // Fills IOCs coupled to the DST node
    //
    PHMapManager::read(top_node);
    
    // Reconstruction modules
    //
    mMutResponseMod->event(top_node);
    mMutFindClusMod->event(top_node);
    mMutFitClusMod->event(top_node);
    mMutFindGapCoordMod->event(top_node);
    mMutFindTrackMCMod->event(top_node);
    //mMutFindTrackMod->event(top_node);
    cumulative_timer.restart();
    mMutBPFitMod->event(top_node);
    cumulative_timer.stop();
    mMutFindVtxMod->event(top_node);
    mMutBPVertexMod->event(top_node);
    // Run the evaluation module
    //
    mMutEvalMod->event(top_node);
    
    // Fill the output ntuple
    //
    fill_ntuple();
    fill_pair_ntuple();
    
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }  
  return 0;
}

int setup_all(PHCompositeNode* dst_node) {

  ana_file = new TFile("mutoo.nt","recreate");

  // Instantiate the output ntuple here
  //
  nt1 = new TNtuple("nt1","nt1","p1true:p1:pvtxtrue:pvtx:bpfit:charge:chisq:x:y:z:px:py:pz");     
  nt2 = new TNtuple("nt2","nt2","arm:mass:sign:zvtx:charg1:charge2:px1:px2:py1:py2:pz1:pz2");     

  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");
  
  // create a node for the output DST
  //
  dstout_node = new PHCompositeNode("DST");
  
  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");
  
  // instantiate new maps
  //    
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_node(mutoo_node,"TMutHitMap");
  TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_node(mutoo_node,"TMutClusMap");
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_node(mutoo_node,"TMutGapCoordMap");  
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap");
  TMutEvalMap* eval_map = TMutNode<TMutEvalMap>::new_node(mutoo_node, "TMutEvalMap");
  TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_node(mutoo_node, "TMutStubMap");
  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_node(mutoo_node, "TMutTrkMap");
  TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::new_node(mutoo_node, "TMutVtxMap");

  // instantiate maps coupled to DST resident IOs
  //
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::new_dst_input_node(mutoo_node,"TMutMCHitMap",
									dst_node, "TMutMCHit");  
  TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::new_dst_input_node(mutoo_node,"TMutMCTrkMap",
									dst_node, "TMutMCTrk");  

  // Instantiate modules and runtime parameter tables
  //  

  mMutBPVertexMod = new mMutBPVertex();
  TMutNode<mMutBPVertexPar>::new_node(mutoo_node,"mMutBPVertexPar");
  
  mMutBPFitMod = new mMutBPFit();
  TMutNode<mMutBPFitPar>::new_node(mutoo_node,"mMutBPFitPar");

  mMutResponseMod = new mMutResponse();
  TMutNode<mMutResponsePar>::new_node(mutoo_node,"mMutResponsePar");  
  
  mMutFindClusMod = new mMutFindClus();
  TMutNode<mMutFindClusPar>::new_node(mutoo_node,"mMutFindClusPar");  
  
  mMutFitClusMod = new mMutFitClus();
  TMutNode<mMutFitClusPar>::new_node(mutoo_node,"mMutFitClusPar");  
  
  mMutFindGapCoordMod = new mMutFindGapCoord();
  TMutNode<mMutFindGapCoordPar>::new_node(mutoo_node,"mMutFindGapCoordPar");  

  mMutFindStubMod = new mMutFindStub();
  TMutNode<mMutFindStubPar>::new_node(mutoo_node,"mMutFindStubPar");  

  mMutStubFitMod = new mMutStubFit();
  TMutNode<mMutStubFitPar>::new_node(mutoo_node,"mMutStubFitPar");  
  
  mMutStartTracksMod = new mMutStartTracks();
  TMutNode<mMutStartTracksPar>::new_node(mutoo_node,"mMutStartTracksPar");  

  mMutFindTrackMCMod = new mMutFindTrackMC();
  TMutNode<mMutFindTrackMCPar>::new_node(mutoo_node,"mMutFindTrackMCPar");  

  mMutFindTrackMod = new mMutFindTrack();
  TMutNode<mMutFindTrackPar>::new_node(mutoo_node,"mMutFindTrackPar");  
  
  mMutTrackFitMod = new mMutTrackFit();
  TMutNode<mMutTrackFitPar>::new_node(mutoo_node,"mMutTrackFitPar");  

  mMutFindVtxMod = new mMutFindVtx();
  TMutNode<mMutFindVtxPar>::new_node(mutoo_node,"mMutFindVtxPar");  
  
  mMutKalFitMod = new mMutKalFit();
  TMutNode<mMutKalFitPar>::new_node(mutoo_node,"mMutKalFitPar");  

  mMutFitVtxMod = new mMutFitVtx();
  TMutNode<mMutFitVtxPar>::new_node(mutoo_node,"mMutFitVtxPar");  
  
  mMutEvalMod = new mMutEval();
  TMutNode<mMutEvalPar>::new_node(mutoo_node,"mMutEvalPar");  

  // change default runtime parameters here
  //
  mMutBPVertexPar* bpv_par = TMutNode<mMutBPVertexPar>::find_node(mutoo_node,"mMutBPVertexPar");    
  bpv_par->set_verbosity(MUTOO::ALOT);

  mMutBPFitPar* bpf_par = TMutNode<mMutBPFitPar>::find_node(mutoo_node,"mMutBPFitPar");    
  bpf_par->set_verbosity(MUTOO::ALOT);

  mMutResponsePar* rsp_par = TMutNode<mMutResponsePar>::find_node(mutoo_node,"mMutResponsePar");    
  rsp_par->set_verbosity(MUTOO::NONE);
  // Turn off charge smearing
  //
  rsp_par->set_smear_q(false);

  mMutFindClusPar* fnc_par = TMutNode<mMutFindClusPar>::find_node(mutoo_node,"mMutFindClusPar");  
  fnc_par->set_verbosity(MUTOO::NONE);
  
  mMutFitClusPar* fic_par = TMutNode<mMutFitClusPar>::find_node(mutoo_node,"mMutFitClusPar");  
  fic_par->set_verbosity(MUTOO::NONE);
  
  mMutFindGapCoordPar* fgc_par = TMutNode<mMutFindGapCoordPar>::find_node(mutoo_node,"mMutFindGapCoordPar");  
  fgc_par->set_verbosity(MUTOO::NONE);

  mMutFindTrackMCPar* fmc_par = TMutNode<mMutFindTrackMCPar>::find_node(mutoo_node,"mMutFindTrackMCPar");  
  fmc_par->set_init_mode(mMutFindTrackMCPar::PERFECT);
  fmc_par->set_vtx_mode(mMutFindTrackMCPar::VTX_MINIMAL);
  fmc_par->set_use_ms_covar(1);
  fmc_par->set_verbosity(MUTOO::NONE);

  mMutFindTrackPar* ftr_par = TMutNode<mMutFindTrackPar>::find_node(mutoo_node,"mMutFindTrackPar");  
  ftr_par->set_verbosity(MUTOO::ALOT);

  mMutFindStubPar* fis_par = TMutNode<mMutFindStubPar>::find_node(mutoo_node,"mMutFindStubPar");  
  fis_par->set_verbosity(MUTOO::NONE);

  mMutStubFitPar* fts_par = TMutNode<mMutStubFitPar>::find_node(mutoo_node,"mMutStubFitPar");  
  fts_par->set_verbosity(MUTOO::NONE);
  
  mMutTrackFitPar* tft_par = TMutNode<mMutTrackFitPar>::find_node(mutoo_node,"mMutTrackFitPar");      
  tft_par->set_verbosity(MUTOO::NONE);

  mMutFindVtxPar* fvx_par = TMutNode<mMutFindVtxPar>::find_node(mutoo_node,"mMutFindVtxPar");      
  fvx_par->set_verbosity(MUTOO::NONE);
  
  mMutFitVtxPar* fiv_par = TMutNode<mMutFitVtxPar>::find_node(mutoo_node,"mMutFitVtxPar");      
  fiv_par->set_verbosity(MUTOO::NONE);
  
  mMutKalFitPar* kft_par = TMutNode<mMutKalFitPar>::find_node(mutoo_node,"mMutKalFitPar");      
  kft_par->set_verbosity(MUTOO::NONE);
  
  mMutEvalPar* evl_par = TMutNode<mMutEvalPar>::find_node(mutoo_node,"mMutEvalPar");      
  evl_par->set_pr_mode(mMutEvalPar::NORMAL);
    
  return 0;  
}

int end_all() {
  ana_file->Write();
  delete dstout;
  MUTOO::TRACE("terminated normally");
  return 0;
}


int
dstout_fopen(PHString dstout_filename)
{
  dstout = new PHNodeIOManager(dstout_filename, PHWrite);
  dstout->SetCompressionLevel(3);
  return 0;
}

void draw_plane(UShort_t arm, UShort_t octant){
  if(!display) do_display();
  display->paint_plane_view(arm,octant);
}


int dinit() {
  return 0;
}

void dump_hit() {
  try {
    TMutHitMap* trk_map = TMutNode<TMutHitMap>::find_node(mutoo_node,"TMutHitMap");          
    trk_map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_clus() {
  try {
    TMutClusMap* map = TMutNode<TMutClusMap>::find_node(mutoo_node,"TMutClusMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_coord() {
  try {
    TMutCoordMap* map = TMutNode<TMutCoordMap>::find_node(mutoo_node,"TMutCoordMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_gap_coord() {
  try {
    TMutGapCoordMap* map = TMutNode<TMutGapCoordMap>::find_node(mutoo_node,"TMutGapCoordMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_stub() {
  try {
    TMutStubMap* map = TMutNode<TMutStubMap>::find_node(mutoo_node,"TMutStubMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_mc_trk() {
  try {
    TMutMCTrkMap* map = TMutNode<TMutMCTrkMap>::find_node(mutoo_node,"TMutMCTrkMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_mc_hit() {
  try {
    TMutMCHitMap* map = TMutNode<TMutMCHitMap>::find_node(mutoo_node,"TMutMCHitMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_trk() {
  try {
    TMutTrkMap* map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_vtx() {
  try {
    TMutVtxMap* map = TMutNode<TMutVtxMap>::find_node(mutoo_node,"TMutVtxMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_eval() {
  try {
    TMutEvalMap* map = TMutNode<TMutEvalMap>::find_node(mutoo_node,"TMutEvalMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

bool setup_display() {    
  display = new PhMutooDisplay();
  display->set_draw_tracks(true);
  display->set_draw_stubs(false);
  return true;
}

void do_display(){  
  static bool display_mode = setup_display();
  display->event(top_node);
}

void draw_octant(UShort_t arm, int octant, int station){
  do_display();
  display->paint_octant_view(arm, octant, station);
}

void draw_side(UShort_t arm, int octant, int station){
  do_display();
  display->paint_side_view(arm, octant, station);
}

void fill_ntuple()
{
  // Fill the output ntuple here
  // 
  TMutEvalMap* eval_map = TMutNode<TMutEvalMap>::find_node(mutoo_node,"TMutEvalMap");
  TMutEvalMap::const_iterator eval_iter = eval_map->range();
  float ntvar[20] = {0};
  while(TMutEvalMap::const_pointer eval_ptr = eval_iter.next()){
    ntvar[0] = eval_ptr->get()->get_trk_eval()->get_ptot_true_us();
    ntvar[1] = eval_ptr->get()->get_trk_eval()->get_ptot_reco_us();
    ntvar[2] = eval_ptr->get()->get_trk_eval()->get_ptot_true_vx();
    ntvar[3] = eval_ptr->get()->get_trk_eval()->get_ptot_reco_vx();
    
    TMutTrkMap::key_iterator trk_iter_nw = eval_ptr->get()->get_associated<TMutTrk>();
    if(!trk_iter_nw.at_end()){
      TMutTrkMap::pointer trk_ptr_nw = trk_iter_nw.current();
      ntvar[4] = trk_ptr_nw->get()->get_bp_fit();
      ntvar[5] = trk_ptr_nw->get()->get_bp_par()->get_charge();
      ntvar[6] = trk_ptr_nw->get()->get_bp_par()->get_chi_sq();
      ntvar[7] = trk_ptr_nw->get()->get_bp_par()->get_x_vtx();
      ntvar[8] = trk_ptr_nw->get()->get_bp_par()->get_y_vtx();
      ntvar[9] = trk_ptr_nw->get()->get_bp_par()->get_z_vtx();
      ntvar[10] = trk_ptr_nw->get()->get_bp_par()->get_px_vtx();
      ntvar[11] = trk_ptr_nw->get()->get_bp_par()->get_py_vtx();
      ntvar[12] = trk_ptr_nw->get()->get_bp_par()->get_pz_vtx();
    }
    nt1->Fill(ntvar);
  }
}

void fill_pair_ntuple()
{
  // Fill the output ntuple here
  // 
  TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(mutoo_node,"TMutVtxMap");
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  float ntvar[10] = {0};
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next()){
    vtx_ptr->get()->print();
    ntvar[0] = vtx_ptr->get()->get_arm();
    ntvar[1] = vtx_ptr->get()->get_mass();
    ntvar[2] = vtx_ptr->get()->get_sign();
    ntvar[3] = vtx_ptr->get()->get_vtx_par()->get_z();
    ntvar[4] = vtx_ptr->get()->get_vtx_par()->get_charge1();
    ntvar[5] = vtx_ptr->get()->get_vtx_par()->get_charge2();
    ntvar[6] = vtx_ptr->get()->get_vtx_par()->get_px1();
    ntvar[7] = vtx_ptr->get()->get_vtx_par()->get_px2();
    ntvar[8] = vtx_ptr->get()->get_vtx_par()->get_py1();
    ntvar[9] = vtx_ptr->get()->get_vtx_par()->get_py2();
    ntvar[10] = vtx_ptr->get()->get_vtx_par()->get_pz1();
    ntvar[11] = vtx_ptr->get()->get_vtx_par()->get_pz2();
    nt2->Fill(ntvar);
  }
}

