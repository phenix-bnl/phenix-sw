/*! ingroup analysis */
/*! \file mutoo_dst.cxx 
  This routine is a dreco style analysis event loop for
  doing MUTOO re-analysis from DST format data format data.  
*/

#include <iostream>
#include "response_dst.h"
#include "MUTOO.h"
#include "ezdst.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "PHIODataNode.h"
#include "PHNodeReset.h"

#include "TSystem.h"
#include "PhenixRun.hh"
#include "PhCanvas.hh"
#include "PhDchDisplay.hh"
#include "PhPadDisplay.hh"
#include "PhTecDisplay.hh"
#include "PhMuiDisplay.hh"
#include "PhMutDisplay.hh"
#include "PhEventDisplay.hh"

PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
static bool init_done = false;

// MUTOO module pointers
//
mMutUnpackMutDst* mMutUnpackMutDstMod;
mMutFindClus* mMutFindClusMod;
mMutFitClus* mMutFitClusMod;
mMutFindStub* mMutFindStubMod;
mMutStubFit* mMutStubFitMod;
mMutFindGapCoord* mMutFindGapCoordMod;
mMutStartTracks* mMutStartTracksMod;
mMutFindTrackMC* mMutFindTrackMCMod;
mMutTrackFit* mMutTrackFitMod;
mMutKalFit* mMutKalFitMod;
mMutResponse* mMutResponseMod;
mMutEval* mMutEvalMod;

// IO manager for output DST file
//
PHNodeIOManager *dstout = 0;
PHCompositeNode *dstout_node=0;
PhMutooDisplay *display = 0;


int process_event (DstContent *dst)
{
  
  enum Mode {ALIGN, TRACKING, KALMAN};
  Mode mode = KALMAN;
  static size_t ievent=0;
  ++ievent;
  std::cout << ievent << std::endl;
  try {

    if(!init_done) { 
      setup_all(dst->get_topNode());
      top_node->addNode(mutoo_node);
      top_node->addNode(dst->get_topNode());
      top_node->print();
    }

    // Clear first for event display
    //
    PHMapManager::clear();
    
    // Fills IOCs coupled to the DST node
    //
    PHMapManager::read(dst->get_topNode());
    
    // Reconstruction modules
    //
    mMutResponseMod->event(top_node);
    mMutFindClusMod->event(top_node);
    mMutFitClusMod->event(top_node);
    mMutFindGapCoordMod->event(top_node);
    
    if(mode == ALIGN) {
      mMutFindStubMod->event(top_node);
      mMutStubFitMod->event(top_node);
      TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(mutoo_node,"TMutStubMap");          
      TMutStubMap::const_iterator stub_iter = stub_map->range();
      while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
  	stub_ptr->get()->print(std::cout,true);
      }
    } else if(mode == TRACKING) {
      mMutFindTrackMCMod->event(top_node);
      mMutTrackFitMod->event(top_node);
    } else if(mode == KALMAN) {
      mMutFindTrackMCMod->event(top_node);
      mMutKalFitMod->event(top_node);
    }

    // Write the output DST
    //
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");          
    if(trk_map->size()){
      PHMapManager::write();    
      dstout->write(dstout_node);
    }

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  init_done=true;
  return 0;
}

bool setup_display() {

  if(!gClient){
    cout<<"you are running in batch"<<endl;
    return false;
  }

  // This creates the gPhenix pointer
  //
  new PhenixRun("Phenix-Run","Phenix-Run",top_node);
  
  // Add MUTOO event display
  //
  display = new PhMutooDisplay();
  display->set_draw_mc_tracks(false);
  gPhenix->GetDisplay()->AddSubsystem(display);
  
  // Hack from dispfuncs.C
  //
  PhEventDisplay* disp = new PhEventDisplay("Main",
					    gClient->GetRoot(),
					    1000,
					    1000,
					    gPhenix->GetDisplay()->GetList());
  
  PhCanvas* can = (PhCanvas*)(disp->GetCanvas());
  can->DefaultRange(-550,-550,-550,550,550,550); //in cm
  can->ResetRange();
  disp->Popup();  

  return true;

}

int setup_all(PHCompositeNode* dst_node) {

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
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_node(mutoo_node,"TMutGapCoordMap");  
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap");
  TMutEvalMap* eval_map = TMutNode<TMutEvalMap>::new_node(mutoo_node, "TMutEvalMap");
  TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_node(mutoo_node, "TMutStubMap");
  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_node(mutoo_node, "TMutTrkMap");
  TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_node(mutoo_node,"TMutClusMap");

  // instantiate maps coupled to DST resident IOs
  //
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::new_dst_input_node(mutoo_node,"TMutMCHitMap",
									dst_node, "TMutMCHit");  
  TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::new_dst_input_node(mutoo_node,"TMutMCTrkMap",
									dst_node, "TMutMCTrk");  
  
  // choose which maps are persistent
  //
  mc_hit_map->make_persistent(dstout_node,"TMutMCHit");
  eval_map->make_persistent(dstout_node,"TMutEval");
  mc_trk_map->make_persistent(dstout_node,"TMutMCTrk");
  hit_map->make_persistent(dstout_node,"TMutHit");
  clus_map->make_persistent(dstout_node,"TMutClus");
  gap_coord_map->make_persistent(dstout_node,"TMutGapCoord");
  stub_map->make_persistent(dstout_node,"TMutStub");
  trk_map->make_persistent(dstout_node,"TMutTrk");
  coord_map->make_persistent(dstout_node,"TMutCoord");  

  // Instantiate modules and runtime parameter tables
  //  
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
  
  mMutTrackFitMod = new mMutTrackFit();
  TMutNode<mMutTrackFitPar>::new_node(mutoo_node,"mMutTrackFitPar");  

  mMutKalFitMod = new mMutKalFit();
  TMutNode<mMutKalFitPar>::new_node(mutoo_node,"mMutKalFitPar");  
  
  mMutEvalMod = new mMutEval();
  TMutNode<mMutEvalPar>::new_node(mutoo_node,"mMutEvalPar");  
  
  // change default runtime parameters here
  //
  mMutResponsePar* rsp_par = TMutNode<mMutResponsePar>::find_node(mutoo_node,"mMutResponsePar");    
  rsp_par->set_verbosity(MUTOO::NONE);
  
  mMutFindClusPar* fnc_par = TMutNode<mMutFindClusPar>::find_node(mutoo_node,"mMutFindClusPar");  
  fnc_par->set_verbosity(MUTOO::NONE);
  
  mMutFitClusPar* fic_par = TMutNode<mMutFitClusPar>::find_node(mutoo_node,"mMutFitClusPar");  
  fic_par->set_verbosity(MUTOO::NONE);
  
  mMutFindGapCoordPar* fgc_par = TMutNode<mMutFindGapCoordPar>::find_node(mutoo_node,"mMutFindGapCoordPar");  
  fgc_par->set_verbosity(MUTOO::NONE);

  mMutFindTrackMCPar* fmc_par = TMutNode<mMutFindTrackMCPar>::find_node(mutoo_node,"mMutFindTrackMCPar");  
  fmc_par->set_init_mode(mMutFindTrackMCPar::MINIMAL);
  fmc_par->set_verbosity(MUTOO::NONE);

  mMutFindStubPar* fis_par = TMutNode<mMutFindStubPar>::find_node(mutoo_node,"mMutFindStubPar");  
  fis_par->set_verbosity(MUTOO::NONE);

  mMutStubFitPar* fts_par = TMutNode<mMutStubFitPar>::find_node(mutoo_node,"mMutStubFitPar");  
  fts_par->set_verbosity(MUTOO::NONE);
  
  mMutStartTracksPar* str_par = TMutNode<mMutStartTracksPar>::find_node(mutoo_node,"mMutStartTracksPar");      
  str_par->set_verbosity(MUTOO::NONE);
  str_par->set_muid_mode(mMutStartTracksPar::NO_MUID);

  mMutTrackFitPar* tft_par = TMutNode<mMutTrackFitPar>::find_node(mutoo_node,"mMutTrackFitPar");      
  tft_par->set_verbosity(MUTOO::SOME);
  tft_par->set_residual_mode(mMutTrackFitPar::NORMAL);
  tft_par->set_fit_mode(mMutTrackFitPar::STANDARD);  
  tft_par->set_fit_type(mMutTrackFitPar::FIELD_ON);
  tft_par->set_min_n_coord(16);
  tft_par->set_use_anodes(1);
  tft_par->set_max_iterations(4);  

  mMutKalFitPar* kft_par = TMutNode<mMutKalFitPar>::find_node(mutoo_node,"mMutKalFitPar");      
  kft_par->set_verbosity(MUTOO::SOME);
  
  mMutEvalPar* evl_par = TMutNode<mMutEvalPar>::find_node(mutoo_node,"mMutEvalPar");      
  evl_par->set_pr_mode(mMutEvalPar::PERFECT);
  
  return 0;

}

int end_all() {
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

void dump_eval() {
  try {
    TMutEvalMap* map = TMutNode<TMutEvalMap>::find_node(mutoo_node,"TMutEvalMap");          
    map->print();
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void do_display(){  

  static bool display_mode = setup_display();

  // Event display setup
  //
  if(display_mode) {
    gPhenix->SetRunNum(0);
    gPhenix->event(top_node);  
    gPhenix->Draw();
  }
}














