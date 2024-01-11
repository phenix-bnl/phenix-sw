/*! ingroup analysis */
/*! \file mutoo_dst.cxx 
  This routine is a dreco style analysis event loop for
  doing MUTOO re-analysis from DST format data format data.  
*/

#include <iostream>
#include "ana_mut_dst.h"
#include "MUTOO.h"
#include "pdst.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"

#include "TMutNode.h"
#include "TMutMuiRoadMap.h"
#include "TMutHitMap.h"
#include "TMutClusMap.h"
#include "TMutCoordMap.h"
#include "TMutTrkMap.h"
#include "TMutClusMap.h"
//#include "TMutMCHitMap.h"
#include "TMutStubMap.h"

#include "PHIODataNode.h"
#include "PHNodeReset.h"
#include "TMutMathieson.h"
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
mMutMuiRoad* mMutMuiRoadMod;
mMutFindClus* mMutFindClusMod;
mMutFitClus* mMutFitClusMod;
mMutFindGapCoord* mMutFindGapCoordMod;
mMutStartTracks* mMutStartTracksMod;
mMutTrackFit* mMutTrackFitMod;
//mMutEvalFramework* mMutEvalFrameworkMod;
mMutFindStub* mMutFindStubMod;

// IO manager for output DST file
//
PHNodeIOManager *dstout = 0;
PHCompositeNode *dstout_node=0;
PhMutooDisplay *display = 0;

static bool display_mode = true;
static ULong_t ievent=0;

int dinit() {
  return 0;
}

int process_event (PHCompositeNode* dst_node)
{

  
  // Increment the event counter
  // 
  try {
    
    // Setup Interface Object Containers and display
    //
    if(!init_done) { 
      setup_all();
      if(display_mode) setup_display();
      top_node->addNode(mutoo_node);
      top_node->addNode(dst_node);
      top_node->print();
    }
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(mutoo_node,"TMutHitMap");      

    // Clear first for event display
    //
    PHMapManager::clear();    
    
    // Run the reconstruction
    //
    mMutUnpackMutDstMod->event(top_node);
    mMutFindClusMod->event(top_node);
    mMutFitClusMod->event(top_node);
    mMutFindGapCoordMod->event(top_node);
    mMutFindStubMod->event(top_node);
    mMutStubFitMod->event(top_node);
    TMutMuiRoadMap* road_map = TMutNode<TMutMuiRoadMap>::find_node(mutoo_node,"TMutMuiRoadMap");    
    road_map->print();

    TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(mutoo_node,"TMutStubMap");    
    stub_map->print();

    // Write the event
    //
    if(road_map->size()) {
      PHMapManager::write();    
      MUTOO::PRINT(std::cout,"Event Number");
      std::cout << ievent << std::endl;
      MUTOO::PRINT(std::cout,"**");
      dstout->write(dstout_node);
    }
    
    // Draw the event
    //
    if(display_mode) {
      gPhenix->SetRunNum(0);
      gPhenix->event(top_node);  
      gPhenix->Draw();
    }
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  return 0;
}

void setup_display() {

  if(!gClient){
    cout<<"you are running in batch"<<endl;
    return;
  }
  
  // This creates the gPhenix pointer
  //
  new PhenixRun("Phenix-Run","Phenix-Run",top_node);
  
  // Add MUTOO event display
  //
  display = new PhMutooDisplay();
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
}

int setup_all() {

  //  TMutMathieson::disable_stereo();
  
  // Create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");
  
  // Create a node for the output DST
  //
  dstout_node = new PHCompositeNode("DST");
  
  // Create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");
  
  // Instantiate maps
  //  
  TMutMuiRoadMap* road_map = TMutNode<TMutMuiRoadMap>::new_node(mutoo_node,"TMutMuiRoadMap");
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_node(mutoo_node,"TMutHitMap");
  TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_node(mutoo_node,"TMutClusMap");
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap");
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_node(mutoo_node,"TMutGapCoordMap");  
  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_node(mutoo_node, "TMutTrkMap");
  TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_node(mutoo_node, "TMutStubMap");
  // Choose which maps to write out here
  //
  road_map->make_persistent(dstout_node,"TMutMuiRoad");
  hit_map->make_persistent(dstout_node,"TMutHit");
  clus_map->make_persistent(dstout_node,"TMutClus");
  coord_map->make_persistent(dstout_node,"TMutCoord");
  gap_coord_map->make_persistent(dstout_node,"TMutGapCoord");
  trk_map->make_persistent(dstout_node,"TMutTrk");
  stub_map->make_persistent(dstout_node,"TMutStub");
  
  // Instantiate modules and runtime parameter tables
  //
  mMutMuiRoadMod = new mMutMuiRoad();
  TMutNode<mMutMuiRoadPar>::new_node(mutoo_node,"mMutMuiRoadPar");  

  mMutUnpackMutDstMod = new mMutUnpackMutDst();
  TMutNode<mMutUnpackMutDstPar>::new_node(mutoo_node,"mMutUnpackMutDstPar");  
  
  mMutFindClusMod = new mMutFindClus();
  TMutNode<mMutFindClusPar>::new_node(mutoo_node,"mMutFindClusPar");  
  
  mMutFitClusMod = new mMutFitClus();
  TMutNode<mMutFitClusPar>::new_node(mutoo_node,"mMutFitClusPar");  
  
  mMutFindGapCoordMod = new mMutFindGapCoord();
  TMutNode<mMutFindGapCoordPar>::new_node(mutoo_node,"mMutFindGapCoordPar");  

  mMutFindStubMod = new mMutFindStub();
  TMutNode<mMutFindStubPar>::new_node(mutoo_node,"mMutFindStubPar");  
  
  
//    mMutStartTracksMod = new mMutStartTracks();
//    TMutNode<mMutStartTracksPar>::new_node(mutoo_node,"mMutStartTracksPar");  

//    mMutTrackFitMod = new mMutTrackFit();
//    TMutNode<mMutTrackFitPar>::new_node(mutoo_node,"mMutTrackFitPar");  
  
//    mMutEvalFrameworkMod = new mMutEvalFramework();
//    TMutNode<mMutEvalFrameworkPar>::new_node(mutoo_node,"mMutEvalFrameworkPar");  
  
  // change default runtime parameters here
  //
  mMutUnpackMutDstPar* unp_par = TMutNode<mMutUnpackMutDstPar>::find_node(mutoo_node,"mMutUnpackMutDstPar");    
  unp_par->set_verbosity(MUTOO::NONE);
  
  mMutFindClusPar* fic_par = TMutNode<mMutFindClusPar>::find_node(mutoo_node,"mMutFindClusPar");  
  fic_par->set_verbosity(MUTOO::NONE);
  
  mMutFitClusPar* ftc_par = TMutNode<mMutFitClusPar>::find_node(mutoo_node,"mMutFitClusPar");  
  ftc_par->set_verbosity(MUTOO::NONE);
  ftc_par->set_fit_type(mMutFitClusPar::MATHIESON);
  
  mMutFindGapCoordPar* fgc_par = TMutNode<mMutFindGapCoordPar>::find_node(mutoo_node,"mMutFindGapCoordPar");  
  fgc_par->set_verbosity(MUTOO::NONE);

  mMutFindStubPar* fsb_par = TMutNode<mMutFindStubPar>::find_node(mutoo_node,"mMutFindStubPar");  
  fsb_par->set_verbosity(MUTOO::NONE);
   

  //    mMutStartTracksPar* str_par = TMutNode<mMutStartTracksPar>::find_node(mutoo_node,"mMutStartTracksPar");  
//    str_par->set_verbosity(MUTOO::NONE);
//    str_par->set_muid_mode(mMutStartTracksPar::START_MUTR);  

//    mMutTrackFitPar* tft_par = TMutNode<mMutTrackFitPar>::find_node(mutoo_node,"mMutTrackFitPar");  
//    tft_par->set_verbosity(MUTOO::NONE);
//    tft_par->set_residual_mode(mMutTrackFitPar::NORMAL);
  
//    mMutEvalFrameworkPar* evf_par = TMutNode<mMutEvalFrameworkPar>::find_node(mutoo_node,"mMutEvalFrameworkPar");  
//    evf_par->set_eval_calibrate(false);
  
  init_done=true;

  return 0;
}

int end_all() {
  MUTOO::TRACE("in end all");
  delete dstout;
  MUTOO::TRACE("terminated normally");
  return 0;
}

void draw_plane(UShort_t arm, UShort_t octant){
  display->paint_plane_view(arm,octant);
}

int
dstout_fopen(PHString dstout_filename)
{
  dstout = new PHNodeIOManager(dstout_filename, PHWrite);
  dstout->SetCompressionLevel(3);
  return 0;
}












