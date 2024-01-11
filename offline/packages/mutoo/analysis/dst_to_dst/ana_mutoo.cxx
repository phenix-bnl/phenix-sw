/*! 
  @defgroup analysis Analysis
  Analysis Event Loops. Routines in this packages contain pdst/ezdst style
  event loops for iterating throught MUTOO DST output.
*/

/*! ingroup analysis */
/*! \file ana_dst.cxx 
  Prototype pdst type analysis routine.  It demos how to access the 
  data in MUTOO Interface Object Containers from the disk resident DST.  
  This a a shameless hack some code Chris wrote.
*/

#include <iostream>
#include <sstream>
#include "ana_mutoo.h"
#include "pdst.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "dMuoTracksOut.h"
#include "PHTypedNodeIterator.h"
#include "TMutLocalDBInit.h"
#include "TMutNode.h"
#include "TMutMuiRoadMap.h"
#include "TMutHitMap.h"
#include "TMutClusMap.h"
#include "TMutCoordMap.h"
#include "TMutGapCoordMap.h"
#include "TMutTrkMap.h"
#include "TMutMCHitMap.h"
#include "TMutMCTrkMap.h"
#include "TMutEvalMap.h"
#include "TMutVtxMap.h"
#include "EventHeader.h"
#include "mMutdbInit.hh"
#include "mMutInvMassP.h"

#include "TrigLvl1.h"
#include "TrigRunLvl1.h"
#include "TrigRunLvl2.h"

#include "TSystem.h"
#include "PhenixRun.hh"
#include "PhCanvas.hh"
#include "PhDchDisplay.hh"
#include "PhPadDisplay.hh"
#include "PhTecDisplay.hh"
#include "PhMuiDisplay.hh"
#include "PhMutDisplay.hh"
#include "PhEventDisplay.hh"
#include "PhMutooDisplay.h"

#include "TriggerHelper.h"
#include "utiCentrality.h"

#include "MWGInclusiveNanoCutsv1.h"
#include "MuonNanoDSTfuncs.h"
#include <cassert>

#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#include "TH1.h"

typedef PHIODataNode<BbcOut> BbcOutNode_t;

TFile*   ana_file;
TNtuple* nt1;
TNtuple* Dimuons;
TNtuple* Event;
TNtuple* TrackOld;
TNtuple* TrackOldHits;
TNtuple* TrackNew;
TNtuple* TrackNewHits;
TH1F* prox_h;
PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
PHCompositeNode* ndst_node;

static bool init_done = false;
PhMutooDisplay *display = 0;
PhMutooDisplay *south = 0;
PhMutooDisplay *north = 0;

// MUTOO module pointers
//
mMutMuiRoad* mMutMuiRoadMod;
mMutFindClus* mMutFindClusMod;
mMutFitClus* mMutFitClusMod;
mMutFindGapCoord* mMutFindGapCoordMod;
mMutFindStub* mMutFindStubMod;
mMutStubFit* mMutStubFitMod;
mMutStartTracks* mMutStartTracksMod;
mMutFindTrack* mMutFindTrackMod;
mMutTrackFit* mMutTrackFitMod;
mMutKalFit* mMutKalFitMod;
mMutKalVtx* mMutKalVtxMod;
mMutRejectTrack* mMutRejectTrackMod;
mMutFindVtx* mMutFindVtxMod;
mMutFitVtx* mMutFitVtxMod;

// Nano DST
// 
PHInclusiveNanoCuts *theNanoCutter = new MWGInclusiveNanoCutsv1();

// IO manager for output DST file
//
PHNodeIOManager *dstout = 0;
PHNodeIOManager *ndstout= 0;
PHCompositeNode *dstout_node=0;

enum Mode {ALIGN, ALIGN_TRACK, TRACKING, KALMAN, VISUALIZE, FILTER, WINDOWS};    

Mode mode = KALMAN;

static int ievent=0;
static int iwrite=0;
static int imissed=0;

int process_event (PHCompositeNode *dst_node)
{
  try {  

    ++ievent;
    if(ievent%100 == 0) std::cout << "processing event "  << ievent << std::endl;
    if(!init_done) { 
      setup_all(dst_node);
      top_node->addNode(mutoo_node);
      top_node->addNode(dst_node);
      top_node->addNode(ndst_node);
      top_node->print();
      ULong_t default_run = 69522;
      TMutLocalDBInit::initialize(top_node, default_run);
      init_done=true;
      try {
	// Get timestamp from event header
	//
	EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
	
	// This ensures the geometry is picked up with the correct timestamp
	//
	mMutdbInit *mMutdbInitMod = new mMutdbInit();
	mMutdbInitMod->dbGetAll(top_node,evt->get_TimeStamp());
      } catch (std::exception& e){
	std::cout << "No timestamp -- using default geometry" << std::endl;
      }
    }  

    // Event Number
    //
    int event = 0;
    try {
      EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
      event = evt->get_EvtSequence();
      std::cout << "event number: " << event << " index: " << ievent << std::endl;
    } catch (std::exception& e) {
      std::cout << "event number: UNKNOWN" << std::endl;
    }   

    // Clear maps (IOCs) from last event
    //
    PHMapManager::clear();
    
    // This call fills the IOCs from the DST-resident objects
    //
    PHMapManager::read(dst_node);

    // Grab various maps from the node tree so we can dump them at will
    //
    //      TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");	      
    //      TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");	      
    //      TMutClusMap* clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");	      
    //      TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");	      
    TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");	      
    
    if(mode == FILTER && keep_event(event)) {
      PHMapManager::write();      
      dstout->write(dstout_node);	
    }
      
    // If we just want to look then return without running any analysis
    //
    if((mode == VISUALIZE) || (mode == FILTER)) return 0;
    
    //    mMutMuiRoadMod->event(top_node);
    mMutFindClusMod->event(top_node);
    mMutFitClusMod->event(top_node);
    mMutFindGapCoordMod->event(top_node);
    
    if(mode == TRACKING || mode == KALMAN || mode == WINDOWS) {

      mMutFindTrackMod->event(top_node);      
      mMutStubFitMod->event(top_node);

      if(mode == TRACKING) {	  
	mMutTrackFitMod->event(top_node);
  	mMutRejectTrackMod->event(top_node);
  	mMutFindVtxMod->event(top_node);
  	mMutFitVtxMod->event(top_node);	  
      } else {	
	mMutKalFitMod->event(top_node);
	mMutRejectTrackMod->event(top_node);
	mMutFindVtxMod->event(top_node);
	mMutKalVtxMod->event(top_node);
      }
	
//        bool good_event=f;
//        TMutVtxMap::iterator vtx_iter = vtx_map->range();
//        while(TMutVtxMap::pointer vtx_ptr = vtx_iter.next()){
//  	if(vtx_ptr->get()->get_mass() < 3.0 ||
//  	   vtx_ptr->get()->get_mass() > 3.4) {
//  	  good_event = true;
//  	}
//        }
      
      bool good_event=true;
//      bool good_event=false;
//      if(vtx_map->size() != 0) good_event=true;

      if(good_event) {	

	// output Ntuple and DST
	//
	if(mode == WINDOWS) {
	  fill_windows_ntuple();
	} else {
	  fill_ntuple(event, top_node);
	}
	PHMapManager::write();
	dstout->write(dstout_node);
	process_MuonNanoDST(top_node, theNanoCutter);

	// output Nano DST.
	//
	PHNodeIterator iter(top_node);
	PHCompositeNode *ndstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "NDST"));
	if(!ndstNode) {
	  cout << " we could not find Nano dst node !!! " <<endl;
	} else {
	  ndstout->write(ndstNode);
	}
	++iwrite;
	std::cout << "wrote " << iwrite << std::endl;
      } else {
	++imissed;
	std::cout << "missed " << imissed << std::endl;
      }
    }
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  return 0;  
}

int setup_all(PHCompositeNode *dst_node) {

  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");

  // Create a node for the output DST
  //
  dstout_node = new PHCompositeNode("DST");

  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");

  // Set up NanoDst node.
  //
  ndst_node = new PHCompositeNode("NDST");

  // Initialize output ntuple
  //
  ana_file = new TFile("top.root", "recreate");
  if(mode == WINDOWS) {
    nt1 = new TNtuple("nt1","nt1","theta1:theta2:theta3:phi1:phi2:phi3");
  } else { 
    nt1 = new TNtuple("nt1","nt1","mass:sign:ntrk:nvtx:event");
    Dimuons = new TNtuple("Dimuons","Dimuons","mass:pair_sign:event:o1:o2:px1:py1:pz1:px2:py2:pz2");
    Event = new TNtuple("Event","Event","massold:massnew:nmassold:nmassnew:event");
    TrackOld = new TNtuple("TrackOld","TrackOld","arm:octant:px0:py0:pz0:px1:py1:pz1:px2:py2:pz2:px3:py3:pz3:x0:y0:z0:x1:y1:z1:x2:y2:z2:x3:y3:z3:event:nhits");
    TrackNew = new TNtuple("TrackNew","TrackNew","arm:octant:px0:py0:pz0:px1:py1:pz1:px2:py2:pz2:px3:py3:pz3:x0:y0:z0:x1:y1:z1:x2:y2:z2:x3:y3:z3:event:nhits:success:ghost");
    TrackOldHits = new TNtuple("TrackOldHits","TrackOldHits","event:station:gap:cath:octant:itrk");
    TrackNewHits = new TNtuple("TrackNewHits","TrackNewHits","event:station:gap:cath:octant:itrk");
  }

  // Initialize nano-dst output
  //
  init_MuonNanoDST(ndst_node);
  theNanoCutter->Initialize(top_node);
  

  try {
    // Read TMutHit and TMutMuiRoad from input DST
    //
    TMutMuiRoadMap* road_map = 
      TMutNode<TMutMuiRoadMap>::new_dst_input_node(mutoo_node,"TMutMuiRoadMap",dst_node,"TMutMuiRoad");

    TMutHitMap* hit_map = 
      TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap", dst_node,"TMutHit" );
    
    // If we are doing reco then instantiate new IOCs if not the just read all from DST node
    //
    if(mode != VISUALIZE && mode != FILTER) {
      
      TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_node(mutoo_node,"TMutClusMap");
      TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap");
      TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_node(mutoo_node,"TMutGapCoordMap");  
      TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_node(mutoo_node, "TMutStubMap");
      TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_node(mutoo_node, "TMutTrkMap");
      TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::new_node(mutoo_node,"TMutVtxMap"); 
      
      road_map->make_persistant(dstout_node, "TMutMuiRoad");    
      hit_map->make_persistant(dstout_node, "TMutHit");    
      coord_map->make_persistant(dstout_node, "TMutCoord");    
      gap_coord_map->make_persistant(dstout_node, "TMutGapCoord");
      stub_map->make_persistant(dstout_node, "TMutStub");
      clus_map->make_persistant(dstout_node, "TMutClus");
      trk_map->make_persistant(dstout_node, "TMutTrk");
      vtx_map->make_persistant(dstout_node, "TMutVtx");

    } else {

      TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_dst_input_node(mutoo_node,"TMutClusMap", dst_node, "TMutClus");      
      TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_dst_input_node(mutoo_node, "TMutCoordMap",dst_node, "TMutCoord");
      TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_dst_input_node(mutoo_node,"TMutGapCoordMap",dst_node,"TMutGapCoord");  
      TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_dst_input_node(mutoo_node, "TMutStubMap",dst_node, "TMutStub");
      TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_dst_input_node(mutoo_node, "TMutTrkMap",dst_node, "TMutTrk");
      TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::new_dst_input_node(mutoo_node, "TMutVtxMap",dst_node, "TMutVtx");

      road_map->make_persistant(dstout_node, "TMutMuiRoad");    
      hit_map->make_persistant(dstout_node, "TMutHit");    
      clus_map->make_persistant(dstout_node, "TMutClus");
      coord_map->make_persistant(dstout_node, "TMutCoord");    
      gap_coord_map->make_persistant(dstout_node, "TMutGapCoord");
      stub_map->make_persistant(dstout_node, "TMutStub");
      trk_map->make_persistant(dstout_node, "TMutTrk");
      vtx_map->make_persistant(dstout_node, "TMutVtx");
    }

    // Instantiate modules and runtime parameter tables
    //
    mMutFindClusMod = new mMutFindClus();
    TMutNode<mMutFindClusPar>::new_node(mutoo_node,"mMutFindClusPar");  
    
    mMutFitClusMod = new mMutFitClus();
    TMutNode<mMutFitClusPar>::new_node(mutoo_node,"mMutFitClusPar");  
    
    mMutFindGapCoordMod = new mMutFindGapCoord();
    TMutNode<mMutFindGapCoordPar>::new_node(mutoo_node,"mMutFindGapCoordPar");  

    mMutMuiRoadMod = new mMutMuiRoad();
    TMutNode<mMutMuiRoadPar>::new_node(mutoo_node,"mMutMuiRoadPar");  

    mMutFindStubMod = new mMutFindStub();
    TMutNode<mMutFindStubPar>::new_node(mutoo_node,"mMutFindStubPar");    
    
    mMutStubFitMod = new mMutStubFit();
    TMutNode<mMutStubFitPar>::new_node(mutoo_node,"mMutStubFitPar");

    mMutStartTracksMod = new mMutStartTracks();
    TMutNode<mMutStartTracksPar>::new_node(mutoo_node,"mMutStartTracksPar");  

    mMutFindTrackMod = new mMutFindTrack();
    TMutNode<mMutFindTrackPar>::new_node(mutoo_node,"mMutFindTrackPar");  

    mMutTrackFitMod = new mMutTrackFit();
    TMutNode<mMutTrackFitPar>::new_node(mutoo_node,"mMutTrackFitPar");  

    mMutKalFitMod = new mMutKalFit();
    TMutNode<mMutKalFitPar>::new_node(mutoo_node,"mMutKalFitPar");  

    mMutKalVtxMod = new mMutKalVtx();
    TMutNode<mMutKalVtxPar>::new_node(mutoo_node,"mMutKalVtxPar");  
    
    mMutRejectTrackMod = new mMutRejectTrack();
    TMutNode<mMutRejectTrackPar>::new_node(mutoo_node,"mMutRejectTrackPar");  
    
    mMutFindVtxMod = new mMutFindVtx();
    TMutNode<mMutFindVtxPar>::new_node(mutoo_node,"mMutFindVtxPar");

    mMutFitVtxMod = new mMutFitVtx();
    TMutNode<mMutFitVtxPar>::new_node(mutoo_node,"mMutFitVtxPar");

    // change default runtime parameters here
    //
    mMutFindClusPar* fic_par = TMutNode<mMutFindClusPar>::find_node(mutoo_node,"mMutFindClusPar");  
    fic_par->set_verbosity(MUTOO::SOME);
    
    mMutFitClusPar* ftc_par = TMutNode<mMutFitClusPar>::find_node(mutoo_node,"mMutFitClusPar");  
    ftc_par->set_verbosity(MUTOO::SOME);
    ftc_par->set_fit_type(mMutFitClusPar::MATHIESON);

    mMutFindGapCoordPar* fgc_par = TMutNode<mMutFindGapCoordPar>::find_node(mutoo_node,"mMutFindGapCoordPar");  
    fgc_par->set_verbosity(MUTOO::SOME);

    mMutFindStubPar* fis_par = TMutNode<mMutFindStubPar>::find_node(mutoo_node,"mMutFindStubPar");  
    fis_par->set_verbosity(MUTOO::SOME);
    
    mMutStubFitPar* fts_par = TMutNode<mMutStubFitPar>::find_node(mutoo_node,"mMutStubFitPar");  
    fts_par->set_verbosity(MUTOO::SOME);
    fts_par->set_residual_mode(mMutStubFitPar::NORMAL);
        
    mMutFindTrackPar* ftr_par = TMutNode<mMutFindTrackPar>::find_node(mutoo_node,"mMutFindTrackPar");  
    ftr_par->set_verbosity(MUTOO::ALOT);
    ftr_par->set_mode(mMutFindTrackPar::NO_MUID);

    mMutTrackFitPar* tft_par = TMutNode<mMutTrackFitPar>::find_node(mutoo_node,"mMutTrackFitPar");  
    tft_par->set_verbosity(MUTOO::SOME);
    tft_par->set_residual_mode(mMutTrackFitPar::NORMAL);
    tft_par->set_fit_mode(mMutTrackFitPar::STANDARD);
    tft_par->set_fit_type(mMutTrackFitPar::FIELD_ON);
    tft_par->set_max_iterations(100);
    
    mMutFitVtxPar* fiv_par = TMutNode<mMutFitVtxPar>::find_node(mutoo_node,"mMutFitVtxPar");  
    fiv_par->set_verbosity(MUTOO::SOME);
    fiv_par->set_max_iterations(100);

    mMutKalVtxPar* kfv_par = TMutNode<mMutKalVtxPar>::find_node(mutoo_node,"mMutKalVtxPar");  
    kfv_par->set_verbosity(MUTOO::SOME);
    kfv_par->set_z_vtx_source(mMutKalVtxPar::Z_BBC);
    kfv_par->set_do_track_loop(false);
    
    mMutKalFitPar* kft_par = TMutNode<mMutKalFitPar>::find_node(mutoo_node,"mMutKalFitPar");  
    kft_par->set_verbosity(MUTOO::SOME);
    kft_par->set_z_vtx_source(mMutKalFitPar::Z_BBC);

    // Grab the EventHeader node from the node tree and add it to the new DST node
    //
    PHNodeIterator iter(dst_node);
    PHIODataNode<EventHeader>* evt_hdr_node = 
      static_cast< PHIODataNode<EventHeader>* >(iter.findFirst("PHIODataNode","EventHeader"));
    if(evt_hdr_node) {
      dstout_node->addNode(evt_hdr_node);
    }

    PHTypedNodeIterator<BbcOut> bbc(dst_node);
    BbcOutNode_t *bbc_node = bbc.find("BbcOut");
    if(bbc_node) {
      dstout_node->addNode(bbc_node);
    }

    TMutStubFinder::set_verbose(false);

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  init_done = true;
  std::cout<<"Haven't crashed yet\n" << std::endl;
  return 0;
};

bool keep_event(int event_number)
{
  if(event_number == 1189492) return true;
  /***
      static std::set<int> keep_events = initialize_keep_event();
  
      // Check event set to see if we are keeping this event
      //
      if(keep_events.find(event_number) != keep_events.end()) return true;
  ***/
  return false;
}

void initialize_keep_event()
{
//    MUTOO::PRINT(std::cout, "Filter Event List");
//    char buffer[256];
  /***
      ifstream event_list("event_list.txt");
      std::set<int> event_number_set;
      do {
      event_list.getline(buffer,256,'\n');
      istringstream line(buffer);    
      int event_number = 0;
      line >> event_number;  
      event_number_set.insert(event_number);
      std::cout << event_number << ":";
      } while(!event_list.fail());
      std::cout << std::endl;
      MUTOO::PRINT(std::cout, "**");
      return event_number_set;
  ***/
}

int end_all() {
  cout<<"Writing output histograms (goodbye)\n";
  ana_file->Write();
  cout<<"  --> Root output file created.\n";
  delete theNanoCutter;
  delete dstout;
  delete ndstout;
  return 0;
}

bool setup_display() {
    
  display = new PhMutooDisplay();
  display->set_draw_tracks(true);
  display->set_draw_stubs(false);
  return true;
}

void draw_plane(int arm, int octant){
  if(arm<0){
    cout<<" Usage: draw_plane(int arm, int octant)\n";
    return; }
  do_display();
  display->paint_plane_view(arm,octant);
}

void draw_octant(int arm, int octant, int station){
  if(arm<0){
    cout<<" Usage: draw_octant(int arm, int octant, int station)\n";
    return; }
  do_display();
  display->paint_octant_view(arm, octant, station);
}

void draw_side(int arm, int octant, int station){
  if(arm<0){
    cout<<" Usage: draw_side(int arm, int octant, int station)\n";
    return; }
  do_display();
  display->paint_side_view(arm, octant, station);
}

int
dstout_fopen(PHString dstout_filename, PHString ndstout_filename = "ndst.root")
{
  dstout = new PHNodeIOManager(dstout_filename, PHWrite);
  ndstout = new PHNodeIOManager(ndstout_filename, PHWrite);
  dstout->SetCompressionLevel(3);
  return 0;
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
    TMutStubMap::iterator stub_iter = map->range();
    while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
      stub_ptr->get()->print();
    }
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
}

void dump_road() {
  try {
    TMutMuiRoadMap* map = TMutNode<TMutMuiRoadMap>::find_node(mutoo_node, "TMutMuiRoadMap");
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

void do_display(){  
  static bool display_mode = setup_display();
  display->event(top_node);
}

void fill_ntuple(int event, PHCompositeNode* top_node) {
  
  int i, j;
  float p1[3], p2[3];
  
  // Get Track node and vtx node from node tree.
  //
  TMutTrkMap*  trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");
  TMutVtxMap*  vtx_map = TMutNode<TMutVtxMap>::find_node(mutoo_node,"TMutVtxMap");
  
  typedef PHIODataNode <dMuoTracksOut> dMuoTracksOutNode_t;

  dMuoTracksOut* pMuoTracksOut=0;

  PHIODataNode<dMuoTracksOut>::iterator MuoTracksIter(top_node);
  dMuoTracksOutNode_t *dMuoTracksOutNode = MuoTracksIter.find("dMuoTracksOut");
  if (dMuoTracksOutNode) {
    pMuoTracksOut = dMuoTracksOutNode->getData();
  }
  else{
    std::cout << "dMuoTracksOut not find in tree" << std::endl;
  }

  // Book an arry for containing the varibles which will be filled in ntuple.
  //
  float nt_vars[100] = {0};
  float nt_vars2[100] = {-999};
  float nt_vars3[100] = {-999};
  float nt_vars4[100] = {-999};
  float nt_vars5[100] = {-999};
  float nt_vars6[100] = {-999};
  float nt_vars7[100] = {-999};
  
  // Loop over TMutTrkMap.
  //
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  int NumNew = 0;
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next()){
    NumNew++;
    nt_vars[0] = vtx_ptr->get()->get_mass();
    nt_vars[1] = vtx_ptr->get()->get_sign();
    nt_vars[2] = trk_map->size();
    nt_vars[3] = vtx_map->size(); 
    nt_vars[4] = (float)event;
    nt1->Fill(nt_vars);
  }  

  for (i = 0; i<pMuoTracksOut->RowCount(); i++){
    nt_vars3[0] = pMuoTracksOut->get_arm(i);
    nt_vars3[1] = pMuoTracksOut->get_octant(0,i);
    p1[0] = pMuoTracksOut->get_px(0,i);
    p1[1] = pMuoTracksOut->get_py(0,i);
    p1[2] = pMuoTracksOut->get_pz(0,i);
    for (j = 0; j<4; j++){
      nt_vars3[2+j*3] = pMuoTracksOut->get_px(j,i);
      nt_vars3[3+j*3] = pMuoTracksOut->get_py(j,i);
      nt_vars3[4+j*3] = pMuoTracksOut->get_pz(j,i);
      nt_vars3[14+j*3] = pMuoTracksOut->get_x(j,i);
      nt_vars3[15+j*3] = pMuoTracksOut->get_y(j,i);
      nt_vars3[16+j*3] = pMuoTracksOut->get_z(j,i);
    }
    nt_vars3[26] = (float)event;
    nt_vars3[27] = (float)pMuoTracksOut->get_nhits(i);
    TrackOld->Fill(nt_vars3);

    for (j = 0; j<27; j++){
      if (pMuoTracksOut->get_signaltype(j,i)){
        nt_vars4[0] = (float)event;
        nt_vars4[1] = (float)pMuoTracksOut->get_station(j,i);
        nt_vars4[2] = (float)pMuoTracksOut->get_gap(j,i);
        nt_vars4[3] = (float)pMuoTracksOut->get_cath(j,i);
        nt_vars4[4] = (float)pMuoTracksOut->get_octant(j,i);
        nt_vars4[5] = (float)i;
        TrackOldHits->Fill(nt_vars4);
      }
    }

  
    for (j = i+1; j<pMuoTracksOut->RowCount(); j++){
      
      p2[0] = pMuoTracksOut->get_px(0,j);
      p2[1] = pMuoTracksOut->get_py(0,j);
      p2[2] = pMuoTracksOut->get_pz(0,j);

      nt_vars2[0]= mMutInvMass(p1,p2);
      nt_vars2[1]= (float)(pMuoTracksOut->get_charge(j) + pMuoTracksOut->get_charge(i));
      nt_vars2[2]= (float)event;
      nt_vars2[3] = p1[0];
      nt_vars2[4] = p1[1];
      nt_vars2[5] = p1[2];
      nt_vars2[6] = p2[0];
      nt_vars2[7] = p2[1];
      nt_vars2[8] = p2[2];

      Dimuons->Fill(nt_vars2); 
    }

  }

  int NumOld = 0;
  for (j = 1; j<pMuoTracksOut->RowCount(); j++) NumOld += pMuoTracksOut->RowCount()-j;

  nt_vars7[0] = nt_vars2[0];
  nt_vars7[1] = nt_vars[0];
  nt_vars7[2] = (float)NumOld;
  nt_vars7[3] = (float)NumNew;
  nt_vars7[4] = (float)event; 

  Event->Fill(nt_vars7); 

  int itrack = 0;
  TMutTrkMap::const_iterator trk_iter = trk_map->range();

  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){

    for (j=0; j<15; j++) nt_vars5[j] = -999;
    nt_vars5[0] = (float)trk_ptr->get()->get_arm();
    nt_vars5[1] = (float)trk_ptr->get()->get_octant();
    nt_vars5[2] = trk_ptr->get()->get_trk_par_vtx()->get_px();
    nt_vars5[3] = trk_ptr->get()->get_trk_par_vtx()->get_py();
    nt_vars5[4] = trk_ptr->get()->get_trk_par_vtx()->get_pz();
    nt_vars5[14] = trk_ptr->get()->get_trk_par_vtx()->get_x();
    nt_vars5[15] = trk_ptr->get()->get_trk_par_vtx()->get_y();
    nt_vars5[16] = trk_ptr->get()->get_trk_par_vtx()->get_z();
    nt_vars5[26] = (float)event;

    int nhits = 0;
    TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
    while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
      nhits++;
      nt_vars6[0] = (float)event;
      nt_vars6[1] = (float)coord_ptr->get()->get_station();
      nt_vars6[2] = (float)coord_ptr->get()->get_gap();
      nt_vars6[3] = (float)coord_ptr->get()->get_cathode();
      nt_vars6[4] = (float)coord_ptr->get()->get_octant();
      nt_vars6[5] = (float)itrack;
      TrackNewHits->Fill(nt_vars6);
    }

    nt_vars5[27] = (float)nhits; 
    nt_vars5[28] = (float)trk_ptr->get()->get_reco_success();
    nt_vars5[29] = (float)trk_ptr->get()->get_ghost();
    TrackNew->Fill(nt_vars5); 

    if (itrack<= 1){
      nt_vars2[itrack + 3] = trk_ptr->get()->get_octant();
      nt_vars2[itrack*3 + 5] = trk_ptr->get()->get_trk_par_vtx()->get_px();
      nt_vars2[itrack*3 + 6] = trk_ptr->get()->get_trk_par_vtx()->get_py();
      nt_vars2[itrack*3 + 7] = trk_ptr->get()->get_trk_par_vtx()->get_pz();
    }
    itrack++;
  }














}

void fill_windows_ntuple() {
  
  // Get Track node and vtx node from node tree.
  //
  TMutVtxMap*  vtx_map = TMutNode<TMutVtxMap>::find_node(mutoo_node,"TMutVtxMap");
  
  // Book an arry for containing the varibles which will be filled in ntuple.
  //
  float nt_vars[100] = {0};
  
  // Loop over TMutTrkMap.
  //
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next()){
    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){

      TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
      TMutStub* stubs[3] = {0};
      while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()) {	
	stubs[stub_ptr->get()->get_station()] = stub_ptr->get();
      }

      double theta1 = std::fabs(stubs[0]->get_theta());
      double theta2 = std::fabs(stubs[1]->get_theta());
      double theta3 = std::fabs(stubs[2]->get_theta());
      double phi1 = std::fabs(stubs[0]->get_phi());
      double phi2 = std::fabs(stubs[1]->get_phi());
      double phi3 = std::fabs(stubs[2]->get_phi());

      nt_vars[0] = theta1;
      nt_vars[1] = theta2;
      nt_vars[2] = theta3;
      nt_vars[3] = phi1;
      nt_vars[4] = phi2;
      nt_vars[5] = phi3;
      nt1->Fill(nt_vars);      
    }  
  }
}












