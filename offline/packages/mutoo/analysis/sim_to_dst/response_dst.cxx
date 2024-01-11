/*! ingroup analysis */
/*! \file mutoo_dst.cxx 

*/

#include <iostream>
#include "response_dst.h"
#include "recoConsts.h"
#include "MUTOO.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "PHNodeIterator.h"
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
#include "RunHeader.h"
#include "RunToTime.hh"

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
mMutFindTrack* mMutFindTrackMod;
mMutTrackFit* mMutTrackFitMod;
mMutFindVtx* mMutFindVtxMod;
mMutFitVtx* mMutFitVtxMod;
mMutKalFit* mMutKalFitMod;
mMutResponse* mMutResponseMod;
mMutEval* mMutEvalMod;

// IO manager for output DST file
//
PHNodeIOManager *dstout = 0;
PHCompositeNode *dstout_node=0;
PhMutooDisplay *display = 0;

// Ntuple output
//
TFile*   ana_file;
TNtuple* nt1;
TNtuple* nt2;

enum Mode {ALIGN, EFFIC, TRACKING, RESP};
Mode mode = RESP;

int process_event (PHCompositeNode *dst)
{
  
  try {
    if(!init_done) { 
      setup_all(dst);
      top_node->addNode(mutoo_node);
      top_node->addNode(dst);
      top_node->print();
      init_done=true;
    }
    
    // Clear first for event display
    //
    PHMapManager::clear();
    
    // Fills IOCs coupled to the DST node
    //
    PHMapManager::read(dst);

    // Reconstruction modules
    //
    if(mode != RESP) {
      mMutResponseMod->event(top_node);
      mMutFindClusMod->event(top_node);
      mMutFitClusMod->event(top_node);
      mMutFindGapCoordMod->event(top_node);
    }

    if(mode == ALIGN) {

      // Run stub finder and fitter
      //
      mMutFindStubMod->event(top_node);
      mMutStubFitMod->event(top_node);

    } else if(mode == EFFIC) {
      
      mMutFindTrackMod->event(top_node);
      mMutEvalMod->event(top_node);

      TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(mutoo_node,"TMutMCTrkMap");          
      TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();      
      while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){
	
	TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();	
	std::set<int> octant_set;
	while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){
	  octant_set.insert(mc_hit_ptr->get()->get_octant());
	}
	if(octant_set.size() != 1) continue;

	float ntvar[10] = {0};
	ntvar[0] = mc_trk_ptr->get()->get_arm();
	ntvar[1] = mc_trk_ptr->get()->get_charge();
	ntvar[2] = mc_trk_ptr->get()->get_ptot_us_gap();
	
	// Get the associated TMutTrk
	//
	TMutTrkMap::key_iterator trk_iter = mc_trk_ptr->get()->get_associated<TMutTrk>();
	if(!trk_iter.at_end()) {
	  // Get the associated TMutEval
	  //
	  TMutEvalMap::key_iterator eval_iter = trk_iter->get()->get_associated<TMutEval>();
	  if(eval_iter.at_end()){
	    MUTOO::TRACE("Error -- associated TMutMCTrk with no eval object ??");
	  } else {
	    ntvar[3] = 1; 
	    if(trk_iter->get()->get_status() == TMutTrk::GLOBAL_FIT) {
	      ntvar[4] = trk_iter->get()->get_charge();
	      ntvar[5] = trk_iter->get()->get_trk_par()->get_ptot();
	    } else {
	      ntvar[4] = trk_iter->get()->get_charge();
	      ntvar[5] = 0;
	    }
	    ntvar[6] = eval_iter->get()->get_trk_eval()->get_n_reco_true_hits();
	    ntvar[7] = eval_iter->get()->get_trk_eval()->get_n_total_true_hits();	    
	  }
	}
	nt1->Fill(ntvar);
	if(ntvar[6] != ntvar[7] && ntvar[2]>2.0){
	  PHMapManager::write();    
	  dstout->write(dstout_node);
	}
	return 0;
      }
      
    } else if(mode == RESP) {
      
      TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(mutoo_node,"TMutMCTrkMap");          
      TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();      
      while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){
	
	TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();	
	while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){


	  float ntvar[23];
	  for(int ind=0; ind<23; ind++) ntvar[ind]=-10.0;
	  ntvar[0] = mc_hit_ptr->get()->get_arm();
	  ntvar[1] = mc_hit_ptr->get()->get_station();
	  ntvar[2] = mc_hit_ptr->get()->get_octant();
	  ntvar[3] = mc_hit_ptr->get()->get_half_octant();
	  ntvar[4] = mc_hit_ptr->get()->get_gap();
	  ntvar[5] = mc_hit_ptr->get()->get_track_id();
	  ntvar[6] = mc_hit_ptr->get()->get_index();
	  TMutHitMap::key_iterator hit_iter = mc_hit_ptr->get()->get_associated<TMutHit>();
	  ntvar[7] = hit_iter.count();

	  const TMutMCHit::strip_list *mc_strip_list = mc_hit_ptr->get()->get_strip_list(); 
	  TMutMCHit::strip_iterator mc_strip_iter = mc_strip_list->begin();
	  float qpeak1 = -10.0;
	  float qpeak2 = -10.0;
	  int i=0;
	  int j=0;
	  while(mc_strip_iter != mc_strip_list->end()) {
       	    if(mc_strip_iter->get_cathode()==Cathode_1) {
	      if(mc_strip_iter->get_q()>qpeak1) qpeak1 = mc_strip_iter->get_q();
	      if(i<3) {
		ntvar[8+i] = mc_strip_iter->get_strip();
		ntvar[14+i] = mc_strip_iter->get_q();
	      }
	      i++;
	    }
       	    if(mc_strip_iter->get_cathode()==Cathode_2) {
	      if(mc_strip_iter->get_q()>qpeak2) qpeak2 = mc_strip_iter->get_q();
	      if(j<3) {
		ntvar[11+j] = mc_strip_iter->get_strip();
		ntvar[17+j] = mc_strip_iter->get_q();
	      }
	      j++;
	    }
	    mc_strip_iter++; 
	  }
	  ntvar[20] = qpeak1;
	  ntvar[21] = qpeak2;
	  ntvar[22] = mc_hit_ptr->get()->get_eloss();
	  nt1->Fill(ntvar);
	}	
      }

      TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(mutoo_node,"TMutHitMap");          
      TMutHitMap::iterator hit_iter = hit_map->range();      
      while(TMutHitMap::pointer hit_ptr = hit_iter.next()){
	float ntvar[12] = {-1.0};
	ntvar[0] = hit_ptr->get()->get_arm();
	ntvar[1] = hit_ptr->get()->get_station();
	ntvar[2] = hit_ptr->get()->get_octant();
	ntvar[3] = hit_ptr->get()->get_half_octant();
	ntvar[4] = hit_ptr->get()->get_gap();
	ntvar[5] = hit_ptr->get()->get_cathode();
	ntvar[6] = hit_ptr->get()->get_strip();
	ntvar[7] = hit_ptr->get()->get_q();
	ntvar[8] = hit_ptr->get()->get_adc(0);
	ntvar[9] = hit_ptr->get()->get_adc(1);
	ntvar[10] = hit_ptr->get()->get_adc(2);
	ntvar[11] = hit_ptr->get()->get_adc(3);
	nt2->Fill(ntvar);
      }

    } else if(mode == TRACKING) {

      // Tracking with real PR
      //
      mMutFindTrackMod->event(top_node);

      // Storage for output ntuple
      //
      float nt_var[2][100] = {{0}};

      // Vertex find
      //
      mMutFindVtxMod->event(top_node);      

      // Cache the pre vertex fit mass
      //
      TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(mutoo_node,"TMutVtxMap");              
      if(vtx_map->size()){
	TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
	if(!vtx_iter.at_end()) {
	  nt_var[0][0] = vtx_iter->get()->get_mass();
	  nt_var[1][0] = vtx_iter->get()->get_mass();
	}	
      }

      // Vertex fit
      //
      //      mMutFitVtxMod->event(top_node);      

      // cache the post vertex fit mass
      //
//        TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
//        if(!vtx_iter.at_end()) {
//  	nt_var[0][1] = vtx_iter->get()->get_mass();
//  	nt_var[1][1] = vtx_iter->get()->get_mass();
//        }	

      // Evaluation module
      //
      mMutEvalMod->event(top_node);

      // Fill the ntuple from the TMutEval interface object
      //
      TMutEvalMap* eval_map = TMutNode<TMutEvalMap>::find_node(mutoo_node,"TMutEvalMap");              
      TMutEvalMap::const_iterator eval_iter = eval_map->range();
      UShort_t which_track=0;
      while(TMutEvalMap::const_pointer eval_ptr = eval_iter.next()){
	if(which_track > 1) break;
	// Per track evaluation
	//
	const TMutTrkEval* trk_eval_ptr = eval_ptr->get()->get_trk_eval();
	nt_var[which_track][2] = trk_eval_ptr->get_px_true_vx();
	nt_var[which_track][3] = trk_eval_ptr->get_py_true_vx();
	nt_var[which_track][4] = trk_eval_ptr->get_pz_true_vx();
	nt_var[which_track][5] = trk_eval_ptr->get_px_reco_vx();
	nt_var[which_track][6] = trk_eval_ptr->get_py_reco_vx();
	nt_var[which_track][7] = trk_eval_ptr->get_pz_reco_vx();
	nt_var[which_track][8] = trk_eval_ptr->get_px_true_us();
	nt_var[which_track][9] = trk_eval_ptr->get_py_true_us();
	nt_var[which_track][10] = trk_eval_ptr->get_pz_true_us();
	nt_var[which_track][11] = trk_eval_ptr->get_px_reco_us();
	nt_var[which_track][12] = trk_eval_ptr->get_py_reco_us();
	nt_var[which_track][13] = trk_eval_ptr->get_pz_reco_us();
	nt_var[which_track][14] = eval_ptr->get()->get_arm();
	nt_var[which_track][15] = eval_ptr->get()->get_octant();
	nt_var[which_track][16] = trk_eval_ptr->get_ptot_true_vx();
	nt_var[which_track][17] = trk_eval_ptr->get_ptot_reco_vx();
	nt_var[which_track][18] = trk_eval_ptr->get_ptot_true_us();
	nt_var[which_track][19] = trk_eval_ptr->get_ptot_reco_us();
	nt_var[which_track][20] = trk_eval_ptr->get_ptot_true_ds();
	++which_track;
      }      
      // Fill the ntuple
      //
      nt1->Fill(nt_var[0]);
      nt1->Fill(nt_var[1]);
    } 

    // Write the output DST
    //
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");          
    if(trk_map->size()==2){
      PHMapManager::write();    
      dstout->write(dstout_node);
    }

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }

  return 0;
}

int vertex_fit_and_eval(PHCompositeNode* dst_node) {
  return 0;
}

int setup_all(PHCompositeNode* dst_node) {

  ana_file = new TFile("mutoo_ntuple.root","recreate");

if(mode == EFFIC) {

    nt1 = new TNtuple("nt1","nt1","arm:charge:ptotus_true:found:charge_reco:ptotus_reco:reco_hits:true_hits");		      

  } else if(mode == RESP) {
    
    nt1 = new TNtuple("nt1","nt1","arm:station:octant:halfoctant:gap:mctrack:mchit:"
		      "numstrips:strip1_cath1:strip2_cath1:strip3_cath1:"
		      "strip1_cath2:strip2_cath2:strip3_cath2:q1_cath1:q2_cath1:"
		      "q3_cath1:q1_cath2:q2_cath2:q3_cath2:qpeak_cath1:qpeak_cath2:eloss");    
    nt2 = new TNtuple("nt2","nt2","arm:station:octant:halfoctant:gap:cathode:strip:q:adc0:adc1:adc2:adc3");
    
  } else {

    nt1 = new TNtuple("nt1","nt1","mass_find:mass_fit:pxvt:pyvt:pzvt:pxvr:pyvr:pzvr"
		      ":pxust:pyust:pzust:pxusr:pyusr:pzusr:arm:octant:ptotvt:ptotvr"
		      ":ptotust:ptotusr:ptotdst");

  } 

  //Get RunHeader run number and time stamp
  //PHCompositeNode *runNode = new PHCompositeNode("RUN");
  PHTypedNodeIterator<RunHeader> iRunHeader(dst_node);
  PHIODataNode<RunHeader>* nRunHeader = iRunHeader.find("RunHeader");
  recoConsts *rc = recoConsts::instance();
  if(nRunHeader>0) {
    //get timestamp into recoConsts
    int runNumber = (*iRunHeader).get_RunNumber();
    RunToTime *runTime = RunToTime::instance();
    PHTimeStamp *beginTime = runTime->getBeginTime(abs(runNumber));
    rc->set_TimeStamp(*beginTime);
    //runNode->addNode(nRunHeader);
  } else {
    std::cout<<"NO RUN NODE! Kluging time! "<<endl;
    PHTimeStamp beginTime(2003,3,1,0,0,0,0);
    rc->set_TimeStamp(beginTime);
  }

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
  //  TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_node(mutoo_node,"TMutHitMap");
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
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap",
									dst_node, "TMutHit");  
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
  vtx_map->make_persistent(dstout_node,"TMutVtx");

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
  mMutResponsePar* rsp_par = TMutNode<mMutResponsePar>::find_node(mutoo_node,"mMutResponsePar");    
  rsp_par->set_verbosity(MUTOO::NONE);
  
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
  ftr_par->set_dphi(0.2);
  ftr_par->set_dtheta(0.2);
  ftr_par->set_v0_r_cut(1000);
  ftr_par->set_drdz_min(0);
  ftr_par->set_drdz_max(1.0);
  ftr_par->set_min_stub_coord(2);

  mMutFindStubPar* fis_par = TMutNode<mMutFindStubPar>::find_node(mutoo_node,"mMutFindStubPar");  
  fis_par->set_verbosity(MUTOO::NONE);
  fis_par->set_mode(mMutFindStubPar::NO_REQUIRE_GC);

  mMutStubFitPar* fts_par = TMutNode<mMutStubFitPar>::find_node(mutoo_node,"mMutStubFitPar");  
  fts_par->set_verbosity(MUTOO::NONE);
  
  mMutStartTracksPar* str_par = TMutNode<mMutStartTracksPar>::find_node(mutoo_node,"mMutStartTracksPar");      
  str_par->set_verbosity(MUTOO::NONE);
  str_par->set_muid_mode(mMutStartTracksPar::NO_MUID);

  mMutTrackFitPar* tft_par = TMutNode<mMutTrackFitPar>::find_node(mutoo_node,"mMutTrackFitPar");      
  tft_par->set_verbosity(MUTOO::NONE);
  tft_par->set_residual_mode(mMutTrackFitPar::NORMAL);
  tft_par->set_fit_mode(mMutTrackFitPar::STANDARD);  
  tft_par->set_fit_type(mMutTrackFitPar::FIELD_ON);
  //  tft_par->set_min_n_coord(16);
  tft_par->set_use_anodes(1);
  tft_par->set_max_iterations(10);  
  tft_par->set_eloss_mode(mMutTrackFitPar::JNAGLE);

  mMutFindVtxPar* fvx_par = TMutNode<mMutFindVtxPar>::find_node(mutoo_node,"mMutFindVtxPar");      
  fvx_par->set_verbosity(MUTOO::NONE);
  
  mMutFitVtxPar* fiv_par = TMutNode<mMutFitVtxPar>::find_node(mutoo_node,"mMutFitVtxPar");      
  fiv_par->set_verbosity(MUTOO::NONE);
  fiv_par->set_max_iterations(10);
  fiv_par->set_mode(mMutFitVtxPar::FDF);
  
  mMutKalFitPar* kft_par = TMutNode<mMutKalFitPar>::find_node(mutoo_node,"mMutKalFitPar");      
  kft_par->set_verbosity(MUTOO::NONE);
  
  mMutEvalPar* evl_par = TMutNode<mMutEvalPar>::find_node(mutoo_node,"mMutEvalPar");      
  evl_par->set_pr_mode(mMutEvalPar::NORMAL);
  
  TMutStubFinder::set_verbose(true);  
  TMutStubFinder::set_dca_cut(4.0);
  TMutStubFinder::set_w_prox_cut(2.0);
  TMutStubFinder::set_reverse_algo(true);
  
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








