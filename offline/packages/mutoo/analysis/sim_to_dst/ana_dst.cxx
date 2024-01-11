/*! ingroup analysis */
/*! \file ana_dst.cxx 

*/

#include <iostream>
#include "ana_dst.h"
#include "recoConsts.h"
#include "MUTOO.h"
#include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "PHNodeReset.h"
#include "TNtuple.h"
#include "TFile.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "headerWrapper.h"
#include "VtxOut.h"
#include "RunToTime.hh"
#include "TMutLocalDBInit.h"
#include "MWGInclusiveNanoCutsv1.h"
#include "MuonNanoDSTfuncs.h"
#include <cassert>


typedef PHIODataNode<BbcOut> BbcOutNode_t;

PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
PHCompositeNode* ndst_node;

static bool init_done = false;
static int naddedbbc = 0;
static int naddedheader = 0;
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
mMutRejectTrack* mMutRejectTrackMod;
mMutFindTrack* mMutFindTrackMod;
mMutTrackFit* mMutTrackFitMod;
mMutFindVtx* mMutFindVtxMod;
mMutFitVtx* mMutFitVtxMod;
mMutKalFit* mMutKalFitMod;
mMutKalVtx* mMutKalVtxMod;
mMutResponse* mMutResponseMod;
mMutEval* mMutEvalMod;

// MUIOO module pointers
//

//mMuiInitModule* mMuiInitMod;
//
mMuiInitModule* mMuiInitMod;
mMuiClusterFinder* mMuiClusterFinderMod;
mMuiRoadFinder1* mMuiRoadFinder1Mod;


// Nano DST
// 
PHInclusiveNanoCuts *theNanoCutter = new MWGInclusiveNanoCutsv1();

// IO manager for output DST file
//
PHNodeIOManager *dstout = 0;
PHNodeIOManager *ndstout= 0;
PHCompositeNode *dstout_node=0;
PhMutooDisplay *display = 0;

// Ntuple output
//
TFile*   ana_file;
TNtuple* nt1;
TNtuple* Effic;

static char* ntfile = NULL;

enum Mode {TRACKING, KALMAN};
Mode mode = KALMAN;
//Mode mode = TRACKING;
BbcOut*  bbcout ;

static int ievent = 0;

int process_event (PHCompositeNode *dst)
{

  int i;

  ievent++;

  try {
    if(!init_done) { 
      setup_all(dst);
      top_node->addNode(mutoo_node);
      top_node->addNode(dst);
      top_node->addNode(ndst_node);
      top_node->print();
      ULong_t default_run = 69522;
      TMutLocalDBInit::initialize(top_node, default_run);

//        PHTimeStamp recoTime(2003,7,4,0,0,0);
//        mMuiInitMod->SetSearchTimeStamp(recoTime);
//        mMuiInitMod->event(top_node);

      init_done=true;
    }
    
    // Clear first for event display
    //
    PHMapManager::clear();
    
    // Fills IOCs coupled to the DST node
    //
    PHMapManager::read(dst);

    // MUIOO reconstruction
    // 
    // mMuiClusterFinderMod->event(top_node);
    // mMuiRoadFinder1Mod->event(top_node);

    // Reconstruction modules
    //
    mMutFindClusMod->event(top_node);
    mMutFitClusMod->event(top_node);
    mMutFindGapCoordMod->event(top_node);
    mMutFindTrackMod->event(top_node);
    mMutStubFitMod->event(top_node);
    mMutEvalMod->event(top_node);
    
    if(mode == TRACKING) {
      mMutTrackFitMod->event(top_node);
      mMutRejectTrackMod->event(top_node);
      mMutFindVtxMod->event(top_node);
      mMutFitVtxMod->event(top_node);      
    } else if(mode == KALMAN) {
      mMutKalFitMod->event(top_node);
      mMutRejectTrackMod->event(top_node);
      mMutFindVtxMod->event(top_node);
      mMutKalVtxMod->event(top_node);            
    }
    
    fill_ntuple(top_node);

    // Calculate strips hit per station.  There must be a better way(?):
    //    
    int nstrips[6] = {0};
    int nhitsmax = 0;


      TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(mutoo_node,"TMutHitMap");          
      TMutHitMap::iterator hit_iter = hit_map->range();

      TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(mutoo_node,"TMutMCTrkMap");          
      TMutMCTrkMap::iterator mc_trk_iter = mc_trk_map->range();      

      while(TMutHitMap::pointer hit_ptr = hit_iter.next()){
        nstrips[3*hit_ptr->get()->get_arm() + hit_ptr->get()->get_station()]++;
      }

      while(TMutMCTrkMap::pointer mc_trk_ptr = mc_trk_iter.next()){
	
	TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();	
	TMutEvalMap::key_iterator eval_iter = mc_trk_ptr->get()->get_associated<TMutEval>();

	std::set<int> octant_set;
	while(TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next()){
	  octant_set.insert(mc_hit_ptr->get()->get_octant());
	}
	if(octant_set.size() != 1) continue;

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
	ntvar[12] = (float)mc_trk_ptr->get()->get_associated<TMutTrk>().count();

	for (i=0; i<6; i++) ntvar[13+i] = (float)nstrips[0+i];
	ntvar[19] = (float)ievent;

	// Get the associated TMutEval(s)
	//
	nhitsmax = 0;

	while(TMutEvalMap::pointer eval_ptr = eval_iter.next()){

          ntvar[9] = eval_ptr->get()->get_trk_eval()->get_station_true_hits(0);
          ntvar[10] = eval_ptr->get()->get_trk_eval()->get_station_true_hits(1);
          ntvar[11] = eval_ptr->get()->get_trk_eval()->get_station_true_hits(2);

    	  ntvar[7] = eval_ptr->get()->get_trk_eval()->get_n_total_true_hits();	    
	  
          // store info on track that has the most number of correct hits:
		
          if (eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits() > nhitsmax){

            nhitsmax = eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits();

  	    TMutTrkMap::key_iterator trk_iter = eval_ptr->get()->get_associated<TMutTrk>();
	    if(!trk_iter.at_end()) {
              ntvar[3] = 1;   // at least one track "found"
	      
	  // Get the associated TMutEval
	  //
  	      ntvar[4] = trk_iter->get()->get_charge();
	      ntvar[5] = trk_iter->get()->get_trk_par()->get_ptot();
	      ntvar[26] = trk_iter->get()->get_reco_success();
	      ntvar[27] = trk_iter->get()->get_ghost();
	      ntvar[28] = trk_iter->get()->has_stub(0);
	      ntvar[29] = trk_iter->get()->has_stub(1);
	      ntvar[30] = trk_iter->get()->has_stub(2);
	      ntvar[6] = eval_ptr->get()->get_trk_eval()->get_n_reco_true_hits();
	      ntvar[31] = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(0);
	      ntvar[32] = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(1);
	      ntvar[33] = eval_ptr->get()->get_trk_eval()->get_station_reco_true_hits(2);
	      ntvar[34] = (float)trk_iter->get()->get_n_coord();
            }
          }
	}  // loop over eval objects associated with MC track object
	Effic->Fill(ntvar);
	if(ntvar[6] != ntvar[7] && ntvar[2]>2.0){
	  PHMapManager::write();    
	  dstout->write(dstout_node);
	}
      }

    PHMapManager::write();
    
    PHTypedNodeIterator<BbcOut> bbc(top_node);
    BbcOutNode_t *bbc_node = bbc.find("BbcOut");
    if(bbc_node) {
      bbcout = (BbcOut*)bbc_node->getData();
      cout << " BBC vertex " << bbcout->get_VertexPoint() << endl;
      if (naddedbbc == 0)
	{
	  dstout_node->addNode(bbc_node);
	  naddedbbc++;
	}
    }
    
    headerWrapper* header = TMutNode<headerWrapper>::find_io_node(top_node,"header");
    if(header) {
      HEADER_ST*   headerst = header->TableData();
      cout << " mult " << headerst[0].multiplicity
	   << " imp " << headerst[0].b
	   << " zvertex " << headerst[0].vertex[2]
	   << endl;
    }

    PHNodeIterator iter(top_node);
    PHNode *n = iter.findFirst("PHIODataNode", "header");
    if (!n) {
      cout << "ERROR:  'in' parameter header not found" << endl;
    }
    if (naddedheader == 0)
      {      
	dstout_node->addNode(n);
	naddedheader++;
      }

    dstout->write(dstout_node);
    //output ndst.
    //
    process_MuonNanoDST(top_node, theNanoCutter);
    
    // output Nano DST.
    //
    PHCompositeNode *ndstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "NDST"));
    if(!ndstNode) {
      cout << " we could not find Nano dst node !!! " <<endl;
    } else {
      ndstout->write(ndstNode);
    }   
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  
  return 0;
}

void fill_ntuple(PHCompositeNode* top_node) {
  
  // Get Track node and vtx node from node tree.
  //
  TMutVtxMap*   vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
  TMutMCTrkMap* mc_map  = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  
  // Book an array for containing the varibles which will be filled in ntuple.
  //
  float ntvar[100] = {0};
  bool good_event=false;
  
  TMutMCTrkMap::const_iterator mc_iter = mc_map->range();
  if(mc_iter.count()<2) return;
  int good_mc = 0;
  while(TMutMCTrkMap::const_pointer mc_ptr = mc_iter.next()){
    TMutMCHitMap::const_key_iterator mc_hit_iter = mc_ptr->get()->get_associated<TMutMCHit>();
    UShort_t good_hit =0x0;
    while(TMutMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next()){
      if(mc_hit_ptr->get()->get_station()==0) 
	good_hit = good_hit|(0x1);
      if(mc_hit_ptr->get()->get_station()==1) 
	good_hit = good_hit|(0x2);
      if(mc_hit_ptr->get()->get_station()==2) 
	good_hit = good_hit|(0x4);
    }
    if(good_hit==7) good_mc++;
  }
  if(good_mc!=2) return;
  
  std::vector<TMutVtxMap::const_pointer> good_vtx;
  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next()){
    int good_trk = 0;
    if(vtx_ptr->get()->get_sign()!=TMutVtx::POSNEG) continue;
    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()) {	  
      if((trk_ptr->get()->has_stub(0))&&(trk_ptr->get()->has_stub(1))&&(trk_ptr->get()->has_stub(2))) good_trk++;
    }
    if(good_trk==2) {
      good_event = true;
      good_vtx.push_back(vtx_ptr);
    } else {
      continue;
    }
  }
  if(good_event) {
    std::vector<TMutVtxMap::const_pointer>::const_iterator good_iter = good_vtx.begin();
    for(;good_iter!=good_vtx.end();++good_iter){
      TMutTrkMap::const_key_iterator trk_iter = (*good_iter)->get()->get_associated<TMutTrk>();
      while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()) {	  
	TMutMCTrkMap::const_key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
	const TMutTrkPar *trk_par = trk_ptr->get()->get_trk_par_vtx();
	ntvar[21] = trk_par->get_z();
	// Get the tracks truth info
	//       
	while(TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()){
	  ntvar[0] = mc_trk_ptr->get()->get_arm();
	  if(mc_trk_ptr->get()->get_charge() == 1) {
	    ntvar[1] = mc_trk_ptr->get()->get_ptot_orig();
	    ntvar[2] = mc_trk_ptr->get()->get_px_orig();
	    ntvar[3] = mc_trk_ptr->get()->get_py_orig();
	    ntvar[4] = mc_trk_ptr->get()->get_pz_orig();
	    ntvar[22]= mc_trk_ptr->get()->get_z_orig();
	  } else {
	    ntvar[5] = mc_trk_ptr->get()->get_ptot_orig();
	    ntvar[6] = mc_trk_ptr->get()->get_px_orig();
	    ntvar[7] = mc_trk_ptr->get()->get_py_orig();
	    ntvar[8] = mc_trk_ptr->get()->get_pz_orig();
	    ntvar[22]= mc_trk_ptr->get()->get_z_orig();
	  }	
	}            
      }
      // true pt for the pair.
      //
      ntvar[9] = sqrt(pow(ntvar[2]+ntvar[6],2)+pow(ntvar[3]+ntvar[7],2));
      // true xf for the pair. 100 GeV/c is the beam energy.
      //
      ntvar[10]=(ntvar[4]+ntvar[8])/100.0;
      // ptot
      //
      ntvar[11]= sqrt(pow(ntvar[4]+ntvar[8],2)+ntvar[9]*ntvar[9]);
      // rapidity.
      //
      float mass_jpsi = 3.09687;
      float E         = sqrt(ntvar[11]*ntvar[11]+mass_jpsi*mass_jpsi);
      ntvar[12]       = 0.5*log((E+100.0*ntvar[10])/(E-100*ntvar[10]));
      // mass.
      //
      float mass_mu = 0.105658357;
      ntvar[13]     = sqrt((sqrt(ntvar[1]*ntvar[1]+mass_mu*mass_mu)+sqrt(ntvar[5]*ntvar[5]+mass_mu*mass_mu))*
			   (sqrt(ntvar[1]*ntvar[1]+mass_mu*mass_mu)+sqrt(ntvar[5]*ntvar[5]+mass_mu*mass_mu))
			   -ntvar[11]*ntvar[11]);
      // reconstructed information for j/psi.
      const TMutVtxPar* vtx_par = (*good_iter)->get()->get_vtx_par();
      // reconstructed pt for the pair.
      //
      ntvar[14]= sqrt((vtx_par->get_px())*(vtx_par->get_px())+(vtx_par->get_py())*(vtx_par->get_py()));
      // reconstructed xf for the pair.
      //
      ntvar[15]= (vtx_par->get_pz())/100.0;
      // reconstructed ptot for the pair.
      //
      ntvar[16]= sqrt(ntvar[14]*ntvar[14]+(vtx_par->get_pz())*(vtx_par->get_pz()));
      // reconstructed mass for the pair.
      //
      ntvar[18]= (*good_iter)->get()->get_mass();
      //reconstructed rapidity.
      //
      float EE = sqrt(ntvar[16]*ntvar[16]+ntvar[18]*ntvar[18]);
      ntvar[17]= 0.5*log((EE+vtx_par->get_pz())/(EE-vtx_par->get_pz()));
      ntvar[19]= 1.0;
      if(!bbcout){ 
	ntvar[20]=-1000;
      }else{
	ntvar[20]=bbcout->get_VertexPoint();
      }      
      nt1->Fill(ntvar);
    } 
  }else {
    while(TMutMCTrkMap::const_pointer mc_trk_ptr = mc_iter.next()) {
      ntvar[0] = mc_trk_ptr->get()->get_arm();
      if(mc_trk_ptr->get()->get_charge() == 1) {
	ntvar[1] = mc_trk_ptr->get()->get_ptot_orig();
	ntvar[2] = mc_trk_ptr->get()->get_px_orig();
	ntvar[3] = mc_trk_ptr->get()->get_py_orig();
	ntvar[4] = mc_trk_ptr->get()->get_pz_orig();
	ntvar[22]= mc_trk_ptr->get()->get_z_orig();
      } else {
	ntvar[5] = mc_trk_ptr->get()->get_ptot_orig();
	ntvar[6] = mc_trk_ptr->get()->get_px_orig();
	ntvar[7] = mc_trk_ptr->get()->get_py_orig();
	ntvar[8] = mc_trk_ptr->get()->get_pz_orig();
	ntvar[22]= mc_trk_ptr->get()->get_z_orig();
      }	
    }
    // true pt for the pair.
    //
    ntvar[9] = sqrt(pow(ntvar[2]+ntvar[6],2)+pow(ntvar[3]+ntvar[7],2));
    // true xf for the pair. 100 GeV/c is the beam energy.
    //
    ntvar[10]=(ntvar[4]+ntvar[8])/100.0;
    // ptot
    //
    ntvar[11]= sqrt(pow(ntvar[4]+ntvar[8],2)+ntvar[9]*ntvar[9]);
    // rapidity.
    //
    float mass_jpsi = 3.09687;
    float E         = sqrt(ntvar[11]*ntvar[11]+mass_jpsi*mass_jpsi);
    ntvar[12]       = 0.5*log((E+100.0*ntvar[10])/(E-100*ntvar[10]));
    // mass.
    //
    float mass_mu = 0.105658357;
    ntvar[13]     = sqrt((sqrt(ntvar[1]*ntvar[1]+mass_mu*mass_mu)+sqrt(ntvar[5]*ntvar[5]+mass_mu*mass_mu))*
			 (sqrt(ntvar[1]*ntvar[1]+mass_mu*mass_mu)+sqrt(ntvar[5]*ntvar[5]+mass_mu*mass_mu))
			 -ntvar[11]*ntvar[11]);
    ntvar[19]= 0.0;
    if(!bbcout){ 
      ntvar[20]=-1000;
    }else{
	ntvar[20]=bbcout->get_VertexPoint();
    }      
    nt1->Fill(ntvar);
  }
}

int setup_all(PHCompositeNode* dst_node) {
  
  ana_file = new TFile(ntfile,"recreate");

  nt1 = new TNtuple("nt1","nt1","arm:ptot_1_true:p_x1_true:p_y1_true:p_z1_true:p_tot2_true:p_x2_true:p_y2_true:p_z2_true:pt_true:xf_true:ptot_true:y_true:mass_true:pt:xf:ptot:y:mass:good:bbc_vtx:trk_vtx:mctrk_vtx");
  Effic = new TNtuple("Effic","Effic","arm:charge:ptotus_true:found:charge_reco:ptotus_reco:nhitsf:nhitst:track_id:nhits1:nhits2:nhits3:nfound:nstrS1:nstrS2:nstrS3:nstrN1:nstrN2:nstrN3:event:pxt:pyt:pzt:xvt:yvt:zvt:recosuc:ghost:found1:found2:found3:nhitsf1:nhitsf2:nhitsf3:nhits");
  
  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");
  
  // create a node for the output DST
  //
  dstout_node = new PHCompositeNode("DST");

  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");

  // Set up NanoDst node.
  //
  ndst_node = new PHCompositeNode("NDST");

  // Initialize nano-dst output
  //
  init_MuonNanoDST(ndst_node);
  theNanoCutter->Initialize(top_node);

  // instantiate new maps
  //    
  // TMutMuiRoadMap* road_map = TMutNode<TMutMuiRoadMap>::new_node(mutoo_node,"TMutMuiRoadMap");
  TMutClusMap* clus_map = TMutNode<TMutClusMap>::new_node(mutoo_node,"TMutClusMap");
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_node(mutoo_node,"TMutGapCoordMap");  
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_node(mutoo_node, "TMutCoordMap");
  TMutEvalMap* eval_map = TMutNode<TMutEvalMap>::new_node(mutoo_node, "TMutEvalMap");
  TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_node(mutoo_node, "TMutStubMap");
  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_node(mutoo_node, "TMutTrkMap");
  TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::new_node(mutoo_node, "TMutVtxMap");
  TMuiRoadMapO* muiroad_map = TMutNode<TMuiRoadMapO>::new_node(mutoo_node,"TMuiRoadMapO");
  TMui1DRoadMapO* mui1droad_map = TMutNode<TMui1DRoadMapO>::new_node(mutoo_node,"TMui1DRoadMapO");
  TMuiClusterMapO* muiclus_map = TMutNode<TMuiClusterMapO>::new_node(mutoo_node,"TMuiClusterMapO");

  // instantiate maps coupled to DST resident IOs
  //
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::new_dst_input_node(mutoo_node,"TMutMCHitMap",
									dst_node, "TMutMCHit");  

  TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::new_dst_input_node(mutoo_node,"TMutMCTrkMap",
									dst_node, "TMutMCTrk");  

  TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap",
									dst_node, "TMutHit");  

  TMuiMCHitMapO* mc_muihit_map = TMutNode<TMuiMCHitMapO>::new_dst_input_node(mutoo_node,"TMuiMCHitMapO",dst_node,"TMuiMCHitO");
  TMuiHitMapO* muihit_map = TMutNode<TMuiHitMapO>::new_dst_input_node(mutoo_node,"TMuiHitMapO",dst_node,"TMuiHitO");
  
  // choose which maps are persistant
  //
  mc_muihit_map->make_persistant(dstout_node,"TMuiMCHitO");
  mc_hit_map->make_persistant(dstout_node,"TMutMCHit");
  eval_map->make_persistant(dstout_node,"TMutEval");
  mc_trk_map->make_persistant(dstout_node,"TMutMCTrk");
  muihit_map->make_persistant(dstout_node,"TMuiHitO");
  hit_map->make_persistant(dstout_node,"TMutHit");
  clus_map->make_persistant(dstout_node,"TMutClus");
  muiclus_map->make_persistant(dstout_node,"TMuiClusterO");
  gap_coord_map->make_persistant(dstout_node,"TMutGapCoord");
  stub_map->make_persistant(dstout_node,"TMutStub");
  mui1droad_map->make_persistant(dstout_node,"TMui1DRoadO");
  trk_map->make_persistant(dstout_node,"TMutTrk");
  muiroad_map->make_persistant(dstout_node,"TMuiRoadO");
  coord_map->make_persistant(dstout_node,"TMutCoord");  
  vtx_map->make_persistant(dstout_node,"TMutVtx");
  

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

  mMutRejectTrackMod = new mMutRejectTrack();
  TMutNode<mMutRejectTrackPar>::new_node(mutoo_node,"mMutRejectTrackPar");  
  
  mMutTrackFitMod = new mMutTrackFit();
  TMutNode<mMutTrackFitPar>::new_node(mutoo_node,"mMutTrackFitPar");  

  mMutFindVtxMod = new mMutFindVtx();
  TMutNode<mMutFindVtxPar>::new_node(mutoo_node,"mMutFindVtxPar");  
  
  mMutKalFitMod = new mMutKalFit();
  TMutNode<mMutKalFitPar>::new_node(mutoo_node,"mMutKalFitPar");  

  mMutKalVtxMod = new mMutKalVtx();
  TMutNode<mMutKalVtxPar>::new_node(mutoo_node,"mMutKalVtxPar");  

  mMutFitVtxMod = new mMutFitVtx();
  TMutNode<mMutFitVtxPar>::new_node(mutoo_node,"mMutFitVtxPar");  
  
  mMutEvalMod = new mMutEval();
  TMutNode<mMutEvalPar>::new_node(mutoo_node,"mMutEvalPar");  
  
  mMuiInitMod = new mMuiInitModule;
  mMuiClusterFinderMod = new mMuiClusterFinder;
  mMuiRoadFinder1Mod = new mMuiRoadFinder1;

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
  fmc_par->set_verbosity(MUTOO::NONE);
  fmc_par->set_vtx_mode(mMutFindTrackMCPar::VTX_MINIMAL);
  
  mMutFindTrackPar* ftr_par = TMutNode<mMutFindTrackPar>::find_node(mutoo_node,"mMutFindTrackPar");  
  ftr_par->set_verbosity(MUTOO::NONE);
  ftr_par->set_framework(mMutFindTrackPar::MUIOO);

  mMutFindStubPar* fis_par = TMutNode<mMutFindStubPar>::find_node(mutoo_node,"mMutFindStubPar");  
  fis_par->set_verbosity(MUTOO::MAX);
  fis_par->set_mode(mMutFindStubPar::NO_REQUIRE_GC);

  mMutStubFitPar* fts_par = TMutNode<mMutStubFitPar>::find_node(mutoo_node,"mMutStubFitPar");  
  fts_par->set_verbosity(MUTOO::MAX);
  
  mMutStartTracksPar* str_par = TMutNode<mMutStartTracksPar>::find_node(mutoo_node,"mMutStartTracksPar");      
  str_par->set_verbosity(MUTOO::NONE);
  str_par->set_muid_mode(mMutStartTracksPar::NO_MUID);

  mMutTrackFitPar* tft_par = TMutNode<mMutTrackFitPar>::find_node(mutoo_node,"mMutTrackFitPar");      
  tft_par->set_verbosity(MUTOO::NONE);

  mMutFindVtxPar* fvx_par = TMutNode<mMutFindVtxPar>::find_node(mutoo_node,"mMutFindVtxPar");      
  fvx_par->set_verbosity(MUTOO::NONE);
  
  mMutFitVtxPar* fiv_par = TMutNode<mMutFitVtxPar>::find_node(mutoo_node,"mMutFitVtxPar");      
  fiv_par->set_verbosity(MUTOO::NONE);
  
  mMutKalFitPar* kft_par = TMutNode<mMutKalFitPar>::find_node(mutoo_node,"mMutKalFitPar");      
  kft_par->set_verbosity(MUTOO::NONE);

  kft_par->set_z_vtx_source(mMutKalFitPar::Z_MC);
  //kft_par->set_pisa_file("pisafile_9jul03_run3.dat.cZ");
  kft_par->set_map_file_flag(3);

  mMutKalVtxPar* kvx_par = TMutNode<mMutKalVtxPar>::find_node(mutoo_node,"mMutKalVtxPar");      
  kvx_par->set_verbosity(MUTOO::NONE);
  kvx_par->set_z_vtx_source(mMutKalVtxPar::Z_MC);
  kvx_par->set_do_track_loop(false);
 
  mMutEvalPar* evl_par = TMutNode<mMutEvalPar>::find_node(mutoo_node,"mMutEvalPar");      
  evl_par->set_pr_mode(mMutEvalPar::NORMAL);
    
  mMutRejectTrackPar* rjt_par = TMutNode<mMutRejectTrackPar>::find_node(mutoo_node,"mMutRejectTrackPar");    
  rjt_par->set_remove_rejected(false);

  TMutNode<mMuiClusterFinderPar>::new_node(mutoo_node,"mMuiClusterFinderPar");    
  TMutNode<mMuiRoadFinder1Par>::new_node(mutoo_node,"mMuiRoadFinder1Par");    

  return 0;  
}

int end_all() {
  ana_file->Write();
  delete theNanoCutter;
  delete dstout;
  delete ndstout;
  MUTOO::TRACE("terminated normally");
  return 0;
}


int
dstout_fopen(PHString dstout_filename, PHString ndstout_filename, char* ntout)
{
  cout << " dstout_filename " << dstout_filename
       << " ndstout_filename " << ndstout_filename
       << " nt_filename " << ntout
       << endl;
  if (!dstout)  dstout = new PHNodeIOManager(dstout_filename, PHWrite);
  if (!ndstout) ndstout= new PHNodeIOManager(ndstout_filename, PHWrite);
  dstout->SetCompressionLevel(3);
  if (!ntfile) ntfile = ntout;
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















