/*! 
  @defgroup analysis Analysis
  Analysis Event Loops. Routines in this packages contain pdst/ezdst style
  event loops for iterating throught MUTOO DST output.
*/

/*! ingroup analysis */
/*! \file ana_dst.cxx 
  Prototype ezdst type analysis routine.  It demos how to access the 
  data in MUTOO Interface Object Containers from the disk resident DST.  
  This a a shameless hack some code Chris wrote.
*/

#include <iostream>
#include "ana_dst.h"
#include "ezdst.h"
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
#include "TMutMCHitMap.h"

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

#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#include "TH1.h"

TFile*   ana_file;
TNtuple* nt1;
TH1F* prox_h;
PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
static bool init_done = false;
PhMutooDisplay *display = 0;

static bool display_mode = true;

int process_event (DstContent *dst)
{
  try {  
    
    PHCompositeNode* dst_node = dst->get_topNode();

    static int ievent=0;
    if(!init_done) { 
      setup_all(dst);
      if(display_mode) setup_display();
      top_node->addNode(mutoo_node);
      top_node->addNode(dst->get_topNode());
      top_node->print();
    }  
    // Clear maps (IOCs) from last event
    //
    PHMapManager::clear();

    // This call fills the IOCs from the DST-resident objects
    //
    PHMapManager::read(dst_node);

    // Get the TMutTrkMap pointer from the node tree
    //
    TMutMuiRoadMap* road_map = TMutNode<TMutMuiRoadMap>::find_node(mutoo_node,"TMutMuiRoadMap");
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(mutoo_node,"TMutHitMap");
    TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::find_node(mutoo_node,"TMutCoordMap");
    
    // Loop over TMutTrk
    //
    TMutTrkMap::const_iterator trk_iter = trk_map->range();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
      
      // Dump the contents of the track map to std::cout
      //
      trk_ptr->get()->print(std::cout, true);      
      UShort_t road_depth = 0;
      
      // Get any associated TMuiRoad from the TMutTrk
      //
      TMutMuiRoadMap::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMutMuiRoad>();
      if(!road_iter.at_end()){
	prox_h->Fill(trk_ptr->get()->get_road_proximity());
	road_iter->get()->print();
	road_depth = road_iter->get()->get_depth();
      }
      
      // Get the associated TMutCoord from the TMutTrk
      //
      TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
      
      float nt_vars[100]={0};
      
      // Loop over TMutTrk residuals and dump data into ntuple
      //
      const TMutTrk::residual_list* residuals = trk_ptr->get()->get_w_residual_list();
      TMutTrk::const_residual_iterator res_iter = residuals->begin();
      for(;res_iter!=residuals->end();++res_iter){      
	nt_vars[0] = res_iter->get_arm();
	nt_vars[1] = res_iter->get_station();
	nt_vars[2] = res_iter->get_octant();
	nt_vars[3] = res_iter->get_half_octant();
	nt_vars[4] = res_iter->get_gap();
	nt_vars[5] = res_iter->get_cathode();
	nt_vars[6] = res_iter->get_q_peak();
	nt_vars[7] = res_iter->get_q_tot();
	nt_vars[8] = res_iter->get_cos_theta_ac();
	nt_vars[9] = res_iter->get_cos_theta_wz();
	nt_vars[10] = res_iter->get_w_trk();
	nt_vars[11] = res_iter->get_w_meas();
	nt_vars[12] = res_iter->get_r_trk();
	nt_vars[13] = trk_ptr->get()->get_w_chi_square();
	nt_vars[14] = trk_ptr->get()->get_r_chi_square();
	nt_vars[15] = trk_ptr->get()->get_n_w_residual();
	nt_vars[16] = trk_ptr->get()->get_n_r_residual();
	nt_vars[17] = road_depth;
	nt_vars[18] = trk_ptr->get()->get_road_proximity();
	nt_vars[19] = 0;
	nt_vars[20] = res_iter->get_w_fit_error();
	nt_vars[21] = res_iter->get_clus_width();	
	double tan_theta = std::sqrt(MUTOO::SQUARE(res_iter->get_fit_par().get_dydz()) +
				     MUTOO::SQUARE(res_iter->get_fit_par().get_dxdz()));
	nt_vars[22] = std::atan(tan_theta);
	nt_vars[23] = res_iter->get_fit_par().get_dxdz();
	nt_vars[24] = res_iter->get_fit_par().get_dydz();
	nt_vars[25] = res_iter->get_fit_par().get_x();
	nt_vars[26] = res_iter->get_fit_par().get_y();
	nt_vars[27] = res_iter->get_cos_theta_r();
	nt1->Fill(nt_vars);
      }
//        for(int istation=0;istation<3;++istation){
//  	// Loop over TMutTrk stub residuals and dump data into ntuple
//  	//
//  	const TMutTrk::residual_list* residuals = trk_ptr->get()->get_stub_residual_list(istation);
//  	TMutTrk::const_residual_iterator res_iter = residuals->begin();
//  	for(;res_iter!=residuals->end();++res_iter){      
//  	  nt_vars[0] = res_iter->get_arm();
//  	  nt_vars[1] = res_iter->get_station();
//  	  nt_vars[2] = res_iter->get_octant();
//  	  nt_vars[3] = res_iter->get_half_octant();
//  	  nt_vars[4] = res_iter->get_gap();
//  	  nt_vars[5] = res_iter->get_cathode();
//  	  nt_vars[6] = res_iter->get_q_peak();
//  	  nt_vars[7] = res_iter->get_q_tot();
//  	  nt_vars[8] = res_iter->get_cos_theta_ac();
//  	  nt_vars[9] = res_iter->get_cos_theta_wz();
//  	  nt_vars[10] = res_iter->get_w_trk();
//  	  nt_vars[11] = res_iter->get_w_meas();
//  	  nt_vars[12] = res_iter->get_r_trk();
//  	  nt_vars[13] = trk_ptr->get()->get_w_chi_square();
//  	  nt_vars[14] = trk_ptr->get()->get_r_chi_square();
//  	  nt_vars[15] = trk_ptr->get()->get_n_w_residual();
//  	  nt_vars[16] = trk_ptr->get()->get_n_r_residual();
//  	  nt_vars[17] = road_depth;
//  	  nt_vars[18] = trk_ptr->get()->get_road_proximity();
//  	  nt_vars[19] = 1;
//  	  nt1->Fill(nt_vars);
//  	}
      //    }
    }
    
    // Do the display
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

int setup_all(DstContent *dst) {
  ana_file = new TFile("top.root","recreate");
  nt1 = new TNtuple("nt1","nt1","arm:sta:oct:hoct:gap:cath:q_peak:q_tot:cos_ac:cos_wz:w_trk:w_meas:r_trk:chi_w:chi_r:n_coord:n_gap:depth:prox:stub:w_fit_err:clus_width:theta:dxdz:dydz:x:y:cos_th_r");

  prox_h = new TH1F("prox_h","prox_h",100,0,50);

  PHCompositeNode* dst_node = dst->get_topNode();

  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");

  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");

  try {
  // Create the the IOCs here and couple to input DST
  //
  TMutNode<TMutMuiRoadMap>::new_dst_input_node(mutoo_node,"TMutMuiRoadMap", dst_node,"TMutMuiRoad" );
  TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap", dst_node,"TMutHit" );
  TMutNode<TMutClusMap>::new_dst_input_node(mutoo_node,"TMutClusMap", dst_node,"TMutClus");
  TMutNode<TMutCoordMap>::new_dst_input_node(mutoo_node,"TMutCoordMap", dst_node,"TMutCoord");
  TMutNode<TMutGapCoordMap>::new_dst_input_node(mutoo_node,"TMutGapCoordMap", dst_node,"TMutGapCoord");
  TMutNode<TMutTrkMap>::new_dst_input_node(mutoo_node, "TMutTrkMap",dst_node,"TMutTrk");

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  init_done = true;
  std::cout<<"Haven't crashed yet\n" << std::endl;
  return 0;
};

int end_all() {
  cout<<"Writing output histograms (goodbye)\n";
  cout<<"  --> Root output file (top.root) created.\n";
  ana_file->Write();
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

void draw_plane(UShort_t arm, UShort_t octant){
  display->paint_plane_view(arm,octant);
}









