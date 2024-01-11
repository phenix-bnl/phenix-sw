/*! 
  @defgroup analysis Analysis
  Analysis Event Loops. Routines in this packages contain pdst style
  event loops for iterating throught MUTOO DST output.
*/

/*! ingroup analysis */
/*! \file ana_dst.cxx 
  Prototype pdst type analysis routine.  It demos how to access the 
  data in MUTOO Interface Object Containers from the disk resident DST.  
*/

#include "ana_dst.h"

// MUTOO includes 
//
#include "TMutNode.h"
#include "TMutMuiRoadMap.h"
#include "TMutHitMap.h"
#include "TMutClusMap.h"
#include "TMutCoordMap.h"
#include "TMutTrkMap.h"
#include "TMutStubMap.h"
#include "TMutClusMap.h"
#include "TMutVtxMap.h"
#include "TMutMCHitMap.h"
#include "TMutMCTrkMap.h"
#include "TMuiMCHitMapO.h"
#include "TMuiHitMapO.h"
#include "TMuiClusterMapO.h"
#include "TMui1DRoadMapO.h"
#include "TMuiRoadMapO.h"
#include "headerWrapper.h"
#include "PhMutooDisplay.h"

// Globals
//
TFile*   ana_file;
TNtuple* nt1;
TNtuple* nt2;
PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
std::string output_ntuple_name;
static bool init_done = false;

// Prototypes
//
void fill_ntuple(TMutTrkMap::const_pointer trk_ptr);
void fill_ntuple(TMutStubMap::const_pointer stub_ptr);


int process_event (DstContent *dst)
{
  // Choose to either fill the ntuple from the TMutStub residual or the TMutTrk residuals
  //
  enum Mode {STUB, TRACK};
  Mode mode = STUB;
  float nt_vars[100]={-1};      
  
  try {  
    
    // Do some setup
    //
    PHCompositeNode* dst_node = dst->get_topNode();    
    static int ievent=0;
    if(!init_done) { 
      setup_all(dst);
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
    
    //Fill event summary ntuple for Hijing studies.
    //
    /* IOC's */
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");    
    TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(mutoo_node,"TMutVtxMap");
    TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(mutoo_node,"TMutStubMap");
    TMutClusMap* clus_map = TMutNode<TMutClusMap>::find_node(mutoo_node,"TMutClusMap");    
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(mutoo_node,"TMutHitMap");    
    TMuiRoadMapO* road_map = TMutNode<TMuiRoadMapO>::find_node(mutoo_node,"TMuiRoadMapO");
    TMuiClusterMapO* mui_clus_map = TMutNode<TMuiClusterMapO>::find_node(mutoo_node,"TMuiClusterMapO");
    TMuiHitMapO* mui_hit_map = TMutNode<TMuiHitMapO>::find_node(mutoo_node,"TMuiHitMapO");
    headerWrapper* header = TMutNode<headerWrapper>::find_io_node(top_node,"header");
    if(header) {
      HEADER_ST*   headerst = header->TableData();
      nt_vars[5] = headerst[0].multiplicity;
      nt_vars[6] = headerst[0].b;
    }

    for(UShort_t iarm = 0; iarm < 2; iarm++) {
      nt_vars[0] = iarm;
      TMutVtxMap::const_iterator vtx_iter = vtx_map->get(iarm);
      TMutTrkMap::const_iterator trk_iter = trk_map->get(iarm);
      TMuiRoadMapO::const_iterator road_iter = road_map->get(iarm);
      TMuiClusterMapO::const_iterator mui_clus_iter = mui_clus_map->get(iarm);
      TMuiHitMapO::const_iterator mui_hit_iter = mui_hit_map->get(iarm);
      for(UShort_t ista = 0; ista < 3; ista++) {
	nt_vars[1] = ista;
	TMutStubMap::const_iterator stub_iter = stub_map->get(iarm, ista);
	int n_stub = stub_iter.count();
	nt_vars[2] = n_stub;
	int n_clus = 0;
	int n_hit = 0;
	UShort_t ngap = 3;
	if(ista==2) ngap = 2;
	for(UShort_t ioct  = 0; ioct < 8; ioct++) {
	  for(UShort_t ihoct = 0; ihoct < 2; ihoct++) {
	    for(UShort_t igap = 0; igap < ngap; igap++) {
	      for(UShort_t icath = 0; icath < 2; icath++) {
		TMutClusMap::const_iterator clus_iter = clus_map->get(iarm,
								      ista,
								      ioct,
								      ihoct,
								      igap,
								      icath);

		TMutHitMap::const_iterator hit_iter = hit_map->get(iarm,
								   ista,
								   ioct,
								   ihoct,
								   igap,
								   icath);



		  n_hit += hit_iter.count();
		  n_stub += stub_iter.count();
		  n_clus += clus_iter.count();

	      }
	    }
	  }
	}
	nt_vars[3] = n_clus;
	nt_vars[4] = n_hit;
	nt2->Fill(nt_vars);
      }    
    }
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }  
  return 0;  
}

void fill_ntuple(TMutTrkMap::const_pointer trk_ptr)
{
  if(!trk_ptr->get()->get_reco_success()) return;
  float nt_vars[100]={0};      
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
    nt_vars[17] = res_iter->get_w_fit_error();
    nt_vars[18] = res_iter->get_clus_width();	
    double tan_theta = std::sqrt(MUTOO::SQUARE(res_iter->get_fit_par()->get_dydz()) +
				 MUTOO::SQUARE(res_iter->get_fit_par()->get_dxdz()));
    nt_vars[19] = std::atan(tan_theta);
    nt_vars[20] = res_iter->get_fit_par()->get_dxdz();
    nt_vars[21] = res_iter->get_fit_par()->get_dydz();
    nt_vars[22] = res_iter->get_fit_par()->get_x();
    nt_vars[23] = res_iter->get_fit_par()->get_y();
    nt_vars[24] = res_iter->get_cos_theta_r();
    nt1->Fill(nt_vars);
  }
}

void fill_ntuple(TMutStubMap::const_pointer stub_ptr)
{
  float nt_vars[100]={0};      
  const TMutStub::residual_list* residuals = stub_ptr->get()->get_w_residual_list();
  TMutStub::const_residual_iterator res_iter = residuals->begin();
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
    nt_vars[13] = stub_ptr->get()->get_w_chi_square();
    nt_vars[14] = stub_ptr->get()->get_r_chi_square();
    nt_vars[15] = stub_ptr->get()->get_n_w_residual();
    nt_vars[16] = stub_ptr->get()->get_n_r_residual();
    nt_vars[17] = res_iter->get_w_fit_error();
    nt_vars[18] = res_iter->get_clus_width();	
    double tan_theta = std::sqrt(MUTOO::SQUARE(res_iter->get_fit_par()->get_dydz()) +
				 MUTOO::SQUARE(res_iter->get_fit_par()->get_dxdz()));
    nt_vars[19] = std::atan(tan_theta);
    nt_vars[20] = res_iter->get_fit_par()->get_dxdz();
    nt_vars[21] = res_iter->get_fit_par()->get_dydz();
    nt_vars[22] = res_iter->get_fit_par()->get_x();
    nt_vars[23] = res_iter->get_fit_par()->get_y();
    nt_vars[24] = res_iter->get_cos_theta_r();
    nt1->Fill(nt_vars);
  }
}

int setup_all(DstContent *dst) {

  ana_file = new TFile(output_ntuple_name.c_str(),"recreate");

  nt1 = new TNtuple("nt1","nt1","arm:sta:oct:hoct:gap:cath:q_peak:q_tot:cos_ac:cos_wz:"
		    "w_trk:w_meas:r_trk:chi_w:chi_r:n_coord:n_gap:w_fit_err:clus_width:"
		    "eta:dxdz:dydz:x:y:cos_th_r");

  nt2 = new TNtuple("nt2","nt2","arm:sta:n_stub:n_clus:n_hit:mult:b");

  PHCompositeNode* dst_node = dst->get_topNode();

  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");

  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");

  // Create the the IOCs here and couple to input DST
  //

  // TMutMuiRoad gets its own try catch block in case we ran the reco in NO_MUID mode
  //
  try {
    //    TMutNode<TMutMuiRoadMap>::new_dst_input_node(mutoo_node,"TMutMuiRoadMap", dst_node,"TMutMuiRoad" );
    TMutNode<TMuiRoadMapO>::new_dst_input_node(mutoo_node,"TMuiRoadMapO",dst_node,"TMuiRoadO");
    TMutNode<TMuiClusterMapO>::new_dst_input_node(mutoo_node,"TMuiClusterMapO",dst_node,"TMuiClusterO");
    TMutNode<TMuiHitMapO>::new_dst_input_node(mutoo_node,"TMuiHitMapO",dst_node,"TMuiHitO");
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  
  try {
    TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap", dst_node,"TMutHit" );
    TMutNode<TMutClusMap>::new_dst_input_node(mutoo_node,"TMutClusMap", dst_node,"TMutClus");
    TMutNode<TMutCoordMap>::new_dst_input_node(mutoo_node,"TMutCoordMap", dst_node,"TMutCoord");
    TMutNode<TMutGapCoordMap>::new_dst_input_node(mutoo_node,"TMutGapCoordMap", dst_node,"TMutGapCoord");
    TMutNode<TMutStubMap>::new_dst_input_node(mutoo_node, "TMutStubMap",dst_node,"TMutStub");
    TMutNode<TMutTrkMap>::new_dst_input_node(mutoo_node, "TMutTrkMap",dst_node,"TMutTrk");
    TMutNode<TMutVtxMap>::new_dst_input_node(mutoo_node, "TMutVtxMap",dst_node,"TMutVtx");
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }

  init_done = true;
  std::cout<<"Haven't crashed yet\n" << std::endl;
  return 0;

};

int end_all() {
  cout<<"Writing output histograms (goodbye)\n";
  cout<<"  --> Root output file " << output_ntuple_name<< " created.\n";
  ana_file->Write();
  return 0;
}

void
set_ntuple_name(char* ntuple_filename)
{
  output_ntuple_name = std::string(ntuple_filename);
}








