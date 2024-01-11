/*! ingroup analysis */
/*! \file lvl2_trigger.cxx 
  This routine is a dreco style analysis event loop for
  doing MUTOO re-analysis from DST format data format data.  
*/

#include <iostream>
#include "lvl2_trigger.h"
#include <math.h>
#include "MUTOO.h"
#include "ezdst.h"
#include "ZdcOut.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "PHIODataNode.h"
#include "PHNodeReset.h"
#include "TMutGeo.h"

#include "TSystem.h"
#include "TFile.h"
#include "TNtuple.h"
#include "PhenixRun.hh"
#include "PhCanvas.hh"
#include "PhDchDisplay.hh"
#include "PhPadDisplay.hh"
#include "PhTecDisplay.hh"
#include "PhMuiDisplay.hh"
#include "PhMutDisplay.hh"
#include "PhEventDisplay.hh"
#include "TMutTrackUtil.h"

PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
static bool init_done = false;

// MUTOO module pointers
//
mMutUnpackMutDst* mMutUnpackMutDstMod;
mMutFindClus* mMutFindClusMod;
mMutFitClus* mMutFitClusMod;
mMutFindGapCoord* mMutFindGapCoordMod;
mMutStartTracks* mMutStartTracksMod;
mMutTrackFit* mMutTrackFitMod;
mMutResponse* mMutResponseMod;

// IO manager for output DST filex
//
PHNodeIOManager *dstout = 0;
PHCompositeNode *dstout_node=0;
PhMutooDisplay *display = 0;
bool display_mode = false;

TFile*   ana_file;
TNtuple* nt1;
static int nevent =0;


int process_event (DstContent *dst)
{
  try {

    if(!init_done) { 
      setup_all(dst->get_topNode());
      top_node->addNode(mutoo_node);
      top_node->addNode(dst->get_topNode());
    }
    
    nevent = nevent + 1;
    // Clear first for event display
    //
    PHMapManager::clear();
    
    // Fills IOCs coupled to the DST node
    //
    PHMapManager::read(dst->get_topNode());
    
    TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::find_node(mutoo_node,"TMutMCHitMap"); 
    TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(mutoo_node,"TMutMCTrkMap");
    TMutStubMap* stub_map = TMutNode<TMutStubMap>::find_node(mutoo_node,"TMutStubMap");
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");

    // Get pointers to both tracks and associated stubs
    //
    TMutTrk* tracks[2] = {0};
    TMutStub* stubs_st2[2] = {0};
    TMutStub* stubs_st3[2] = {0};

    // if(trk_map->size() != 2) return 0;
    // if(stub_map->size() != 6) return 0;

    size_t itrack=0;    
    TMutTrkMap::iterator trk_iter = trk_map->range();
    while(TMutTrkMap::pointer trk_ptr = trk_iter.next()){
      tracks[itrack++] = trk_ptr->get();
    }

    for(itrack=0; itrack<2; ++itrack){
      TMutTrk* trk = tracks[itrack];
      // Get station 3 stubs
      //
      TMutStubMap::key_iterator stub_iter = trk->get_associated<TMutStub>();
      while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
	if(stub_ptr->get()->get_station() == MUTOO::Station3) {
	  stubs_st3[itrack] = stub_ptr->get(); 
	}
	if(stub_ptr->get()->get_station() == MUTOO::Station2) {
	  stubs_st2[itrack] = stub_ptr->get(); 
	}
      }            
    }

    // Punt if we don't have a stubs in station 2 and 3 for both tracks
    //
    for(itrack=0; itrack<2; ++itrack){
      if(stubs_st2[itrack] == 0 || stubs_st3[itrack] == 0) return 0;
    }

    // Truth Info
    //
    double px1_true = 0;
    double py1_true = 0;
    double pz1_true = 0;
    double ptot1_true = 0;
    {
      TMutMCTrkMap::key_iterator mc_trk_iter = tracks[0]->get_associated<TMutMCTrk>();
      px1_true = mc_trk_iter->get()->get_px_orig();
      py1_true = mc_trk_iter->get()->get_py_orig();
      pz1_true = mc_trk_iter->get()->get_pz_orig();
      ptot1_true = mc_trk_iter->get()->get_ptot_orig();
    }

    double px2_true = 0;
    double py2_true = 0;
    double pz2_true = 0;
    double ptot2_true = 0;
    {
      TMutMCTrkMap::key_iterator mc_trk_iter = tracks[1]->get_associated<TMutMCTrk>();
      px2_true = mc_trk_iter->get()->get_px_orig();
      py2_true = mc_trk_iter->get()->get_py_orig();
      pz2_true = mc_trk_iter->get()->get_pz_orig();
      ptot2_true = mc_trk_iter->get()->get_ptot_orig();
    }
    double mass_true = calc_mass(px1_true,py1_true,pz1_true,px2_true,py2_true,pz2_true);
    MUTOO::PRINT(std::cout,"Truth Info");
    std::cout << " px trk1: " << px1_true << " py trk1: " << py1_true << " pz trk1: " << pz1_true << std::endl;
    std::cout << " px trk2: " << px2_true << " py trk2: " << py2_true << " pz trk2: " << pz2_true << std::endl;
    std::cout << " ptot trk1: " << ptot1_true << " ptot trk2: " << ptot2_true << std::endl;
    std::cout << " mass: " << mass_true << std::endl;
    MUTOO::PRINT(std::cout,"**");

    // Calc theta, dphi using full stub fit
    //
    PHPoint stub_pt_st2_trk1 = stubs_st2[0]->get_fit_par()->get_point();
    PHPoint stub_pt_st3_trk1 = stubs_st3[0]->get_fit_par()->get_point();
    PHPoint stub_pt_st2_trk2 = stubs_st2[1]->get_fit_par()->get_point();
    PHPoint stub_pt_st3_trk2 = stubs_st3[1]->get_fit_par()->get_point();

    MUTOO::PRINT(std::cout,"Stub Points");
    std::cout << " track 1, station 2 = {" 
	      << stub_pt_st2_trk1.getX() << "," 
	      << stub_pt_st2_trk1.getY() << "," 
	      << stub_pt_st2_trk1.getZ() << "}" << std::endl;

    std::cout << " track 1, station 3 = {" 
	      << stub_pt_st3_trk1.getX() << "," 
	      << stub_pt_st3_trk1.getY() << "," 
	      << stub_pt_st3_trk1.getZ() << "}" << std::endl;

    std::cout << " track 2, station 2 = {" 
	      << stub_pt_st2_trk2.getX() << "," 
	      << stub_pt_st2_trk2.getY() << "," 
	      << stub_pt_st2_trk2.getZ() << "}" << std::endl;

    std::cout << " track 2, station 3 = {" 
	      << stub_pt_st3_trk2.getX() << "," 
	      << stub_pt_st3_trk2.getY() << "," 
	      << stub_pt_st3_trk2.getZ() << "}" << std::endl;

    MUTOO::PRINT(std::cout,"**");

    double px1_stub = 0;
    double py1_stub = 0;
    double pz1_stub = 0;
    double ptot1_stub = 0;
    double px2_stub = 0;
    double py2_stub = 0;
    double pz2_stub = 0;
    double ptot2_stub = 0;
    double mass_stub = 0;

    // Calc theta, dphi using stub coordinates
    //
    {
      double r = 0;
      
      // theta calculation (0,0,0) (stubs position gap0 station 2)
      //
      r = std::sqrt(MUTOO::SQUARE(stub_pt_st2_trk1.getX()) + 
		    MUTOO::SQUARE(stub_pt_st2_trk1.getY()));
      double theta_stub_trk1 = MUTOO::RAD_TO_DEG*std::atan2(r,std::fabs(stub_pt_st2_trk1.getZ()));
      
      r = std::sqrt(MUTOO::SQUARE(stub_pt_st2_trk2.getX()) + 
		    MUTOO::SQUARE(stub_pt_st2_trk2.getY()));
      
      double theta_stub_trk2 = MUTOO::RAD_TO_DEG*std::atan2(r,std::fabs(stub_pt_st2_trk2.getZ()));
      
      // dphi calculation (stub position gap0 station 2, gap 0 station 3)
      //
      double dphi_stub_trk1 = MUTOO::RAD_TO_DEG*std::fabs(std::atan2(stub_pt_st2_trk1.getY(),stub_pt_st2_trk1.getX())
							  - std::atan2(stub_pt_st3_trk1.getY(),stub_pt_st3_trk1.getX()));
      
      double dphi_stub_trk2 = MUTOO::RAD_TO_DEG*std::fabs(std::atan2(stub_pt_st2_trk2.getY(),stub_pt_st2_trk2.getX())
							  - std::atan2(stub_pt_st3_trk2.getY(),stub_pt_st3_trk2.getX()));
      
      double phi_stub_trk1 = std::atan2(stub_pt_st2_trk1.getY(),stub_pt_st2_trk1.getX());
      double phi_stub_trk2 = std::atan2(stub_pt_st2_trk2.getY(),stub_pt_st2_trk2.getX());
      
      double p1_stub = calc_mom(dphi_stub_trk1,theta_stub_trk1);
      double p2_stub = calc_mom(dphi_stub_trk2,theta_stub_trk2);

      px1_stub = p1_stub*sin(MUTOO::DEG_TO_RAD*theta_stub_trk1)*cos(phi_stub_trk1);
      py1_stub = p1_stub*sin(MUTOO::DEG_TO_RAD*theta_stub_trk1)*sin(phi_stub_trk1);
      pz1_stub = p1_stub*cos(MUTOO::DEG_TO_RAD*theta_stub_trk1);
      px2_stub = p2_stub*sin(MUTOO::DEG_TO_RAD*theta_stub_trk2)*cos(phi_stub_trk2);
      py2_stub = p2_stub*sin(MUTOO::DEG_TO_RAD*theta_stub_trk2)*sin(phi_stub_trk2);
      pz2_stub = p2_stub*cos(MUTOO::DEG_TO_RAD*theta_stub_trk2);

      mass_stub = calc_mass(px1_stub, py1_stub, pz1_stub, px2_stub, py2_stub, pz2_stub);
      
      MUTOO::PRINT(std::cout,"dphi,theta from STUB");
      std::cout << "track 1: theta,dphi = {" << theta_stub_trk1 << "," << dphi_stub_trk1 << "}" << std::endl;
      std::cout << "track 2: theta,dphi = {" << theta_stub_trk2 << "," << dphi_stub_trk2 << "}" << std::endl;
      std::cout << " p_trk1: " << p1_stub << " p_trk2: " << p2_stub << std::endl;
      std::cout << " mass: " << mass_stub << std::endl;
      MUTOO::PRINT(std::cout,"**");

    }

    // Calc theta, dphi using gap0 gap_coordinate
    //
    PHPoint gap_pt_st3_trk1;
    PHPoint gap_pt_st3_trk2;
    PHPoint gap_pt_st2_trk1;
    PHPoint gap_pt_st2_trk2;
    double px1_gap = 0;
    double py1_gap = 0;
    double pz1_gap = 0;
    double ptot1_gap = 0;
    double px2_gap = 0;
    double py2_gap = 0;
    double pz2_gap = 0;
    double ptot2_gap = 0;
    double mass_gap = 0;

    {
      double r = 0;
      {
	//	stubs_st3[0]->print();
  	TMutGapCoordMap::key_iterator gap_iter = stubs_st3[0]->get_associated<TMutGapCoord>();
  	while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next()){
	  //	  gap_ptr->get()->print();
  	  if (gap_ptr->get()->get_gap() != 0) continue;
  	  gap_pt_st3_trk1 = gap_ptr->get()->get_coord();
  	}
      }
      {
	//	stubs_st3[1]->print();
  	TMutGapCoordMap::key_iterator gap_iter = stubs_st3[1]->get_associated<TMutGapCoord>();
  	while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next()){
	  //	  gap_ptr->get()->print();
  	  if (gap_ptr->get()->get_gap() != 0) continue;
  	  gap_pt_st3_trk2 = gap_ptr->get()->get_coord();
  	}
      }
      {
	//	stubs_st2[0]->print();
  	TMutGapCoordMap::key_iterator gap_iter = stubs_st2[0]->get_associated<TMutGapCoord>();
  	while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next()){
	  //	  gap_ptr->get()->print();
  	  if (gap_ptr->get()->get_gap() != 0) continue;
  	  gap_pt_st2_trk1 = gap_ptr->get()->get_coord();
  	}
      }
      {
	//	stubs_st2[1]->print();
  	TMutGapCoordMap::key_iterator gap_iter = stubs_st2[1]->get_associated<TMutGapCoord>();
  	while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next()){
	  //	  gap_ptr->get()->print();
  	  if (gap_ptr->get()->get_gap() != 0) continue;
  	  gap_pt_st2_trk2 = gap_ptr->get()->get_coord();
  	}
      }
    

      r = std::sqrt(MUTOO::SQUARE(gap_pt_st2_trk1.getX()) + 
  		    MUTOO::SQUARE(gap_pt_st2_trk1.getY()));
      double theta_gap_trk1 = MUTOO::RAD_TO_DEG*std::atan2(r,std::fabs(gap_pt_st2_trk1.getZ()));
    
      r = std::sqrt(MUTOO::SQUARE(gap_pt_st2_trk2.getX()) + 
  		    MUTOO::SQUARE(gap_pt_st2_trk2.getY()));
      double theta_gap_trk2 = MUTOO::RAD_TO_DEG*std::atan2(r,std::fabs(gap_pt_st2_trk2.getZ()));
    
      // dphi calculation (stub position gap0 station 2, gap 0 station 3)
      //
      double dphi_gap_trk1 = MUTOO::RAD_TO_DEG*std::fabs(std::atan2(gap_pt_st2_trk1.getY(),gap_pt_st2_trk1.getX())
  							 - std::atan2(gap_pt_st3_trk1.getY(),gap_pt_st3_trk1.getX()));
    
      double dphi_gap_trk2 = MUTOO::RAD_TO_DEG*std::fabs(std::atan2(gap_pt_st2_trk2.getY(),gap_pt_st2_trk2.getX())
  							 - std::atan2(gap_pt_st3_trk2.getY(),gap_pt_st3_trk2.getX()));
    
      double phi_gap_trk1 = std::atan2(gap_pt_st2_trk1.getY(),gap_pt_st2_trk1.getX());
      double phi_gap_trk2 = std::atan2(gap_pt_st2_trk2.getY(),gap_pt_st2_trk2.getX());
    
      double p1_gap = calc_mom(dphi_gap_trk1,theta_gap_trk1);
      double p2_gap = calc_mom(dphi_gap_trk2,theta_gap_trk2);
      
      px1_gap = p1_gap*sin(MUTOO::DEG_TO_RAD*theta_gap_trk1)*cos(phi_gap_trk1);
      py1_gap = p1_gap*sin(MUTOO::DEG_TO_RAD*theta_gap_trk1)*sin(phi_gap_trk1);
      pz1_gap = p1_gap*cos(MUTOO::DEG_TO_RAD*theta_gap_trk1);

      px2_gap = p2_gap*sin(MUTOO::DEG_TO_RAD*theta_gap_trk2)*cos(phi_gap_trk2);
      py2_gap = p2_gap*sin(MUTOO::DEG_TO_RAD*theta_gap_trk2)*sin(phi_gap_trk2);
      pz2_gap = p2_gap*cos(MUTOO::DEG_TO_RAD*theta_gap_trk2);
  
      mass_gap = calc_mass(px1_gap,py1_gap,pz1_gap,px2_gap,py2_gap,pz2_gap);                
    
      MUTOO::PRINT(std::cout,"dphi,theta from Gap Coordinate");
      std::cout << "track 1: theta,dphi = {" << theta_gap_trk1 << "," << dphi_gap_trk1 << "}" << std::endl;
      std::cout << "track 2: theta,dphi = {" << theta_gap_trk2 << "," << dphi_gap_trk2 << "}" << std::endl;
      std::cout << " p_trk1: " << p1_gap << " p_trk2: " << p2_gap << std::endl;
      std::cout << " mass: " << mass_gap << std::endl;
      MUTOO::PRINT(std::cout,"**");
    }

    float nt_vars[100] = {0};
    nt_vars[0] = px1_true;
    nt_vars[1] = py1_true;
    nt_vars[2] = pz1_true;
    nt_vars[3] = px2_true;
    nt_vars[4] = py2_true;
    nt_vars[5] = pz2_true;
    nt_vars[6] = px1_stub;
    nt_vars[7] = py1_stub;
    nt_vars[8] = pz1_stub;
    nt_vars[9] = px2_stub;
    nt_vars[10] = py2_stub;
    nt_vars[11] = pz2_stub;
    nt_vars[12] = px1_gap;
    nt_vars[13] = py1_gap;
    nt_vars[14] = pz1_gap;
    nt_vars[15] = px2_gap;
    nt_vars[16] = py2_gap;
    nt_vars[17] = pz2_gap;
    nt_vars[18] = mass_true;
    nt_vars[19] = mass_stub;
    nt_vars[20] = mass_gap;
    nt1->Fill(nt_vars);
    
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  
  init_done=true;
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

int setup_all(PHCompositeNode* dst_node) {
  // create ROOT ntuple and output file
  //
  ana_file = new TFile("l2_ntuple.root","recreate");
  nt1 = new TNtuple("nt1","nt1","px1_true:py1_true:pz1_true:px2_true:py2_true:pz2_true:px1_mc:py1_mc:pz1_mc:px2_mc:py2_mc:pz2_mc:px1_gap:py1_gap:pz1_gap:px2_gap:py2_gap:pz2_gap:mass_true:mass_mc:mass_gap"); 
  
  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");
  
  // create a node for the output DST
  //
  dstout_node = new PHCompositeNode("DST");
  
  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");
  
  // instantiate maps coupled to DST resident IOs
  //
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::new_dst_input_node(mutoo_node,"TMutMCHitMap",dst_node, "TMutMCHit");  
  TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::new_dst_input_node(mutoo_node,"TMutMCTrkMap",dst_node, "TMutMCTrk");  
  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::new_dst_input_node(mutoo_node,"TMutTrkMap",dst_node,"TMutTrk");
  TMutStubMap* stub_map = TMutNode<TMutStubMap>::new_dst_input_node(mutoo_node,"TMutStubMap",dst_node,"TMutStub");
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::new_dst_input_node(mutoo_node,"TMutCoordMap",dst_node,"TMutCoord");
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::new_dst_input_node(mutoo_node,"TMutGapCoordMap",dst_node,"TMutGapCoord");

  init_done = true;

  return 0;
}


int end_all() {
  cout<<"Writing output histograms (goodbye)\n";
  cout<<"  --> Root output file (l2_ntuple_s_v0.root) created.\n";
  ana_file->Write();
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
  //  dstout->SeCompressionLevel(3);
  return 0;
}

void dump_mc_hit() {
  TMutMCHitMap* mc_hit_map = TMutNode<TMutMCHitMap>::find_node(mutoo_node,"TMutMCHitMap");          
  mc_hit_map->print();
}

void dump_mc_trk() {
  TMutMCTrkMap* mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(mutoo_node,"TMutMCTrkMap");          
  mc_trk_map->print();
}


float
calc_mom(float phidiff, float avetheta)
{  
  // this routine needs some work - parameters tuned for South only 
  // (simulations)
  static const float t = 12.;
  static const float Norm = 64.;
  static const float Const = 1.2;

  // phidiff and avetheta come in as absolute values
  // should be in degrees
  if (phidiff != 0 && avetheta > t)
    {
      float mom = Norm/(phidiff) * 1/(avetheta -t) + Const;
      return mom;
    }
  else 
    {
      return 0.0;
    }
}

float calc_mass(float px1, float py1, float pz1,
		float px2, float py2, float pz2)
{
  static const float mmu2 = 0.011163695; // ~= 0.105^2, i.e. muon mass squared

  float netmom2 = (px1+px2)*(px1+px2) + 
    (py1+py2)*(py1+py2) + (pz1+pz2)*(pz1+pz2); // square of the net mom. vector

  // energy of particles 1 and 2
  float E1 = sqrt(px1*px1+py1*py1+pz1*pz1 + mmu2);
  float E2 = sqrt(px2*px2+py2*py2+pz2*pz2 + mmu2);

  // calc. invariant mass squared of particles 1 and 2 and return mass if
  // it's valid
  float mass2 = (E1+E2)*(E1+E2) - netmom2;
  if (mass2 > 0)
    {
      return sqrt(mass2);
    }
  else
    { 
      return 0.0;  
    }
}

float 
calc_orig_phi(float phidiff, float phi2, float avetheta)
{  
  // phidiff, phi2 and avetheta are in degrees
  // this routine needs some work
  static const float p0 = -0.5;
  static const float p1 = 0.09;
  
  float factor = p0 + p1*avetheta;
  float origphi = phi2 - factor*phidiff;
  return origphi;
}






