#ifndef analyze_cxx
#define analyze_cxx
// ROOT headers
//
#include <TROOT.h>        
#include <TChain.h>       
#include <TFile.h>        
#include <TApplication.h> 
#include <TSystem.h>      
#include <TH1.h>          
#include <TNtuple.h>      

// PHENIX headers
//
#include "PHGlobal.h" 
#include "TrigLvl1.h"
#include "PHMuoTracksOut.h" 
#include "MUTOO.h"
#include "Event.h"
#include "TriggerUtilities.h"
#include "TriggerHelper.h"

// SL
//
#include<fstream>
#include<string>
#include<iostream>
#include<iomanip>
#include<FROG.h>
#include<math.h>

#endif
TFile* ana_file;
TNtuple* nt1; 
TNtuple* nt2;
TNtuple* nt3; 
TNtuple* nt4;

void fill_mutoo(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt);
void fill_mutoo_single(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt);
void fill_mut(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt);
void fill_mut_single(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt);
float get_phi(float px, float py);
float get_theta(float px, float py, float pz);
float get_eta(float px, float py, float pz);

int 
main(int argc,char **argv)
{
 
  if(argc==1 || argc > 3 ) {
    std::cout << "usage: scan MWG ndst file [output ntuple file]" << std::endl;
    return 0;
  } 
    
  // existence check on input file
  //
  ifstream file;
  std::string filename;
  FROG fr;
  file.open(fr.location(argv[1]),std::ios::in);
  if(!file.is_open()) {
    std::cout << "can't open input file " << argv[1] << std::endl;
    return 0;
  } else {    
    filename = std::string(argv[1]);
    file.close();
  }
  std::string outfile;
  if(argc==3) {
    outfile=std::string(argv[2]);
  } else {
    int shortstart = filename.rfind("/");
    std::string shortinput; 
    if(shortstart>0) {
      shortinput = filename.substr(shortstart+1, filename.size());
    } else {
      shortinput = filename;
    }
    outfile = "mwgntold_"+shortinput;
  }

  TROOT root("Analyze","Version");
  TApplication MyApp("Analyze",&argc,argv);
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libndst.so");
  gSystem->Load("libtriggerUtility.so");
  gSystem->Load("libtrigger.so");
  gSystem->Load("libFROG.so");

  cout<< " setup output ntuple " << outfile << endl;

  ana_file = new TFile(outfile.c_str(), "RECREATE");
  
  nt1 = new TNtuple("nt1","mutoo MWG analysis ntuple for dimuons","px:py:pz:mass:vtx_chi:sign:x_vtx:y_vtx:z_vtx:px1:py1:pz1:chi_square1:sign1:road1_depth1:road1_depth2:road1_depth3:road1_x1:road1_x2:road1_x3:road1_y1:road1_y2:road1_y3:road1_z1:road1_z2:road1_z3:x1_vtx:y1_vtx:z1_vtx:mutrhit1:status1:px2:py2:pz2:chi_square2:sign2:road2_depth1:road2_depth2:road2_depth3:road2_x1:road2_x2:road2_x3:road2_y1:road2_y2:road2_y3:road2_z1:road2_z2:road2_z3:x2_vtx:y2_vtx:z2_vtx:mutrhit2:status2:ievt:bbc_zvtx:bbc_q_N:bbc_q_S:zdc_e_N:zdc_e_S:centrality:Is_minibias:MUID2D_Live:MUID2D_SCALED:MUID1D1S_Live:MUID1D1S_SCALED:Lvl1_Bit_Live:Lvl1_Bit_Scaled:run:evt");

  nt2 = new TNtuple("nt2","mut MWG analysis ntuple for dimuons  ","px:py:pz:mass:vtx_chi:sign:x_vtx:y_vtx:z_vtx:px1:py1:pz1:chi_square1:sign1:road1_depth1:road1_depth2:road1_depth3:road1_x1:road1_x2:road1_x3:road1_y1:road1_y2:road1_y3:road1_z1:road1_z2:road1_z3:x1_vtx:y1_vtx:z1_vtx:mutrhit1:status1:px2:py2:pz2:chi_square2:sign2:road2_depth1:road2_depth2:road2_depth3:road2_x1:road2_x2:road2_x3:road2_y1:road2_y2:road2_y3:road2_z1:road2_z2:road2_z3:x2_vtx:y2_vtx:z2_vtx:mutrhit2:status2:ievt:bbc_zvtx:bbc_q_N:bbc_q_S:zdc_e_N:zdc_e_S:centrality:Is_minibias:MUID2D_Live:MUID2D_SCALED:MUID1D1S_Live:MUID1D1S_SCALED:Lvl1_Bit_Live:Lvl1_Bit_Scaled:run:evt");

  nt3 = new TNtuple("nt3","mutoo MWG analysis ntuple for single muons","px:py:pz:phi:theta:eta:trk_chi:sign:x_vtx:y_vtx:z_vtx:road_depth1:road_depth2:road_depth3:road_x1:road_x2:road_x3:road_y1:road_y2:road_y3:road_z1:road_z2:road_z3:mutrhit:status:ievt:bbc_zvtx:bbc_q_N:bbc_q_S:zdc_e_N:zdc_e_S:centrality:Is_minibias:MUID2D_Live:MUID2D_SCALED:MUID1D1S_Live:MUID1D1S_SCALED:Lvl1_Bit_Live:Lvl1_Bit_Scaled:run:evt");

  nt4 = new TNtuple("nt4","mut MWG analysis ntuple for single muons  ","px:py:pz:phi:theta:eta:trk_chi:sign:x_vtx:y_vtx:z_vtx:road_depth1:road_depth2:road_depth3:road_x1:road_x2:road_x3:road_y1:road_y2:road_y3:road_z1:road_z2:road_z3:mutrhit:status:ievt:bbc_zvtx:bbc_q_N:bbc_q_S:zdc_e_N:zdc_e_S:centrality:Is_minibias:MUID2D_Live:MUID2D_SCALED:MUID1D1S_Live:MUID1D1S_SCALED:Lvl1_Bit_Live:Lvl1_Bit_Scaled:run:evt");

  PHMuoTracksOut* mut=0;  
  PHMuoTracksOut* mutoo = 0; 
  PHGlobal* global = 0;
  TrigLvl1* triglvl1 = 0;

  TChain* T = new TChain("T",""); 
  T->Add(fr.location(filename.c_str())); 
  T->SetBranchAddress("DST/PHGlobal", &global);
  T->SetBranchAddress("DST/TrigLvl1", &triglvl1);
  T->SetBranchAddress("DST/PHMuoTracksOO", &mutoo);
  T->SetBranchAddress("DST/PHMuoTracks", &mut);

  int entry_read=0, ent_read=0, nentries=(int)T->GetEntries(); 


  for(int ievt=0; ievt<nentries; ievt++) { 

    T->GetEntry(ievt);
    // MUTOO 
    //
    fill_mutoo(mutoo, global, triglvl1, ievt);
    fill_mutoo_single(mutoo,global,triglvl1,ievt);
    // Mut   
    //
    fill_mut(mut, global, triglvl1, ievt);
    fill_mut_single(mut,global,triglvl1,ievt);
    if((ievt%999)==1) std::cout << " we just finished " << ievt << "th event in file " << fr.location(filename.c_str()) << "." <<std::endl;
  } 
  ana_file->Write();
}


void 
fill_mutoo(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt)
{

  //  Event * evt = 0;
  //  static TriggerUtilities tu;
  //  TriggerHelper* th=tu.getTriggerHelper(global->getRunNumber(), evt); 

  // decide Lvl1 trigger information based on lvl1 bit.
  //
  // use an arry to store the bit position of 
  // minibias trigger ------- first element of the arry.
  // south 2D trigger ------- second element of the arry.
  // south DS trigger ------- third element of the arry.
  // north 2D trigger ------- fourth element of the arry.
  // south DS trigger ------- fifth element of the arry.
  // 
  // One has to been careful because the position has been changed during
  // data taking, the arry filled below is according to run 80312.
  //
  ULong_t bit_position[5] = {0};

  // PPRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 87791 && global->getRunNumber() <=88260) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00100000;
    bit_position[4] = 0x00020000;
  }
  if(global->getRunNumber() >= 88350 && global->getRunNumber() <= 92446) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00200000;
    bit_position[4] = 0x00040000;
  }
  // dAuRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 78269 && global->getRunNumber() <=80312) {
    bit_position[0] = 0x00000004;
    bit_position[1] = 0x00000010;
    bit_position[2] = 0x00080000;
    bit_position[3] = 0x00040000;
    bit_position[4] = 0x00800000;
  }
  if(global->getRunNumber() >= 77530 && global->getRunNumber() <=78213) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00080000;
    bit_position[4] = 0x00800000;
  }
  
  
  unsigned short is_mini_bias         = 0;
  unsigned short is_2D_south_scaled   = 0;
  unsigned short is_2D_south_live     = 0;
  unsigned short is_1D1S_south_scaled = 0;
  unsigned short is_1D1S_south_live   = 0;
  unsigned short is_2D_north_scaled   = 0;
  unsigned short is_2D_north_live     = 0;
  unsigned short is_1D1S_north_scaled = 0;
  unsigned short is_1D1S_north_live   = 0;

  if((triglvl1->get_lvl1_trigscaled())&bit_position[0])
    is_mini_bias = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[1])
    is_2D_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[1])
    is_2D_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[2])
    is_1D1S_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[2])
    is_1D1S_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[3])
    is_2D_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[3])
    is_2D_north_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[4])
    is_1D1S_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[4])
    is_1D1S_north_live = 1;

  // Cache format so we can restore
  //

  int ndimu = muon->get_ndimu();
  for (int idimu=0; idimu<muon->get_ndimu(); idimu++){
    
    float nt_vars[100]={0.0};

    int index_trk1 = muon->get_ditrkIndex(0,idimu);
    int index_trk2 = muon->get_ditrkIndex(1,idimu);
    
    if (muon->get_ghostflag(index_trk1) || muon->get_ghostflag(index_trk2)) continue;
    
    nt_vars[0]  = muon->get_dipx(idimu);
    nt_vars[1]  = muon->get_dipy(idimu);
    nt_vars[2]  = muon->get_dipz(idimu);
    nt_vars[3]  = muon->get_dimass(idimu);
    nt_vars[4]  = muon->get_vtx_chisquare(idimu);
    nt_vars[5]  = muon->get_dicharge(idimu);
    nt_vars[6]  = muon->get_vtx_xpos(idimu);
    nt_vars[7]  = muon->get_vtx_ypos(idimu);
    nt_vars[8]  = muon->get_vtx_zpos(idimu);
    
    nt_vars[9]  = muon->get_px(0,index_trk1);
    nt_vars[10] = muon->get_py(0,index_trk1);
    nt_vars[11] = muon->get_pz(0, index_trk1);
    nt_vars[12] = muon->get_chisquare(index_trk1);
    nt_vars[13] = muon->get_charge(index_trk1);
    for(int k = 0; k < 3; k++) {
      nt_vars[14+k]=muon->get_muIDOOhits(k,index_trk1);
      nt_vars[17+k]=muon->get_muIDOO_gap0(0,k,index_trk1);
      nt_vars[20+k]=muon->get_muIDOO_gap0(1,k,index_trk1);
      nt_vars[23+k]=muon->get_muIDOO_gap0(2,k,index_trk1);
    }
    nt_vars[26] = muon->get_xpos(0,index_trk1);
    nt_vars[27] = muon->get_ypos(0,index_trk1);
    nt_vars[28] = muon->get_zpos(0,index_trk1);
    nt_vars[29] = muon->get_muTRhits(index_trk1);
    nt_vars[30] = muon->get_TMutTrk_status(index_trk1);
    
    nt_vars[31] = muon->get_px(0, index_trk2);
    nt_vars[32] = muon->get_py(0, index_trk2);
    nt_vars[33] = muon->get_pz(0, index_trk2);
    nt_vars[34] = muon->get_chisquare(index_trk2);
    nt_vars[35] = muon->get_charge(index_trk2);
    for(int k = 0; k < 3; k++) {
      nt_vars[36+k]=muon->get_muIDOOhits(k,index_trk2);
      nt_vars[39+k]=muon->get_muIDOO_gap0(0,k,index_trk2);
      nt_vars[42+k]=muon->get_muIDOO_gap0(1,k,index_trk2);
      nt_vars[45+k]=muon->get_muIDOO_gap0(2,k,index_trk2);
    }
    nt_vars[48] = muon->get_xpos(0,index_trk2);
    nt_vars[49] = muon->get_ypos(0,index_trk2);
    nt_vars[50] = muon->get_zpos(0,index_trk2);
    nt_vars[51] = muon->get_muTRhits(index_trk2);
    nt_vars[52] = muon->get_TMutTrk_status(index_trk2);
    nt_vars[53] = ievt;
    nt_vars[54] = global->getBbcZVertex();
    nt_vars[55] = global->getBbcChargeN();
    nt_vars[56] = global->getBbcChargeS();
    nt_vars[57] = global->getZdcEnergyN();
    nt_vars[58] = global->getZdcEnergyS();
    nt_vars[59] = global->getCentralitybyClock();
    nt_vars[60] = float(is_mini_bias);
    if(is_2D_south_live) nt_vars[61] = 1;
    if(is_2D_north_live) nt_vars[61] = 2;
    if(is_2D_south_live&&is_2D_north_live) nt_vars[61] = 3;
    if(is_2D_south_scaled) nt_vars[62] = 1;
    if(is_2D_north_scaled) nt_vars[62] = 2;
    if(is_2D_south_scaled&&is_2D_north_scaled) nt_vars[62] = 3;
    if(is_1D1S_south_live) nt_vars[63] = 1;
    if(is_1D1S_north_live) nt_vars[63] = 2;
    if(is_1D1S_south_live&&is_1D1S_north_live) nt_vars[63] = 3;
    if(is_1D1S_south_scaled) nt_vars[64] = 1;
    if(is_1D1S_north_scaled) nt_vars[64] = 2;
    if(is_1D1S_south_scaled&&is_1D1S_north_scaled) nt_vars[64] = 3;
    nt_vars[65] = float(triglvl1->get_lvl1_triglive());
    nt_vars[66] = float(triglvl1->get_lvl1_trigscaled());
    nt_vars[67] = global->getRunNumber();
    nt_vars[68] = global->getEventNumber();
    nt1->Fill(nt_vars);
   } 
}

void 
fill_mutoo_single(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt)
{

  //  Event * evt = 0;
  //  static TriggerUtilities tu;
  //  TriggerHelper* th=tu.getTriggerHelper(global->getRunNumber(), evt); 

  // decide Lvl1 trigger information based on lvl1 bit.
  //
  // use an arry to store the bit position of 
  // minibias trigger ------- first element of the arry.
  // south 2D trigger ------- second element of the arry.
  // south DS trigger ------- third element of the arry.
  // north 2D trigger ------- fourth element of the arry.
  // south DS trigger ------- fifth element of the arry.
  // 
  // One has to been careful because the position has been changed during
  // data taking, the arry filled below is according to run 80312.
  //
  ULong_t bit_position[5] = {0};

  // PPRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 87791 && global->getRunNumber() <=88260) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00100000;
    bit_position[4] = 0x00020000;
  }
  if(global->getRunNumber() >= 88350 && global->getRunNumber() <= 92446) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00200000;
    bit_position[4] = 0x00040000;
  }
  // dAuRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 78269 && global->getRunNumber() <=80312) {
    bit_position[0] = 0x00000004;
    bit_position[1] = 0x00000010;
    bit_position[2] = 0x00080000;
    bit_position[3] = 0x00040000;
    bit_position[4] = 0x00800000;
  }
  if(global->getRunNumber() >= 77530 && global->getRunNumber() <=78213) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00080000;
    bit_position[4] = 0x00800000;
  }
  
  
  unsigned short is_mini_bias         = 0;
  unsigned short is_2D_south_scaled   = 0;
  unsigned short is_2D_south_live     = 0;
  unsigned short is_1D1S_south_scaled = 0;
  unsigned short is_1D1S_south_live   = 0;
  unsigned short is_2D_north_scaled   = 0;
  unsigned short is_2D_north_live     = 0;
  unsigned short is_1D1S_north_scaled = 0;
  unsigned short is_1D1S_north_live   = 0;

  if((triglvl1->get_lvl1_trigscaled())&bit_position[0])
    is_mini_bias = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[1])
    is_2D_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[1])
    is_2D_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[2])
    is_1D1S_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[2])
    is_1D1S_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[3])
    is_2D_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[3])
    is_2D_north_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[4])
    is_1D1S_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[4])
    is_1D1S_north_live = 1;

  // Cache format so we can restore
  //

  int ntrk = muon->get_npart();
  for (int itrk=0; itrk<ntrk; itrk++){
    
    float nt_vars[100]={0.0};

    // By defination all mutoo tracks are not ghosts.
    //
    if (muon->get_ghostflag(itrk)) continue;
    
    // First index in get_px gives the location of 
    // where we measured the px, "0" means at vtx.
    //
    nt_vars[0] = muon->get_px(0,itrk);
    nt_vars[1] = muon->get_py(0,itrk);
    nt_vars[2] = muon->get_pz(0, itrk);
    nt_vars[3] = get_phi(muon->get_px(0,itrk),
			 muon->get_py(0,itrk));
    nt_vars[4] = get_theta(muon->get_px(0,itrk),
			   muon->get_py(0,itrk),
			   muon->get_pz(0,itrk));
    nt_vars[5] = get_eta(muon->get_px(0,itrk),
			 muon->get_py(0,itrk),
			 muon->get_pz(0,itrk));
    nt_vars[6] = muon->get_chisquare(itrk);
    nt_vars[7] = muon->get_charge(itrk);
    nt_vars[8] = muon->get_xpos(0,itrk);
    nt_vars[9] = muon->get_ypos(0,itrk);
    nt_vars[10] = muon->get_zpos(0,itrk);
    for(int k = 0; k < 3; k++) {
      nt_vars[11+k]=muon->get_muIDOOhits(k,itrk);
      nt_vars[14+k]=muon->get_muIDOO_gap0(0,k,itrk);
      nt_vars[17+k]=muon->get_muIDOO_gap0(1,k,itrk);
      nt_vars[20+k]=muon->get_muIDOO_gap0(2,k,itrk);
    }
    nt_vars[23] = muon->get_muTRhits(itrk);
    nt_vars[24] = muon->get_TMutTrk_status(itrk);
    nt_vars[25] = ievt;
    nt_vars[26] = global->getBbcZVertex();
    nt_vars[27] = global->getBbcChargeN();
    nt_vars[28] = global->getBbcChargeS();
    nt_vars[29] = global->getZdcEnergyN();
    nt_vars[30] = global->getZdcEnergyS();
    nt_vars[31] = global->getCentralitybyClock();
    nt_vars[32] = float(is_mini_bias);
    if(is_2D_south_live) nt_vars[33] = 1;
    if(is_2D_north_live) nt_vars[33] = 2;
    if(is_2D_south_live&&is_2D_north_live) nt_vars[33] = 3;
    if(is_2D_south_scaled) nt_vars[34] = 1;
    if(is_2D_north_scaled) nt_vars[34] = 2;
    if(is_2D_south_scaled&&is_2D_north_scaled) nt_vars[34] = 3;
    if(is_1D1S_south_live) nt_vars[35] = 1;
    if(is_1D1S_north_live) nt_vars[35] = 2;
    if(is_1D1S_south_live&&is_1D1S_north_live) nt_vars[35] = 3;
    if(is_1D1S_south_scaled) nt_vars[36] = 1;
    if(is_1D1S_north_scaled) nt_vars[36] = 2;
    if(is_1D1S_south_scaled&&is_1D1S_north_scaled) nt_vars[36] = 3;
    nt_vars[37] = float(triglvl1->get_lvl1_triglive());
    nt_vars[38] = float(triglvl1->get_lvl1_trigscaled());
    nt_vars[39] = global->getRunNumber();
    nt_vars[40] = global->getEventNumber();
    nt3->Fill(nt_vars);
   } 
}

void 
fill_mut(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt)
{
  // decide Lvl1 trigger information based on lvl1 bit.
  //
  // use an arry to store the bit position of 
  // minibias trigger ------- first element of the arry.
  // south 2D trigger ------- second element of the arry.
  // south DS trigger ------- third element of the arry.
  // north 2D trigger ------- fourth element of the arry.
  // south DS trigger ------- fifth element of the arry.
  // 
  // One has to been careful because the position has been changed during
  // data taking, the arry filled below is according to run 80312.
  //
  ULong_t bit_position[5] = {0};
  // PPRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 87791 && global->getRunNumber() <=88260) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00100000;
    bit_position[4] = 0x00020000;
  }
  if(global->getRunNumber() >= 88350 && global->getRunNumber() <= 92446) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00200000;
    bit_position[4] = 0x00040000;
  }
  // dAuRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 78269 && global->getRunNumber() <=80312) {
    bit_position[0] = 0x00000004;
    bit_position[1] = 0x00000010;
    bit_position[2] = 0x00080000;
    bit_position[3] = 0x00040000;
    bit_position[4] = 0x00800000;
  }
  if(global->getRunNumber() >= 77530 && global->getRunNumber() <=78213) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00080000;
    bit_position[4] = 0x00800000;
  }
  

  unsigned short is_mini_bias         = 0;
  unsigned short is_2D_south_scaled   = 0;
  unsigned short is_2D_south_live     = 0;
  unsigned short is_1D1S_south_scaled = 0;
  unsigned short is_1D1S_south_live   = 0;
  unsigned short is_2D_north_scaled   = 0;
  unsigned short is_2D_north_live     = 0;
  unsigned short is_1D1S_north_scaled = 0;
  unsigned short is_1D1S_north_live   = 0;

  if((triglvl1->get_lvl1_trigscaled())&bit_position[0])
    is_mini_bias = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[1])
    is_2D_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[1])
    is_2D_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[2])
    is_1D1S_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[2])
    is_1D1S_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[3])
    is_2D_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[3])
    is_2D_north_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[4])
    is_1D1S_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[4])
    is_1D1S_north_live = 1;

  // Cache format so we can restore
  //

  int ndimu = muon->get_ndimu();
  for (int idimu=0; idimu<muon->get_ndimu(); idimu++){
    
    float nt_vars[100]={0.0};

    int index_trk1 = muon->get_ditrkIndex(0,idimu);
    int index_trk2 = muon->get_ditrkIndex(1,idimu);
    
    if (muon->get_ghostflag(index_trk1) || muon->get_ghostflag(index_trk2)) continue;
    
    // std::cout << " the chi_square for this vtx is " << muon->get_vtx_chisquare(idimu) << endl;

    nt_vars[0]  = muon->get_dipx(idimu);
    nt_vars[1]  = muon->get_dipy(idimu);
    nt_vars[2]  = muon->get_dipz(idimu);
    nt_vars[3]  = muon->get_dimass(idimu);
    //    nt_vars[4]  = muon->get_vtx_chisquare(idimu);
    nt_vars[5]  = muon->get_dicharge(idimu);
    nt_vars[6]  = muon->get_vtx_xpos(idimu);
    nt_vars[7]  = muon->get_vtx_ypos(idimu);
    nt_vars[8]  = muon->get_vtx_zpos(idimu);

    nt_vars[9]  = muon->get_px(0,index_trk1);
    nt_vars[10] = muon->get_py(0,index_trk1);
    nt_vars[11] = muon->get_pz(0, index_trk1);
    nt_vars[12] = muon->get_chisquare(index_trk1);
    nt_vars[13] = muon->get_PID(index_trk1);
    for(int k = 0; k < 3; k++) {
      nt_vars[14+k]=muon->get_muIDhits(index_trk1);
      nt_vars[17+k]=muon->get_muID_gap0(0,index_trk1);
      nt_vars[20+k]=muon->get_muID_gap0(1,index_trk1);
      nt_vars[23+k]=muon->get_muID_gap0(2,index_trk1);
    }
    nt_vars[26] = muon->get_xpos(0,index_trk1);
    nt_vars[27] = muon->get_ypos(0,index_trk1);
    nt_vars[28] = muon->get_zpos(0,index_trk1);
    nt_vars[29] = muon->get_muTRhits(index_trk1);
    nt_vars[30] = muon->get_TMutTrk_status(index_trk1);

    nt_vars[31] = muon->get_px(0, index_trk2);
    nt_vars[32] = muon->get_py(0, index_trk2);
    nt_vars[33] = muon->get_pz(0, index_trk2);
    nt_vars[34] = muon->get_chisquare(index_trk2);
    nt_vars[35] = muon->get_PID(index_trk2);
    for(int k = 0; k < 3; k++) {
      nt_vars[36+k]=muon->get_muIDhits(index_trk2);
      nt_vars[39+k]=muon->get_muID_gap0(0,index_trk2);
      nt_vars[42+k]=muon->get_muID_gap0(1,index_trk2);
      nt_vars[45+k]=muon->get_muID_gap0(2,index_trk2);
    }
    nt_vars[48] = muon->get_xpos(0,index_trk2);
    nt_vars[49] = muon->get_ypos(0,index_trk2);
    nt_vars[50] = muon->get_zpos(0,index_trk2);
    nt_vars[51] = muon->get_muTRhits(index_trk2);
    nt_vars[52] = muon->get_TMutTrk_status(index_trk2);
    nt_vars[53] = ievt;
    nt_vars[54] = global->getBbcZVertex();
    nt_vars[55] = global->getBbcChargeN();
    nt_vars[56] = global->getBbcChargeS();
    nt_vars[57] = global->getZdcEnergyN();
    nt_vars[58] = global->getZdcEnergyS();
    nt_vars[59] = global->getCentralitybyClock();
    nt_vars[60] = float(is_mini_bias);
    if(is_2D_south_live) nt_vars[61] = 1;
    if(is_2D_north_live) nt_vars[61] = 2;
    if(is_2D_south_live&&is_2D_north_live) nt_vars[61] = 3;
    if(is_2D_south_scaled) nt_vars[62] = 1;
    if(is_2D_north_scaled) nt_vars[62] = 2;
    if(is_2D_south_scaled&&is_2D_north_scaled) nt_vars[62] = 3;
    if(is_1D1S_south_live) nt_vars[63] = 1;
    if(is_1D1S_north_live) nt_vars[63] = 2;
    if(is_1D1S_south_live&&is_1D1S_north_live) nt_vars[63] = 3;
    if(is_1D1S_south_scaled) nt_vars[64] = 1;
    if(is_1D1S_north_scaled) nt_vars[64] = 2;
    if(is_1D1S_south_scaled&&is_1D1S_north_scaled) nt_vars[64] = 3;
    nt_vars[65] = float(triglvl1->get_lvl1_triglive());
    nt_vars[66] = float(triglvl1->get_lvl1_trigscaled());
    nt_vars[67] = global->getRunNumber();
    nt_vars[68] = global->getEventNumber();
    nt2->Fill(nt_vars);
   } 
}

void 
fill_mut_single(PHMuoTracksOut* muon, PHGlobal* global, TrigLvl1* triglvl1, int ievt)
{

  //  Event * evt = 0;
  //  static TriggerUtilities tu;
  //  TriggerHelper* th=tu.getTriggerHelper(global->getRunNumber(), evt); 

  // decide Lvl1 trigger information based on lvl1 bit.
  //
  // use an arry to store the bit position of 
  // minibias trigger ------- first element of the arry.
  // south 2D trigger ------- second element of the arry.
  // south DS trigger ------- third element of the arry.
  // north 2D trigger ------- fourth element of the arry.
  // south DS trigger ------- fifth element of the arry.
  // 
  // One has to been careful because the position has been changed during
  // data taking, the arry filled below is according to run 80312.
  //
  ULong_t bit_position[5] = {0};

  // PPRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 87791 && global->getRunNumber() <=88260) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00100000;
    bit_position[4] = 0x00020000;
  }
  if(global->getRunNumber() >= 88350 && global->getRunNumber() <= 92446) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00200000;
    bit_position[4] = 0x00040000;
  }
  // dAuRun3 trigger configuration.
  //
  if(global->getRunNumber() >= 78269 && global->getRunNumber() <=80312) {
    bit_position[0] = 0x00000004;
    bit_position[1] = 0x00000010;
    bit_position[2] = 0x00080000;
    bit_position[3] = 0x00040000;
    bit_position[4] = 0x00800000;
  }
  if(global->getRunNumber() >= 77530 && global->getRunNumber() <=78213) {
    bit_position[0] = 0x00000004;
    bit_position[2] = 0x00080000;
    bit_position[4] = 0x00800000;
  }
  
  
  unsigned short is_mini_bias         = 0;
  unsigned short is_2D_south_scaled   = 0;
  unsigned short is_2D_south_live     = 0;
  unsigned short is_1D1S_south_scaled = 0;
  unsigned short is_1D1S_south_live   = 0;
  unsigned short is_2D_north_scaled   = 0;
  unsigned short is_2D_north_live     = 0;
  unsigned short is_1D1S_north_scaled = 0;
  unsigned short is_1D1S_north_live   = 0;

  if((triglvl1->get_lvl1_trigscaled())&bit_position[0])
    is_mini_bias = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[1])
    is_2D_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[1])
    is_2D_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[2])
    is_1D1S_south_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[2])
    is_1D1S_south_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[3])
    is_2D_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[3])
    is_2D_north_live = 1;
  if((triglvl1->get_lvl1_trigscaled())&bit_position[4])
    is_1D1S_north_scaled = 1;
  if((triglvl1->get_lvl1_triglive())&bit_position[4])
    is_1D1S_north_live = 1;

  // Cache format so we can restore
  //

  int ntrk = muon->get_npart();
  for (int itrk=0; itrk<ntrk; itrk++){
    
    float nt_vars[100]={0.0};

    // By defination all mutoo tracks are not ghosts.
    //
    if (muon->get_ghostflag(itrk)) continue;
    
    // First index in get_px gives the location of 
    // where we measured the px, "0" means at vtx.
    //
    nt_vars[0] = muon->get_px(0,itrk);
    nt_vars[1] = muon->get_py(0,itrk);
    nt_vars[2] = muon->get_pz(0, itrk);
    nt_vars[3] = get_phi(muon->get_px(0,itrk),
			 muon->get_py(0,itrk));
    nt_vars[4] = get_theta(muon->get_px(0,itrk),
			   muon->get_py(0,itrk),
			   muon->get_pz(0,itrk));
    nt_vars[5] = get_eta(muon->get_px(0,itrk),
			 muon->get_py(0,itrk),
			 muon->get_pz(0,itrk));
    nt_vars[6] = muon->get_chisquare(itrk);
    nt_vars[7] = muon->get_charge(itrk);
    nt_vars[8] = muon->get_xpos(0,itrk);
    nt_vars[9] = muon->get_ypos(0,itrk);
    nt_vars[10] = muon->get_zpos(0,itrk);
    for(int k = 0; k < 3; k++) {
      nt_vars[11+k]=muon->get_muIDhits(itrk);
      nt_vars[14+k]=muon->get_muID_gap0(0,itrk);
      nt_vars[17+k]=muon->get_muID_gap0(1,itrk);
      nt_vars[20+k]=muon->get_muID_gap0(2,itrk);
    }
    nt_vars[23] = muon->get_muTRhits(itrk);
    nt_vars[24] = muon->get_TMutTrk_status(itrk);
    nt_vars[25] = ievt;
    nt_vars[26] = global->getBbcZVertex();
    nt_vars[27] = global->getBbcChargeN();
    nt_vars[28] = global->getBbcChargeS();
    nt_vars[29] = global->getZdcEnergyN();
    nt_vars[30] = global->getZdcEnergyS();
    nt_vars[31] = global->getCentralitybyClock();
    nt_vars[32] = float(is_mini_bias);
    if(is_2D_south_live) nt_vars[33] = 1;
    if(is_2D_north_live) nt_vars[33] = 2;
    if(is_2D_south_live&&is_2D_north_live) nt_vars[33] = 3;
    if(is_2D_south_scaled) nt_vars[34] = 1;
    if(is_2D_north_scaled) nt_vars[34] = 2;
    if(is_2D_south_scaled&&is_2D_north_scaled) nt_vars[34] = 3;
    if(is_1D1S_south_live) nt_vars[35] = 1;
    if(is_1D1S_north_live) nt_vars[35] = 2;
    if(is_1D1S_south_live&&is_1D1S_north_live) nt_vars[35] = 3;
    if(is_1D1S_south_scaled) nt_vars[36] = 1;
    if(is_1D1S_north_scaled) nt_vars[36] = 2;
    if(is_1D1S_south_scaled&&is_1D1S_north_scaled) nt_vars[36] = 3;
    nt_vars[37] = float(triglvl1->get_lvl1_triglive());
    nt_vars[38] = float(triglvl1->get_lvl1_trigscaled());
    nt_vars[39] = global->getRunNumber();
    nt_vars[40] = global->getEventNumber();
    nt4->Fill(nt_vars);
   } 
}

// calculate phi based on px, py. The return  value is 0-360 degree.
//
float get_phi(float px, float py) {
  if(py > 0 ) return 180.0*atan2(py,px)/M_PI;
  if(py < 0 ) return 180.0*(2.0+atan2(py,px)/M_PI);
}

// calculate theta based on px, py, pz, the return value is 0-180 degree.
//
float get_theta(float px, float py, float pz) {
  float pt = sqrt(px*px+py*py);
  float theta = atan(pt/pz);
  if(pz>0) return theta*180.0/M_PI;
  if(pz<0) return (M_PI+theta)*180/M_PI;
}

//calculate pseudo-rapidity based on px, py, pz.
//
float get_eta(float px, float py, float pz) {
  float ptot = sqrt(px*px+py*py+pz*pz);
  float eta = 0.5*log((ptot+pz)/(ptot-pz));
  return eta;
}




