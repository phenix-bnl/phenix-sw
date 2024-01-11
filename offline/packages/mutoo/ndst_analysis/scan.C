#ifndef scan_cxx
#define scan_cxx
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
#include "PHMuoTracksOut.h" 
#include "MUTOO.h"

// SL
//
#include<fstream>
#include<string>
#include<iostream>
#include<iomanip>

#endif

void PRINT(std::ostream& os, std::string message);
void dump_dimuons(PHMuoTracksOut* muon);
void dump_single_tracks(PHMuoTracksOut* muon);

int 
main(int argc,char **argv)
{


  if(argc==1 || argc>2) {
    std::cout << "usage: scan [MWG ndst file]" << std::endl;
    return 0;
  }

  // existence check on input file
  //
  ifstream file;
  std::string filename;
  file.open(argv[1],std::ios::in);
  if(!file.is_open()) {
    std::cout << "can't open input file " << argv[1] << std::endl;
    return 0;
  } else {    
    filename = std::string(argv[1]);
    file.close();
  }

  TROOT root("Scan","Version");
  TApplication MyApp("Scan",&argc,argv);
  gSystem->Load("libndst.so");
  
  PHMuoTracksOut* mut=0;  
  PHMuoTracksOut* mutoo = 0; 

  TChain* T = new TChain("T",""); 
  T->Add(filename.c_str()); 
  T->SetBranchAddress("DST/PHMuoTracks", &mut);
  T->SetBranchAddress("DST/PHMuoTracksOO", &mutoo);

  int entry_read=0, ent_read=0, nentries=(int)T->GetEntries(); 

  for(int ievt=0; ievt<nentries; ievt++) { 

    T->GetEntry(ievt);

    // MUTOO single muons
    //
    PRINT(std::cout,"mutoo");
    dump_single_tracks(mutoo);
    PRINT(std::cout,"**");    

    // MUT single muons
    //
    PRINT(std::cout,"mut");
    dump_single_tracks(mut);
    PRINT(std::cout,"**");    

    // MUTOO dimuons
    //
    PRINT(std::cout,"mutoo di-muons");
    dump_dimuons(mutoo);
    PRINT(std::cout,"**");    
    
    // MUT dimuons
    //
    PRINT(std::cout,"mut di-muons");
    dump_dimuons(mut);
    PRINT(std::cout,"**");    

  } 
}

void 
PRINT(std::ostream& os, std::string message){
  const int max_col=80;
  if(!message.size()) {
    os << std::string(max_col,'-') << std::endl;
    return;
  }
  int fill = max_col - message.size() - 2;
  int pre = static_cast<int>(std::floor(fill/2.0));
  int post = fill - pre;
  os << std::string(pre,'-') << " ";
  os << message << " ";
  os << std::string(post,'-') << std::endl;  
}

void 
dump_dimuons(PHMuoTracksOut* muon)
{
  // Cache format so we can restore
  //
  ios::fmtflags old = std::cout.flags();
  std::cout << std::setprecision(3) << std::setiosflags(ios::showpoint); 
  
  int ndimu = muon->get_ndimu();
  for (int idimu=0; idimu<muon->get_ndimu(); idimu++){
    
    int index_trk1 = muon->get_ditrkIndex(0,idimu);
    int index_trk2 = muon->get_ditrkIndex(1,idimu);
    
    if (muon->get_ghostflag(index_trk1) || muon->get_ghostflag(index_trk2)) continue;
    
    Float_t p0[3]={muon->get_px(0,index_trk1), muon->get_py(0,index_trk1), muon->get_pz(0,index_trk1)};
    Float_t p1[3]={muon->get_px(0,index_trk2), muon->get_py(0,index_trk2), muon->get_pz(0,index_trk2)};
    
    if(muon->get_dicharge(idimu)==0) std::cout << "sign +-" << " ";
    if(muon->get_dicharge(idimu)==1) std::cout << "sign ++" << " ";
    if(muon->get_dicharge(idimu)==-1) std::cout << "sign --" << " ";      

    std::cout << " mass:" << muon->get_dimass(idimu) << " ";
    std::cout << " p trk1 = {" << p0[0] << "," << p0[1] << "," << p0[2] << "}";
    std::cout << " p trk2 = {" << p1[0] << "," << p1[1] << "," << p1[2] << "}" << std::endl;
  } 
  // Reset old format
  //
  std::cout.flags(old);
}

void 
dump_single_tracks(PHMuoTracksOut* muon)
{  
  // Cache format so we can restore
  //
  ios::fmtflags old = std::cout.flags();

  std::cout << std::setprecision(3) << std::setiosflags(ios::showpoint);

  for (int itrk=0; itrk<muon->get_npart(); itrk++){

    // Index, Charge, Chi-Square
    //
    char chrg = (muon->get_charge(itrk)>0) ? '+' : '-';
    std::cout << "index: " << 
      itrk << "  " << "charge:" << chrg << " chi:" << muon->get_chisquare(itrk) << " ";

    // Postition at vertex
    //
    std::cout << " r = {" << 
      muon->get_xpos(0,itrk) << "," << 
      muon->get_ypos(0,itrk) << "," << 
      muon -> get_zpos(0,itrk) << "}";
    
    // Momentum at vertex
    //
    std::cout << " p = {" << 
      muon->get_px(0,itrk) << "," << 
      muon->get_py(0,itrk) << "," << 
      muon->get_pz(0,itrk) << "}" << std::endl;    
  } 
  std::cout.flags(old);
}





