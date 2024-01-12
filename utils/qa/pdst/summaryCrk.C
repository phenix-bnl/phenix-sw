#include "TFile.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "summaryQA.h"
#include <fstream>
#include <iostream>

using namespace std;
int QASummary::processCrk()
{
  cout << "Crk..." << endl;
fstream textFile(outputName1, ios::in); if (textFile) { textFile.close(); textFile.open(outputName1,ios::app|ios::out); }
fstream statusFile(outputName2, ios::in); if (statusFile) {statusFile.close();  statusFile.open(outputName2,ios::app|ios::out); }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- CRK QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  textFile << "Run: " << runNumber << endl;
//  textFile << endl;

  unsigned int status_word=0;

  int i, j, ch, count;
  float t0,error;
  Float_t toff[5120];
  Int_t reject[5120];
  char title[40];
  Int_t nhit_pmts[5120];

  TH2F *crkt0raw;
  TH1F *crkhit;
  TH1F *crkncrk;
  TH1F *crkpe[32];
  TH1F *crkringz[4];
  TH1F *crkringphi[4];
  TH1F *crkringnpe[4];
  TF1 *crkf1;

  Int_t error_in_t0mean,error_in_t0sigma,error_in_nhits,error_in_nhits2,error_in_gain;

  crkt0raw = (TH2F *) qafile->Get ("crkt0raw");
  crkhit = (TH1F *) qafile->Get ("crkhit");
  crkncrk = (TH1F *) qafile->Get ("crkncrk");

  for(i=0;i<32;i++){
    sprintf(title,"crkpe%d",i);
    crkpe[i] = (TH1F *) qafile->Get(title);
  }

  for(i=0;i<4;i++){
    sprintf(title,"crkringz%d",i);
    crkringz[i] = (TH1F *) qafile->Get(title);
    sprintf(title,"crkringphi%d",i);
    crkringphi[i] = (TH1F *) qafile->Get(title);
    sprintf(title,"crkringnpe%d",i);
    crkringnpe[i] = (TH1F *) qafile->Get(title);
  }

// add here analysis and printout results to textFile

  Float_t mean_ncrk,sigma_ncrk;
  Float_t mean_ringnpe[4],sigma_ringnpe[4];
  Float_t mean_ringphi[4],sigma_ringphi[4];
  Float_t mean_ringz[4],sigma_ringz[4];
  Float_t nrings[4];

  mean_ncrk = crkncrk->GetMean(); sigma_ncrk = crkncrk->GetRMS();

// cout << "Mean_of_RICH_hits_per_event: " << mean_ncrk << endl;
  textFile << "Mean_of_RICH_hits_per_event: " << mean_ncrk << endl;

// cout << "Sigma_of_RICH_hits_per_event: " << sigma_ncrk << endl;
// cout << endl;

  textFile << "Sigma_of_RICH_hits_per_event: " << sigma_ncrk << endl;
//  textFile << endl;

  for(i=0;i<4;i++){
    nrings[i] = crkringnpe[i]->GetEntries();
    mean_ringnpe[i] = crkringnpe[i]->GetMean();
    sigma_ringnpe[i] = crkringnpe[i]->GetRMS();
    mean_ringphi[i] = crkringphi[i]->GetMean();
    sigma_ringphi[i] = crkringphi[i]->GetRMS();
    mean_ringz[i] = crkringz[i]->GetMean();
    sigma_ringz[i] = crkringz[i]->GetRMS();
  }

  for(i=0;i<4;i++){
//    cout << "Mean_of_RICH_rings_at_Sector_" << i << ": " << nrings[i] << endl;
    textFile << "Mean_of_RICH_rings_at_Sector_" << i << ": " << nrings[i] << endl;
  }
  for(i=0;i<4;i++){
//    cout << "Mean_of_Z_position_of_RICH_ring_at_Sector_" << i << ": " << mean_ringz[i] << endl;
    textFile << "Mean_of_Z_position_of_RICH_ring_at_Sector_" << i << ": " << mean_ringz[i] << endl;
  }
  for(i=0;i<4;i++){
//    cout << "Sigma_of_Z_position_of_RICH_ring_at_Sector_" << i << ": " << sigma_ringz[i] << endl;
    textFile << "Sigma_of_Z_position_of_RICH_ring_at_Sector_" << i << ": " << sigma_ringz[i] << endl;
  }
  for(i=0;i<4;i++){
//    cout << "Mean_of_Phi_position_of_RICH_ring_at_Sector_" << i << ": " << mean_ringphi[i] << endl;
    textFile << "Mean_of_Phi_position_of_RICH_ring_at_Sector_" << i << ": " << mean_ringphi[i] << endl;
  }
  for(i=0;i<4;i++){
//    cout << "Sigma_of_Phi_position_of_RICH_ring_at_Sector_" << i << ": " << sigma_ringphi[i] << endl;
    textFile << "Sigma_of_Phi_position_of_RICH_ring_at_Sector_" << i << ": " << sigma_ringphi[i] << endl;
  }
  for(i=0;i<4;i++){
//    cout << "Mean_of_NPE_of_RICH_ring_at_Sector_" << i << ": " << mean_ringnpe[i] << endl;
    textFile << "Mean_of_NPE_of_RICH_ring_at_Sector_" << i << ": " << mean_ringnpe[i] << endl;
  }
  for(i=0;i<4;i++){
//    cout << "Sigma_of_NPE_of_RICH_ring_at_Sector_" << i << ": " << sigma_ringnpe[i] << endl;
    textFile << "Sigma_of_NPE_of_RICH_ring_at_Sector_" << i << ": " << sigma_ringnpe[i] << endl;
  }
//  cout << endl;
//  textFile << endl;

  //
  // First, we will read rejected channel map
  //
  for(i=0;i<5120;i++) toff[i]=-1000;
  for(i=0;i<5120;i++) reject[i]=0;
  ifstream fin("crk_t0_offset_Run3dAu.txt");
  while(fin >>ch >> count >> t0 >> error ) toff[ch]=t0;
  fin.close();
//  fin.open("crk_reject_map.txt");
  fin.open("crk_reject_map_Run3dAu.txt");
  while(fin >>ch ){reject[ch]=1;}
  fin.close();

  // 
  // Now comes to the analysis..
  //

  //
  // First check is the number of PMT hits over entire PMTs
  //
  for(i=0;i<5120;i++){
    if(!reject[i]) nhit_pmts[i]=(Int_t)crkhit->GetBinContent(i+1);
    else nhit_pmts[i]= -1000;
  }

  int nhitmax=0;
  for(i=0;i<5120;i++)if(!reject[i]&&nhit_pmts[i]>nhitmax)nhitmax=nhit_pmts[i];

  TH1F *ana_nhit = new TH1F("ana_nhit","ana_nhit",nhitmax+10,0,nhitmax+10);
  for(i=0;i<5120;i++) if(!reject[i]) ana_nhit->Fill(nhit_pmts[i]);

  Float_t nhit_mean = ana_nhit->GetMean();
  Float_t nhit_sigma = ana_nhit->GetRMS();
  crkf1 = new TF1("crkf1","gaus",nhit_mean-3*nhit_sigma,nhit_mean+3*nhit_sigma);
  crkf1->SetParameter(0,ana_nhit->GetEntries());
  crkf1->SetParameter(1,nhit_mean);
  crkf1->SetParameter(2,nhit_sigma);

  ana_nhit->Fit("crkf1","RQN");

  nhit_mean= crkf1->GetParameter(1); nhit_sigma= crkf1->GetParameter(2);
//  cout << "Mean_of_Hits_in_a_PMT: " << nhit_mean << endl;
  textFile << "Mean_of_Hits_in_a_PMT: " << nhit_mean << endl;
//  cout << "Sigma_of_Hits_in_a_PMT: " << nhit_sigma << endl;
//  cout << endl;
  textFile << "Sigma_of_Hits_in_a_PMT: " << nhit_sigma << endl;
//  textFile << endl;
  // delete crkf1;

  int error_pmt=0;
  for(i=0;i<320;i++){
    Float_t eva_nhit=0.0;
    Int_t eva_count=0;
    for(j=0;j<16;j++){
      if(!reject[i*16+j]){
        eva_nhit+=nhit_pmts[i*16+j];
        eva_count++;
      }
    }
    if(eva_count>2 && (eva_nhit/eva_count<nhit_mean-2*nhit_sigma || eva_nhit/eva_count>nhit_mean+2*nhit_sigma)) error_pmt++;
  }

//  cout << "Number_of_Error_pmt_section_in_nHits: " << error_pmt << endl;
//  cout << endl;
  textFile << "Number_of_Error_pmt_section_in_nHits: " << error_pmt << endl;
//  textFile << endl;
  float error_pmt_all = (float)error_pmt; 

  if(error_pmt<=1) {error_in_nhits=0;}
  else {
	  if(error_pmt>255) {error_in_nhits=255;} else {error_in_nhits=error_pmt;}
  }
  
  status_word = status_word | (error_in_nhits << 4);

  // Another test of hit pattern check. so that the bad address
  // mapping data will be rescued from rejection.
  //
  TH1F *ana_nhit2 = new TH1F("ana_nhit2","ana_nhit2",nhitmax+10,0,nhitmax+10);
  for(i=0;i<5120;i++) if(!reject[i] && (i<3040 || i>=3359) && (i<4320 || i>=4640)) ana_nhit2->Fill(nhit_pmts[i]);

  Float_t nhit2_mean = ana_nhit2->GetMean();
  Float_t nhit2_sigma = ana_nhit2->GetRMS();
  crkf1 = new TF1("crkf1","gaus",nhit2_mean-3*nhit2_sigma,nhit2_mean+3*nhit2_sigma);
  crkf1->SetParameter(0,ana_nhit2->GetEntries());
  crkf1->SetParameter(1,nhit2_mean);
  crkf1->SetParameter(2,nhit2_sigma);

  ana_nhit2->Fit("crkf1","RQN");

  nhit2_mean= crkf1->GetParameter(1); nhit2_sigma= crkf1->GetParameter(2);
//  cout << "Mean_of_Hits_in_a_PMT_in_limited: " << nhit2_mean << endl;
  textFile << "Mean_of_Hits_in_a_PMT_in_limited: " << nhit2_mean << endl;
//  cout << "Sigma_of_Hits_in_a_PMT_in_limited: " << nhit2_sigma << endl;
//  cout << endl;
  textFile << "Sigma_of_Hits_in_a_PMT_in_limited: " << nhit2_sigma << endl;
//  textFile << endl;
  // delete crkf1;

  error_pmt=0;
  for(i=0;i<320;i++){
    if((i>=190 && i<210) || (i>=270 && i<290)) continue;
    Float_t eva_nhit=0.0;
    Int_t eva_count=0;
    for(j=0;j<16;j++){
      if(!reject[i*16+j]){
        eva_nhit+=nhit_pmts[i*16+j];
        eva_count++;
      }
    }
    if(eva_count>2 && (eva_nhit/eva_count<nhit_mean-2*nhit_sigma || eva_nhit/eva_count>nhit_mean+2*nhit_sigma)) error_pmt++;
  }

//  cout << "Number_of_Error_pmt_section_in_nHits_limited: " << error_pmt << endl;
//  cout << endl;
  textFile << "Number_of_Error_pmt_section_in_nHits_limited: " << error_pmt << endl;
//  textFile << endl;

  float error_pmt_limited = (float)error_pmt;

  if(error_pmt<=1) {error_in_nhits2=0;}
  else {
	  if(error_pmt>255) {error_in_nhits2=255;} else {error_in_nhits2=error_pmt;}
  }
  
  //
  // Next check is the t0 position and width over entire PMTs
  //
  TH1F *ana_t0mean = new TH1F("ana_t0mean","ana_t0mean",4000,-100,100);
  TH1F *ana_t0sigma = new TH1F("ana_t0sigma","ana_t0sigma",800,0,40);
  TH1D *t0pmts;
  Float_t crkt0mean[5120],t0sigma[5120];

  cout << "Staring loop over 5120 PMT..." << endl;

  Int_t PMTcount=0;
  for(i=0;i<5120;i++){
    t0pmts=crkt0raw->ProjectionY("proj",i+1,i+1);
    Float_t temp_t0mean = t0pmts->GetMean();
    Float_t temp_t0sigma = t0pmts->GetRMS();
    Float_t temp_count = t0pmts->GetEntries();
    if(toff[i]>-800 && temp_count>8){
      crkt0mean[i]=temp_t0mean;
      ana_t0mean->Fill(crkt0mean[i]-toff[i]);
      t0sigma[i]=temp_t0sigma;
      ana_t0sigma->Fill(t0sigma[i]);
      PMTcount++;
    }
    // delete t0pmts;
    if((i+1)%10==0) cout << "PMT # " << i << endl;
  }

  cout << "   ...loop ended." << endl;
  //
  // Check the mean of t0
  Float_t tempall_t0mean,tempall_t0sigma;

  tempall_t0mean = ana_t0mean->GetMean();
  tempall_t0sigma = ana_t0mean->GetRMS();
  crkf1 = new TF1("crkf1","gaus",tempall_t0mean-3*tempall_t0sigma,tempall_t0mean+3*tempall_t0sigma);
  crkf1->SetParameter(0,ana_t0mean->GetEntries());
  crkf1->SetParameter(1,tempall_t0mean);
  crkf1->SetParameter(2,tempall_t0sigma);

  ana_t0mean->Fit("crkf1","RQN");

  tempall_t0mean = crkf1->GetParameter(1); tempall_t0sigma= crkf1->GetParameter(2);
//  cout << "Mean_of_t0_mean_in_a_PMT: " << tempall_t0mean << endl;
  textFile << "Mean_of_t0_mean_in_a_PMT: " << tempall_t0mean << endl;
//  cout << "Sigma_of_t0_mean_in_a_PMT: " << tempall_t0sigma << endl;
//  cout << endl;
  textFile << "Sigma_of_t0_mean_in_a_PMT: " << tempall_t0sigma << endl;
//  textFile << endl;
  // delete crkf1;

  error_pmt=0;
  for(i=0;i<320;i++){
    Float_t eva_mean=0.0;
    Int_t eva_count=0;
    for(j=0;j<16;j++){
      if(toff[i*16+j]>-800){
        eva_mean+=crkt0mean[i*16+j];
        eva_count++;
      }
    }
    if(eva_count>3 && (eva_mean/eva_count<tempall_t0mean-3*tempall_t0sigma || eva_mean/eva_count>tempall_t0mean+3*tempall_t0sigma)) error_pmt++;
  }

//  cout << "Number_of_Error_pmt_section_in_t0_mean: " << error_pmt << endl;
//  cout << endl;
  textFile << "Number_of_Error_pmt_section_in_t0_mean: " << error_pmt << endl;
//  textFile << endl;
  if(error_pmt>30) error_in_t0mean=1; else error_in_t0mean=0;

  float error_pmt_t0 = (float)error_pmt;
  //
  // Check the width of t0
  tempall_t0mean = ana_t0sigma->GetMean();
  tempall_t0sigma = ana_t0sigma->GetRMS();
  crkf1 = new TF1("crkf1","gaus",tempall_t0mean-3*tempall_t0sigma,tempall_t0mean+3*tempall_t0sigma);
  crkf1->SetParameter(0,ana_t0sigma->GetEntries());
  crkf1->SetParameter(1,tempall_t0mean);
  crkf1->SetParameter(2,tempall_t0sigma);

  ana_t0sigma->Fit("crkf1","RQN");

  tempall_t0mean = crkf1->GetParameter(1); tempall_t0sigma= crkf1->GetParameter(2);
//  cout << "Mean_of_t0_sigma_in_a_PMT: " << tempall_t0mean << endl;
  textFile << "Mean_of_t0_sigma_in_a_PMT: " << tempall_t0mean << endl;
//  cout << "Sigma_of_t0_sigma_in_a_PMT: " << tempall_t0sigma << endl;
//  cout << endl;
  textFile << "Sigma_of_t0_sigma_in_a_PMT: " << tempall_t0sigma << endl;
//  textFile << endl;
  // delete crkf1;

  error_pmt=0;
  for(i=0;i<320;i++){
    Float_t eva_sigma=0.0;
    Int_t eva_count=0;
    for(j=0;j<16;j++){
      if(toff[i*16+j]>-800){
        eva_sigma+=t0sigma[i*16+j];
        eva_count++;
      }
    }
    if(eva_count>3&&(eva_sigma/eva_count<tempall_t0mean-3*tempall_t0sigma || eva_sigma/eva_count>tempall_t0mean+3*tempall_t0sigma)) error_pmt++;
  }

//  cout << "Number_of_Error_pmt_section_in_t0_sigma: " << error_pmt << endl;
//  cout << endl;
  textFile << "Number_of_Error_pmt_section_in_t0_sigma: " << error_pmt << endl;
//  textFile << endl;

  float error_pmt_t0sigma = (float)error_pmt;

  if(error_pmt>30) error_in_t0sigma=1; else error_in_t0sigma=0;

  //
  // Check the gain variation of charge
  // 
  error_pmt=0;
  for(i=0;i<32;i++){
    crkf1 = new TF1("crkf1","gaus",0.70,1.5);
    crkf1->SetParameter(0,crkpe[i]->GetEntries());
    crkf1->SetParameter(1,crkpe[i]->GetMean());
    crkf1->SetParameter(2,crkpe[i]->GetRMS());

    crkpe[i]->Fit("crkf1","RQN");
    if(crkf1->GetParameter(1)>1.10 || crkf1->GetParameter(1)<0.90) error_pmt++;
//    cout << "Single-photoelectron_peak_for_part_" << i << ": " << crkf1->GetParameter(1) << endl;
    textFile << "Single-photoelectron_peak_for_part_" << i << ": " << crkf1->GetParameter(1) << endl;
    // delete crkf1;
  }
//  cout << "Number_of_Error_pmt_in_charge_gain: " << error_pmt << endl;
  textFile << "Number_of_Error_pmt_in_charge_gain: " << error_pmt << endl;

  status_word = status_word | (error_pmt << 12);
  error_in_gain=error_pmt;

//
// add code to determine crk status 
//

  if(error_in_gain>0 || error_in_nhits>0){
    if(error_in_gain==2 && error_in_nhits2==0) status_word = status_word | 2;
    else status_word = status_word | 1;
  }
  else status_word  = status_word | 0;


//
// If the statistics is too low, QA must not be reliable
//
  if(nhit_mean<14){
//    cout << "!! Number of hits per PMT is less than 20 !!"<< endl;
//    cout << "Please run another " << 20.0/((Float_t)nhit_mean) << " times events to obtain a concrete QA result" << endl;
//    textFile << endl;
    textFile << "Statistics: Low"<< endl;
    textFile << "Run " << 14.0/((Float_t)nhit_mean) << " times_events_to_obtain_a_reliable_QA_result!" << endl;
    status_word = status_word | 4;

    textFile << "CRK_Status: " << status_word << endl;
    
    statusFile << status_word << " " ;
    return 0;
  }
  else{
//    textFile << endl;
    textFile << "Statistics: Good"<< endl;
    textFile << "Run 1 times_events_to_obtain_a_reliable_QA_result!" << endl;
  }

//  cout << "CRK Status = " << status_word << endl;
//  textFile<< endl;
  textFile << "CRK_Status: " << status_word << endl;
  
  statusFile << status_word << " " ;
  
  CommitToQADatabase("Crk", "Hits per PMT", nhit_mean, nhit_sigma);
  CommitToQADatabase("Crk", "N. PMT nhits error", (float)error_pmt_all, nhit_sigma);
  CommitToQADatabase("Crk", "Hits per PMT in limited PMTs", nhit2_mean, nhit2_sigma);
  CommitToQADatabase("Crk", "No. limited PMT NHits errors", (float)error_pmt_limited, 0.0);
  CommitToQADatabase("Crk", "PMT T0", tempall_t0mean, tempall_t0sigma);
  CommitToQADatabase("Crk", "No. PMTs with T0 errors", (float)error_pmt_t0, 0.0);
  CommitToQADatabase("Crk", "No. PMTs with sigma errors", (float)error_pmt_t0sigma, 0.0);
  CommitToQADatabase("Crk", "No. PMTs with charge gain errors", (float)error_in_gain, 0.0);
  
  cout << "    ...done." << endl;
  return 0;
}
