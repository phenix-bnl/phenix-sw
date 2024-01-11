#include <cstdio>
#include <iostream>
#include <fstream>
#include <TFile.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TTree.h>
#include <TChain.h>

#include <TSystem.h>
#include <TStyle.h>
#include <TVector3.h>

#include "TestAnalysis.h"

TestAnalysis::TestAnalysis()
{
   m_h1_ert_check = 0;
   m_h2_gl1_check = 0;
}

TestAnalysis::~TestAnalysis()
{
   if (m_h1_ert_check) delete m_h1_ert_check;
   if (m_h2_gl1_check) delete m_h2_gl1_check;

   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t TestAnalysis::SetTree(TTree* tree)
{
//     if (fChain) delete fChain->GetCurrentFile();
   fChain = tree;
   fCurrent = -1;
   
//   fChain->SetBranchAddress("run",&m_run);
////   fChain->SetBranchAddress("evt",&m_evt);
////     fChain->SetBranchAddress("bbct0",&m_bbct0);
//   fChain->SetBranchAddress("trig_4x4c",&m_trig_4x4c);
//   fChain->SetBranchAddress("trig_4x4a",&m_trig_4x4a);
//   fChain->SetBranchAddress("trig_bbc",&m_trig_bbc);
//
//   fChain->SetBranchAddress("n_photon",&m_n_photon);
//   fChain->SetBranchAddress("armsect",m_armsect);
//   fChain->SetBranchAddress("ypos",m_ypos);
//   fChain->SetBranchAddress("zpos",m_zpos);
//   if (m_bl_ene_corr) {
//      fChain->SetBranchAddress("multiplicity",m_multiplicity);
//      fChain->SetBranchAddress("multiplicity_total",&m_multiplicity_total);
//      fChain->SetBranchAddress("towerid",m_towerid);
//      fChain->SetBranchAddress("partesum",m_partesum);
//      fChain->SetBranchAddress("e",m_e);
//   }
//   fChain->SetBranchAddress("ecore",m_ecore);
//   fChain->SetBranchAddress("x",m_x);
//   fChain->SetBranchAddress("y",m_y);
//   fChain->SetBranchAddress("z",m_z);
   
   return 1;
}

Int_t TestAnalysis::GetEntry(Long64_t entry)
{
// Read contents of entry.
//   if (!fChain) return 0;
//   return fChain->GetEntry(entry);
   return 0;
}

Long64_t TestAnalysis::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
//   if (!fChain) return -5;
//   Long64_t centry = fChain->LoadTree(entry);
//   if (centry < 0) return centry;
//   if (fChain->IsA() != TChain::Class()) return centry;
//   TChain *chain = (TChain*)fChain;
//   if (chain->GetTreeNumber() != fCurrent) {
//      fCurrent = chain->GetTreeNumber();
//   }
//   return centry;
   return 0;
}

void TestAnalysis::ProcessInput(char* ifname)
{
//   SetTree(tree);
//   if (fChain == 0) return;

//   Long64_t nentries = (Long64_t)fChain->GetEntriesFast();
//   Int_t nbytes = 0, nb = 0;
//   for (Long64_t jentry = 0; jentry < nentries; jentry++) {
//      Long64_t ientry = LoadTree(jentry);
//      if (ientry < 0) break;
//      nb = fChain->GetEntry(jentry);   nbytes += nb;
//   }
   TFile* ifile = new TFile(ifname);
   TH1D* h1_ert_check = (TH1D*)ifile->Get("h1_ert_check");
   TH2D* h2_gl1_check = (TH2D*)ifile->Get("h2_gl1_check");

   if (! m_h1_ert_check) {
      gROOT->cd();
      m_h1_ert_check = (TH1D*)h1_ert_check->Clone("h1_ert_check");
      m_h2_gl1_check = (TH2D*)h2_gl1_check->Clone("h2_gl1_check");
   } else {
      m_h1_ert_check->Add(h1_ert_check);
      m_h2_gl1_check->Add(h2_gl1_check);
   }
   delete ifile;
}

void TestAnalysis::Analysis()
{
   TFile* ofile = new TFile("test_dst.root", "RECREATE");
   ofile->cd();
   m_h1_ert_check->Write();
   m_h2_gl1_check->Write();
   delete ofile;
}
