//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Jan  4 02:05:57 2001 by ROOT version 2.23/12)
//   from TTree nt_emc/EMCAL clusters
//   found on file: uDST_data05/run-0000012399-0000.root
//////////////////////////////////////////////////////////


#ifndef nt_emc_h
#define nt_emc_h

#if !defined(__CINT__) || defined(__MAKECINT__)
#include <stdio.h>
#include <iostream>
#include <TTree.h>
#include <TFile.h>
#include <TROOT.h>
#endif
#include "ClustTr.hh"

class nt_emc { //: public TTree {
   public :
   ClustTr        clt;

   TTree          *fTree;    //pointer to the analyzed TTree or TChain
   TTree          *fCurrent; //pointer to the current TTree
//Declaration of leaves types
   Float_t         run;
   Float_t         seq;
   Float_t         evt;
   Float_t         trig;
   Float_t         zdcz;
   Float_t         zdce0;
   Float_t         zdce1;
   Float_t         zdct0;
   Float_t         bbcn;
   Float_t         bbcs;
   Float_t         bbcqn;
   Float_t         bbcqs;
   Float_t         bbcz;
   Float_t         bbct0;
   Float_t         ndc;
   Float_t         ndchit;
   Float_t         npc1;
   Float_t         npc3;
   Float_t         ntec;
   Float_t         nemc;
   Float_t         ntof;
   Float_t         ncrk;
   Float_t         ndcw;
   Float_t         ndchitw;
   Float_t         npc1w;
   Float_t         nemcw;
   Float_t         etotw;
   Float_t         etote;
   Float_t         emcx;
   Float_t         emcy;
   Float_t         emcz;
   Float_t         swkey;
   Float_t         e;
   Float_t         ecore;
   Float_t         ecorr;
   Float_t         ecent;
   Float_t         emctc;
   Float_t         tmax;
   Float_t         prob;
   Float_t         emct;
   Float_t         tmin;
   Float_t         twrhit;
   Float_t         echi2;
   Float_t         pesum0;
   Float_t         pesum1;
   Float_t         pesum2;
   Float_t         pesum3;

//List of branches
   TBranch        *b_run;
   TBranch        *b_seq;
   TBranch        *b_evt;
   TBranch        *b_trig;
   TBranch        *b_zdcz;
   TBranch        *b_zdce0;
   TBranch        *b_zdce1;
   TBranch        *b_zdct0;
   TBranch        *b_bbcn;
   TBranch        *b_bbcs;
   TBranch        *b_bbcqn;
   TBranch        *b_bbcqs;
   TBranch        *b_bbcz;
   TBranch        *b_bbct0;
   TBranch        *b_ndc;
   TBranch        *b_ndchit;
   TBranch        *b_npc1;
   TBranch        *b_npc3;
   TBranch        *b_ntec;
   TBranch        *b_nemc;
   TBranch        *b_ntof;
   TBranch        *b_ncrk;
   TBranch        *b_ndcw;
   TBranch        *b_ndchitw;
   TBranch        *b_npc1w;
   TBranch        *b_nemcw;
   TBranch        *b_etotw;
   TBranch        *b_etote;
   TBranch        *b_emcx;
   TBranch        *b_emcy;
   TBranch        *b_emcz;
   TBranch        *b_swkey;
   TBranch        *b_e;
   TBranch        *b_ecore;
   TBranch        *b_ecorr;
   TBranch        *b_ecent;
   TBranch        *b_emctc;
   TBranch        *b_tmax;
   TBranch        *b_prob;
   TBranch        *b_emct;
   TBranch        *b_tmin;
   TBranch        *b_twrhit;
   TBranch        *b_echi2;
   TBranch        *b_pesum0;
   TBranch        *b_pesum1;
   TBranch        *b_pesum2;
   TBranch        *b_pesum3;

   nt_emc(TTree *tree=0);
   ~nt_emc() {};
   Int_t GetEntry(Int_t entry);
   Int_t GetEntries(){
     int i = (int) fTree->GetEntries();
     return i; };
   Int_t LoadTree(Int_t entry);
   void  Init(TTree *tree);
   void  Loop();
   Bool_t Notify();
   void  Show(Int_t entry = -1);
};

#endif

#ifdef nt_emc_cxx
nt_emc::nt_emc(TTree *tree)
{
   Init(tree);
}


Int_t nt_emc::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fTree) return 0;
   Int_t ibyte = fTree->GetEntry(entry);   
   //--------------------------------------------------------
   clt.Reset();
   // Global Information
   clt.run = (int)run;
   clt.evn = (int)evt;
   clt.zdcz = zdcz;
   clt.zdct0 = zdct0;
   clt.zdcch = zdce0 + zdce1;
   clt.bbcz = bbcz;
   clt.bbct0 = bbct0;
   clt.bbcch = bbcqn + bbcqs;
   clt.bbcnhit = bbcn + bbcs;
   clt.pc1nhit = npc1;
   clt.emcnhit = nemc;
   //
   // EMCal Clustering information
   if( swkey>0 ){
     clt.arm = (int)(swkey / 100000);
     clt.sector = (int)((swkey-100000*clt.arm)/10000);
     clt.ind[1] = (int)((swkey-100000*clt.arm-10000*clt.sector)/100);
     clt.ind[0] = (int)( swkey-100000*clt.arm-10000*clt.sector-100*clt.ind[1]);
   } else {
     clt.arm = -1;
     clt.sector = 0;
     clt.ind[1] = 0;
     clt.ind[0] = 0;
   }

   clt.tof = emct;
   clt.e = e;
   clt.ecent = ecent;
   clt.ecore = ecore;
   clt.ecorr = ecorr;
   clt.tofcorr = echi2;
   clt.chi2 = prob; // FIX.ME!!!!!!!!
   clt.pos[0] = emcx;
   clt.pos[1] = emcy;
   clt.pos[2] = emcz;
   //trc.disp[],padisp[]..
   //--------------------------------------------------------
   return ibyte;
}
Int_t nt_emc::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fTree) return -5;
   Int_t centry = fTree->LoadTree(entry);
   if (centry < 0) return centry;
   if (fTree->GetTree() != fCurrent) {
      fCurrent = fTree->GetTree();
      Notify();
   }
   return centry;
}

void nt_emc::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fTree    = tree;
   fCurrent = 0;

   fTree->SetBranchAddress("run",&run);
   fTree->SetBranchAddress("seq",&seq);
   fTree->SetBranchAddress("evt",&evt);
   fTree->SetBranchAddress("trig",&trig);
   fTree->SetBranchAddress("zdcz",&zdcz);
   fTree->SetBranchAddress("zdce0",&zdce0);
   fTree->SetBranchAddress("zdce1",&zdce1);
   fTree->SetBranchAddress("zdct0",&zdct0);
   fTree->SetBranchAddress("bbcn",&bbcn);
   fTree->SetBranchAddress("bbcs",&bbcs);
   fTree->SetBranchAddress("bbcqn",&bbcqn);
   fTree->SetBranchAddress("bbcqs",&bbcqs);
   fTree->SetBranchAddress("bbcz",&bbcz);
   fTree->SetBranchAddress("bbct0",&bbct0);
   fTree->SetBranchAddress("ndc",&ndc);
   fTree->SetBranchAddress("ndchit",&ndchit);
   fTree->SetBranchAddress("npc1",&npc1);
   fTree->SetBranchAddress("npc3",&npc3);
   fTree->SetBranchAddress("ntec",&ntec);
   fTree->SetBranchAddress("nemc",&nemc);
   fTree->SetBranchAddress("ntof",&ntof);
   fTree->SetBranchAddress("ncrk",&ncrk);
   fTree->SetBranchAddress("ndcw",&ndcw);
   fTree->SetBranchAddress("ndchitw",&ndchitw);
   fTree->SetBranchAddress("npc1w",&npc1w);
   fTree->SetBranchAddress("nemcw",&nemcw);
   fTree->SetBranchAddress("etotw",&etotw);
   fTree->SetBranchAddress("etote",&etote);
   fTree->SetBranchAddress("emcx",&emcx);
   fTree->SetBranchAddress("emcy",&emcy);
   fTree->SetBranchAddress("emcz",&emcz);
   fTree->SetBranchAddress("swkey",&swkey);
   fTree->SetBranchAddress("e",&e);
   fTree->SetBranchAddress("ecore",&ecore);
   fTree->SetBranchAddress("ecorr",&ecorr);
   fTree->SetBranchAddress("ecent",&ecent);
   fTree->SetBranchAddress("emctc",&emctc);
   fTree->SetBranchAddress("tmax",&tmax);
   fTree->SetBranchAddress("prob",&prob);
   fTree->SetBranchAddress("emct",&emct);
   fTree->SetBranchAddress("tmin",&tmin);
   fTree->SetBranchAddress("twrhit",&twrhit);
   fTree->SetBranchAddress("echi2",&echi2);
   fTree->SetBranchAddress("pesum0",&pesum0);
   fTree->SetBranchAddress("pesum1",&pesum1);
   fTree->SetBranchAddress("pesum2",&pesum2);
   fTree->SetBranchAddress("pesum3",&pesum3);
}

Bool_t nt_emc::Notify()
{
//   called by LoadTree when loading a new file
//   get branch pointers
   b_run = fTree->GetBranch("run");
   b_seq = fTree->GetBranch("seq");
   b_evt = fTree->GetBranch("evt");
   b_trig = fTree->GetBranch("trig");
   b_zdcz = fTree->GetBranch("zdcz");
   b_zdce0 = fTree->GetBranch("zdce0");
   b_zdce1 = fTree->GetBranch("zdce1");
   b_zdct0 = fTree->GetBranch("zdct0");
   b_bbcn = fTree->GetBranch("bbcn");
   b_bbcs = fTree->GetBranch("bbcs");
   b_bbcqn = fTree->GetBranch("bbcqn");
   b_bbcqs = fTree->GetBranch("bbcqs");
   b_bbcz = fTree->GetBranch("bbcz");
   b_bbct0 = fTree->GetBranch("bbct0");
   b_ndc = fTree->GetBranch("ndc");
   b_ndchit = fTree->GetBranch("ndchit");
   b_npc1 = fTree->GetBranch("npc1");
   b_npc3 = fTree->GetBranch("npc3");
   b_ntec = fTree->GetBranch("ntec");
   b_nemc = fTree->GetBranch("nemc");
   b_ntof = fTree->GetBranch("ntof");
   b_ncrk = fTree->GetBranch("ncrk");
   b_ndcw = fTree->GetBranch("ndcw");
   b_ndchitw = fTree->GetBranch("ndchitw");
   b_npc1w = fTree->GetBranch("npc1w");
   b_nemcw = fTree->GetBranch("nemcw");
   b_etotw = fTree->GetBranch("etotw");
   b_etote = fTree->GetBranch("etote");
   b_emcx = fTree->GetBranch("emcx");
   b_emcy = fTree->GetBranch("emcy");
   b_emcz = fTree->GetBranch("emcz");
   b_swkey = fTree->GetBranch("swkey");
   b_e = fTree->GetBranch("e");
   b_ecore = fTree->GetBranch("ecore");
   b_ecorr = fTree->GetBranch("ecorr");
   b_ecent = fTree->GetBranch("ecent");
   b_emctc = fTree->GetBranch("emctc");
   b_tmax = fTree->GetBranch("tmax");
   b_prob = fTree->GetBranch("prob");
   b_emct = fTree->GetBranch("emct");
   b_tmin = fTree->GetBranch("tmin");
   b_twrhit = fTree->GetBranch("twrhit");
   b_echi2 = fTree->GetBranch("echi2");
   b_pesum0 = fTree->GetBranch("pesum0");
   b_pesum1 = fTree->GetBranch("pesum1");
   b_pesum2 = fTree->GetBranch("pesum2");
   b_pesum3 = fTree->GetBranch("pesum3");
   return true;
}

void nt_emc::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fTree) return;
   fTree->Show(entry);
}
#endif // #ifdef nt_emc_cxx

