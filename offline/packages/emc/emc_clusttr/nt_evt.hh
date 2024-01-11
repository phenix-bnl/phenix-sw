#ifndef nt_evt_hh
#define nt_evt_hh

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

class nt_evt {
   public :
   TTree          *fChain;   //pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //current Tree number in a TChain
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

   nt_evt(TTree *tree=0);
   ~nt_evt();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef nt_evt_cxx
nt_evt::nt_evt(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("/phenix/data08/microDSTv03_1/ntuple/RCF/run0000012468_0001.root");
      if (!f) {
         f = new TFile("/phenix/data08/microDSTv03_1/ntuple/RCF/run0000012468_0001.root");
      }
      tree = (TTree*)gDirectory->Get("nt_evt");

   }
   Init(tree);
}

nt_evt::~nt_evt()
{
   if (!fChain) return;
}

Int_t nt_evt::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t nt_evt::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Int_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void nt_evt::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;

   fChain->SetBranchAddress("run",&run);
   fChain->SetBranchAddress("seq",&seq);
   fChain->SetBranchAddress("evt",&evt);
   fChain->SetBranchAddress("zdcz",&zdcz);
   fChain->SetBranchAddress("zdce0",&zdce0);
   fChain->SetBranchAddress("zdce1",&zdce1);
   fChain->SetBranchAddress("zdct0",&zdct0);
   fChain->SetBranchAddress("bbcn",&bbcn);
   fChain->SetBranchAddress("bbcs",&bbcs);
   fChain->SetBranchAddress("bbcqn",&bbcqn);
   fChain->SetBranchAddress("bbcqs",&bbcqs);
   fChain->SetBranchAddress("bbcz",&bbcz);
   fChain->SetBranchAddress("bbct0",&bbct0);
   fChain->SetBranchAddress("ndc",&ndc);
   fChain->SetBranchAddress("ndchit",&ndchit);
   fChain->SetBranchAddress("npc1",&npc1);
   fChain->SetBranchAddress("npc3",&npc3);
   fChain->SetBranchAddress("ntec",&ntec);
   fChain->SetBranchAddress("nemc",&nemc);
   fChain->SetBranchAddress("ntof",&ntof);
   fChain->SetBranchAddress("ncrk",&ncrk);
   fChain->SetBranchAddress("ndcw",&ndcw);
   fChain->SetBranchAddress("ndchitw",&ndchitw);
   fChain->SetBranchAddress("npc1w",&npc1w);
   fChain->SetBranchAddress("nemcw",&nemcw);
   fChain->SetBranchAddress("etotw",&etotw);
   fChain->SetBranchAddress("etote",&etote);
   Notify();
}

Bool_t nt_evt::Notify()
{
//   called when loading a new file
//   get branch pointers
   b_run = fChain->GetBranch("run");
   b_seq = fChain->GetBranch("seq");
   b_evt = fChain->GetBranch("evt");
   b_trig = fChain->GetBranch("trig");
   b_zdcz = fChain->GetBranch("zdcz");
   b_zdce0 = fChain->GetBranch("zdce0");
   b_zdce1 = fChain->GetBranch("zdce1");
   b_zdct0 = fChain->GetBranch("zdct0");
   b_bbcn = fChain->GetBranch("bbcn");
   b_bbcs = fChain->GetBranch("bbcs");
   b_bbcqn = fChain->GetBranch("bbcqn");
   b_bbcqs = fChain->GetBranch("bbcqs");
   b_bbcz = fChain->GetBranch("bbcz");
   b_bbct0 = fChain->GetBranch("bbct0");
   b_ndc = fChain->GetBranch("ndc");
   b_ndchit = fChain->GetBranch("ndchit");
   b_npc1 = fChain->GetBranch("npc1");
   b_npc3 = fChain->GetBranch("npc3");
   b_ntec = fChain->GetBranch("ntec");
   b_nemc = fChain->GetBranch("nemc");
   b_ntof = fChain->GetBranch("ntof");
   b_ncrk = fChain->GetBranch("ncrk");
   b_ndcw = fChain->GetBranch("ndcw");
   b_ndchitw = fChain->GetBranch("ndchitw");
   b_npc1w = fChain->GetBranch("npc1w");
   b_nemcw = fChain->GetBranch("nemcw");
   b_etotw = fChain->GetBranch("etotw");
   b_etote = fChain->GetBranch("etote");
   return kTRUE;
}

void nt_evt::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t nt_evt::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef nt_evt_cxx

