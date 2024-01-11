//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Wed Mar  4 16:20:33 2009 by ROOT version 5.17/01
// from TTree EmbedMcRecoTrack/EmbedMcRecoTrack
// found on file: embedcom_pions.root
//////////////////////////////////////////////////////////

#ifndef Recal_h
#define Recal_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TH1.h>
#include <vector>

class Recal {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leave types
   Float_t         pc3dphiS;
   Float_t         pc3dzS;
   Float_t         pc3dphiR;
   Float_t         pc3dzR;
   Float_t         pc3sdphiS;
   Float_t         pc3sdzS;
   Float_t         pc3sdphiR;
   Float_t         pc3sdzR;
   Float_t         QS;
   Float_t         QR;
   Float_t         zedS;
   Float_t         zedR;
   Float_t         nx1hS;
   Float_t         nx2hS;
   Float_t         nx1hR;
   Float_t         nx2hR;
   Float_t         phiS;
   Float_t         phiR;
   Float_t         thetaS;
   Float_t         thetaR;
   Float_t         alphaS;
   Float_t         alphaR;
   Float_t         momG;
   Float_t         w1;
   Float_t         w2;
   Float_t         momS;
   Float_t         momR;
   Float_t         type;
   Float_t         gen;
   Float_t         bbccent;
   Float_t         crknpmt0S;
   Float_t         crknpmt0R;

   // List of branches
   TBranch        *b_pc3dphiS;   //!
   TBranch        *b_pc3dzS;   //!
   TBranch        *b_pc3dphiR;   //!
   TBranch        *b_pc3dzR;   //!
   TBranch        *b_pc3sdphiS;   //!
   TBranch        *b_pc3sdzS;   //!
   TBranch        *b_pc3sdphiR;   //!
   TBranch        *b_pc3sdzR;   //!
   TBranch        *b_QS;   //!
   TBranch        *b_QR;   //!
   TBranch        *b_zedS;   //!
   TBranch        *b_zedR;   //!
   TBranch        *b_nx1hS;   //!
   TBranch        *b_nx2hS;   //!
   TBranch        *b_nx1hR;   //!
   TBranch        *b_nx2hR;   //!
   TBranch        *b_phiS;   //!
   TBranch        *b_phiR;   //!
   TBranch        *b_thetaS;   //!
   TBranch        *b_thetaR;   //!
   TBranch        *b_alphaS;   //!
   TBranch        *b_alphaR;   //!
   TBranch        *b_momG;   //!
   TBranch        *b_w1;   //!
   TBranch        *b_w2;   //!
   TBranch        *b_momS;   //!
   TBranch        *b_momR;   //!
   TBranch        *b_type;   //!
   TBranch        *b_gen;   //!
   TBranch        *b_bbccent;   //!
   TBranch        *b_crknpmt0S;
   TBranch        *b_crknpmt0R;

   Recal(TTree *tree=0, char* outFileName = "recal.root", 
	 int ncb=0, double cb[]=0, int npt=0, double pt[]=0);
   virtual ~Recal();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);


   // Stuff added by Andrew

   const int nPar;
   enum ePar  {PMU, PSG, ZMU, ZSG};
   char* sPar[4];
   double pc3dzMin;  // cm                                                                
   double pc3dzMax;
   double pc3dpMin;  // rad                                                                
   double pc3dpMax;
   double zedCut;    // cm
   double fitPtMin;
   double fitPtMax;

   const int nTypes; // 0=S, 1=R.
   enum eType {S, R, SP, SN, RP, RN};
   char* sType[10];
   int color[10];
   
   const int nCentBins;
   double centBin[100];
   const int nPtBins;
   double ptBin[100];
   
   void bookHistos();
   void matchRecal();
   void fitPC3VsPt(TH1F* hf);
   TFile* outFile;
   TH1F* hCentBin;
   TH1F* hPtBin;
   
/*    // Raw PC3 matching - phi and z */
//   CINT is too crappy to handle nested vectors
//   It seg faults when pushing_back a vector.
/*    vector<vector<vector<TH1F*> > > hp; // [eType][cent][pT] */
/*    vector<vector<vector<TH1F*> > > hz; */

   TH1F* hp[6][100][100]; // [eType][cent][pT] 
   TH1F* hz[6][100][100]; // [eType][cent][pT] 

   // Gaussian fit parameters vs. pT
   TH1F* hf[4][6][100]; // [ePar][eType][cent] 

};

#endif

#ifdef Recal_cxx
Recal::~Recal()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t Recal::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t Recal::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (!fChain->InheritsFrom(TChain::Class()))  return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void Recal::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normaly not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("pc3dphiS", &pc3dphiS, &b_pc3dphiS);
   fChain->SetBranchAddress("pc3dzS", &pc3dzS, &b_pc3dzS);
   fChain->SetBranchAddress("pc3dphiR", &pc3dphiR, &b_pc3dphiR);
   fChain->SetBranchAddress("pc3dzR", &pc3dzR, &b_pc3dzR);
   fChain->SetBranchAddress("pc3sdphiS", &pc3sdphiS, &b_pc3sdphiS);
   fChain->SetBranchAddress("pc3sdzS", &pc3sdzS, &b_pc3sdzS);
   fChain->SetBranchAddress("pc3sdphiR", &pc3sdphiR, &b_pc3sdphiR);
   fChain->SetBranchAddress("pc3sdzR", &pc3sdzR, &b_pc3sdzR);
   fChain->SetBranchAddress("QS", &QS, &b_QS);
   fChain->SetBranchAddress("QR", &QR, &b_QR);
   fChain->SetBranchAddress("zedS", &zedS, &b_zedS);
   fChain->SetBranchAddress("zedR", &zedR, &b_zedR);
   fChain->SetBranchAddress("nx1hS", &nx1hS, &b_nx1hS);
   fChain->SetBranchAddress("nx2hS", &nx2hS, &b_nx2hS);
   fChain->SetBranchAddress("nx1hR", &nx1hR, &b_nx1hR);
   fChain->SetBranchAddress("nx2hR", &nx2hR, &b_nx2hR);
   fChain->SetBranchAddress("phiS", &phiS, &b_phiS);
   fChain->SetBranchAddress("phiR", &phiR, &b_phiR);
   fChain->SetBranchAddress("thetaS", &thetaS, &b_thetaS);
   fChain->SetBranchAddress("thetaR", &thetaR, &b_thetaR);
   fChain->SetBranchAddress("alphaS", &alphaS, &b_alphaS);
   fChain->SetBranchAddress("alphaR", &alphaR, &b_alphaR);
   fChain->SetBranchAddress("momG", &momG, &b_momG);
/*    fChain->SetBranchAddress("w1", &w1, &b_w1); */
/*    fChain->SetBranchAddress("w2", &w2, &b_w2); */
   fChain->SetBranchAddress("momS", &momS, &b_momS);
   fChain->SetBranchAddress("momR", &momR, &b_momR);
   fChain->SetBranchAddress("type", &type, &b_type);
   fChain->SetBranchAddress("gen", &gen, &b_gen);
   fChain->SetBranchAddress("bbccent", &bbccent, &b_bbccent);
   fChain->SetBranchAddress("crknpmt0S", &crknpmt0S, &b_crknpmt0S);
   fChain->SetBranchAddress("crknpmt0R", &crknpmt0R, &b_crknpmt0R);
   Notify();
}

Bool_t Recal::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normaly not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void Recal::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t Recal::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}

#endif // #ifdef Recal_cxx
