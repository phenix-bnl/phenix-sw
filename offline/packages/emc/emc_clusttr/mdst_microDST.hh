//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Tue Dec 11 13:42:47 2001 by ROOT version3.01/05)
//   from TTree T/titled by PHOOL
//   found on file: /phenix/data21/phnxreco/run2_v01_pro14/microDST/microDST_run2_v01-0000027197-0000.root
//////////////////////////////////////////////////////////


#ifndef mdst_microDST_h
#define mdst_microDST_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <stream.h>
#include <microGlobalEventv1.hh>
#include <microGlobalTrackListv1.hh>
#include <microTecTrackListv1.hh>
#include <microDchTrackListv1.hh>
#include <microPadClusterListv1.hh>
#include <microTofHitListv1.hh>
#include <microEmcClusterListv1.hh>
#include <microCrkRingListv1.hh>

class mdst_microDST {
public :
  TTree          *fChain;   //pointer to the analyzed TTree or TChain
  Int_t           fCurrent; //current Tree number in a TChain

  microGlobalEventv1* glb;
  microGlobalTrackListv1* cgl;
  microGlobalTrackListv1* cglbg;
  microTecTrackListv1* tec;
  microDchTrackListv1* dch;
  microPadClusterListv1* pc1;
  microPadClusterListv1* pc2;
  microPadClusterListv1* pc3;
  microTofHitListv1* tof;
  microEmcClusterListv1* emc;
  microCrkRingListv1* crk;

  //List of branches
  TBranch        *b_glb;
  TBranch        *b_cgl;
  TBranch        *b_cglbg;
  TBranch        *b_tec;
  TBranch        *b_dch;
  TBranch        *b_pc1;
  TBranch        *b_pc2;
  TBranch        *b_pc3;
  TBranch        *b_tof;
  TBranch        *b_emc;
  TBranch        *b_emt;
  TBranch        *b_crk;

   mdst_microDST(TTree *tree=0);
   ~mdst_microDST();
   Int_t  Cut(Int_t entry);
   Int_t  GetEntry(Int_t entry);
   Int_t  LoadTree(Int_t entry);
   void   Init(TTree *tree);
   void   Loop();
   Bool_t Notify();
   void   Show(Int_t entry = -1);
};

#endif

#ifdef mdst_microDST_cxx
mdst_microDST::mdst_microDST(TTree *tree)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
  if (tree == 0) {
    cout<<" mdst_microDST got NULL pointer of TTree* "<<endl;
    return;
  }
  tree = (TTree*)gDirectory->Get("T");
  glb = new microGlobalEventv1();
  cgl = new microGlobalTrackListv1();
  cglbg = new microGlobalTrackListv1();
  tec = new microTecTrackListv1();
  dch = new microDchTrackListv1();
  pc1 = new microPadClusterListv1();
  pc2 = new microPadClusterListv1();
  pc3 = new microPadClusterListv1();
  tof = new microTofHitListv1();
  emc = new microEmcClusterListv1();
  crk = new microCrkRingListv1();
  Init(tree);
}

mdst_microDST::~mdst_microDST()
{
   if (!fChain) return;
   delete glb;
   delete cgl;
   delete cglbg;
   delete tec;
   delete dch;
   delete pc1;
   delete pc2;
   delete pc3;
   delete tof;
   delete emc;
   delete crk;
}

Int_t mdst_microDST::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Int_t mdst_microDST::LoadTree(Int_t entry)
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

void mdst_microDST::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fChain    = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

  microGlobalEventv1* glb = new microGlobalEventv1();
  microGlobalTrackListv1* cgl = new microGlobalTrackListv1();
  microGlobalTrackListv1* cglbg = new microGlobalTrackListv1();
  microTecTrackListv1* tec = new microTecTrackListv1();
  microDchTrackListv1* dch = new microDchTrackListv1();
  microPadClusterListv1* pc1 = new microPadClusterListv1();
  microPadClusterListv1* pc2 = new microPadClusterListv1();
  microPadClusterListv1* pc3 = new microPadClusterListv1();
  microTofHitListv1* tof = new microTofHitListv1();
  microEmcClusterListv1* emc = new microEmcClusterListv1();
  microCrkRingListv1* crk = new microCrkRingListv1();


  fChain->SetBranchAddress("UDST/UEVT/microGlobalEventList", &glb);
  fChain->SetBranchAddress("UDST/UCGL/microGlobalTrackList", &cgl);
  fChain->SetBranchAddress("UDST/UCGLBG/microGlobalTrackListBG", &cglbg);
  fChain->SetBranchAddress("UDST/UTEC/microTecTrackList", &tec);
  fChain->SetBranchAddress("UDST/UDCH/microDchTrackList", &dch);
  fChain->SetBranchAddress("UDST/UPC1/microPc1ClusterList", &pc1);
  fChain->SetBranchAddress("UDST/UPC2/microPc2ClusterList", &pc2);
  fChain->SetBranchAddress("UDST/UPC3/microPc3ClusterList", &pc3);
  fChain->SetBranchAddress("UDST/UTOF/microTofHitList", &tof);
  fChain->SetBranchAddress("UDST/UEMC/microEmcClusterList", &emc);
  fChain->SetBranchAddress("UDST/UCrk/microCrkRingList", &crk);

  Notify();
}

Bool_t mdst_microDST::Notify()
{
//   called when loading a new file
//   get branch pointers
  b_glb = fChain->GetBranch("UDST/UEVT/microGlobalEventList");
  b_cgl = fChain->GetBranch("UDST/UCGL/microGlobalTrackList");
  b_cglbg = fChain->GetBranch("UDST/UCGLBG/microGlobalTrackListBG");
  b_tec = fChain->GetBranch("UDST/UTEC/microTecTrackList");
  b_dch = fChain->GetBranch("UDST/UDCH/microDchTrackList");
  b_pc1 = fChain->GetBranch("UDST/UPC1/microPc1ClusterList");
  b_pc2 = fChain->GetBranch("UDST/UPC2/microPc2ClusterList");
  b_pc3 = fChain->GetBranch("UDST/UPC3/microPc3ClusterList");
  b_tof = fChain->GetBranch("UDST/UTOF/microTofHitList");
  b_emc = fChain->GetBranch("UDST/UEMC/microEmcClusterList");
  b_crk = fChain->GetBranch("UDST/UCrk/microCrkRingList");

   return kTRUE;
}

void mdst_microDST::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t mdst_microDST::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef mdst_microDST_cxx

