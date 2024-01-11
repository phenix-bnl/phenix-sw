//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Sun Dec 17 11:51:54 2000 by ROOT version 2.23/12)
//   from TTree nt_evt/Event characteristics
//   found on file: mergedEvent1.root
//////////////////////////////////////////////////////////


#ifndef classMicroEvent_h
#define classMicroEvent_h

#if !defined(__CINT__) || defined(__MAKECINT__)
#include <TTree.h>
#include <TFile.h>
#include "TArrayD.h"
#include "TGraph.h"
#include "TF1.h"
#include "TFormula.h"
#include "TVector2.h"
#include <TH2.h>
#include <TNtuple.h>

#endif

#include <cstring>
#include <iostream.h>
class classMicroEvent {
   public :
   TTree          *fTree;    //pointer to the analyzed TTree or TChain
   TTree          *fCurrent; //pointer to the current TTree
//Declaration of leaves types
   Float_t         run;
   Float_t         seq;
   Float_t         evt;
   Float_t         trig;
   Float_t         trigScaledLsb;
   Float_t         trigScaledMsb;
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
   TBranch        *b_trigScaledLsb;
   TBranch        *b_trigScaledMsb;
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

   classMicroEvent(TTree *tree=0);
  ~classMicroEvent();
   Int_t GetEntry(Int_t entry);
   Int_t LoadTree(Int_t entry);
   void  Init(TTree *tree);
   void  Loop();
   void  Notify();
   void  Show(Int_t entry = -1);
   void  Selection(Char_t*, double bbcmax , double zdcmax);

   void LoopOnce();
   void Loop_a_fileOnce(Char_t* file);
  Int_t centBin(Float_t& ncoll, Float_t& npart);
  void LoadCentAngles(Int_t run= -1);
  void InitHisto();
  void CloseHisto();


private:

  TFile* file;
  TNtuple* centrality;

  int DEFAULT_CAPACITY ; // = 1000;
  int NUM_DIVISION ; // = 200;
  double BBCLOWLIMIT;
  double BBCUPLIMIT;
  double ZDCLOWLIMIT;
  double ZDCUPLIMIT;

  double bbcMaxValueFilled;
  double zdcMaxValueFilled;

  int outsideLimits;
 
  int arrayCapacity;		// current array capacity
  int filledCapacity;		// number of points filled in array
  TArrayD *BbcChargeData;	// array for BbcCharge data
  TArrayD *ZdcEngData;		// array for zero degree energy data
  TGraph *centralityGraph;
  TF1 *fittingFunction;         // function to fit centrality data
  TGraph *distributionGraph;

  void increaseArrayCapacity();//increase array capacity by DEFAULT_CAPACITY
  void incrementDataNumber() { filledCapacity++; }
  double getBbcChargeValue();	// get BbcCharge value from table
  double getZdcEngValue();	// get Zdc energy value from table
  void addDataPoint();	// get data from table and add to data arrays 
  void createGraphs();	// create the graphs and store them in file
  void createDistGraph();  // create the distribution graph
  

  Int_t totevents; 
  Int_t events[10]; 
 
  bool WithinRange(double Xvalue, double Yvalue,
		   double lowerBound, double upperBound);

  TF1 *mFittingFunction;
  TGraph *mDistributionGraph;

  TH2D* mSelected;
  TH2D* mAll;

  double mBbcNormalization;
  double mZdcNormalization;

  // JV parameters for centrality calculation
  Float_t bbcMax;
  Float_t zdcMax;
  Float_t phiCut[20];



};

#endif

#ifdef classMicroEvent_cxx
classMicroEvent::classMicroEvent(TTree *tree)
{
  BbcChargeData = NULL;
  ZdcEngData = NULL;
  centralityGraph = NULL;
  distributionGraph = NULL;
  fittingFunction = NULL;

// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
     printf("Missing Tree \n");
   }

   totevents = 0;
   for (int a=0; a<10;a++) {
     events[a] = 0; 
   }
 
   LoadCentAngles();
   DEFAULT_CAPACITY =1000;
   NUM_DIVISION = 200;
   // 
   // HARDWIRED limits for bbc charge and zdc energy
   //
   BBCLOWLIMIT = 0;
   BBCUPLIMIT  = 200;
   ZDCLOWLIMIT = 0;
   ZDCUPLIMIT  = 2500;
   
   arrayCapacity = DEFAULT_CAPACITY;
   filledCapacity = 0;
   outsideLimits = 0;
   
   bbcMaxValueFilled = 0;
   zdcMaxValueFilled = 0;
   
   BbcChargeData = new TArrayD(arrayCapacity);
   ZdcEngData = new TArrayD(arrayCapacity);
   


}

classMicroEvent::~classMicroEvent()
{
  if( BbcChargeData )delete BbcChargeData;
  if( ZdcEngData ) delete ZdcEngData;
  cout << "----------------------------------"<< endl;
  cout << "----------------------------------"<< endl;
  cout << "Final statistic "<< endl;
  cout << "Total Events "<< totevents << endl;
  for (int a=0; a<10; a++) {
    cout << "Total Events in a =" << a << "  is "<< events[a] << endl;
  }

}


Int_t classMicroEvent::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fTree) return 0;
   return fTree->GetEntry(entry);
}
Int_t classMicroEvent::LoadTree(Int_t entry)
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

void classMicroEvent::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;
   fTree    = tree;
   fCurrent = 0;

   fTree->SetBranchAddress("run",&run);
   fTree->SetBranchAddress("seq",&seq);
   fTree->SetBranchAddress("evt",&evt);
   fTree->SetBranchAddress("trig",&trig);
   fTree->SetBranchAddress("trigScaledLsb",&trigScaledLsb);
   fTree->SetBranchAddress("trigScaledMsb",&trigScaledMsb);
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
}

void classMicroEvent::Notify()
{
//   called by LoadTree when loading a new file
//   get branch pointers
   b_run = fTree->GetBranch("run");
   b_seq = fTree->GetBranch("seq");
   b_evt = fTree->GetBranch("evt");
   b_trig = fTree->GetBranch("trig");
   b_trigScaledLsb = fTree->GetBranch("trigScaledLsb");
   b_trigScaledMsb = fTree->GetBranch("trigScaledMsb");
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
}

void classMicroEvent::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fTree) return;
   fTree->Show(entry);
}
#endif // #ifdef classMicroEvent_cxx

