#ifndef TESTSUBSYSRECO_H__
#define TESTSUBSYSRECO_H__

#include "EmcAnaCommon.h"

#include <SubsysReco.h>

#include <TString.h>

class PHCompositeNode;
class TH1D;
class TH2D;
class TTree;
class TFile;

class Warnmap;

class TestSubsysReco: public SubsysReco
{
  ////
  //// variables for tree
  ////
  
  //// PHGlobal
  Int_t d_run;
  //  Int_t d_evt;
  //  Float_t d_bbcz;
  //  Float_t d_bbct0;
  Int_t d_trig_4x4a;
  Int_t d_trig_4x4b;
  Int_t d_trig_4x4c;
  Int_t d_trig_2x2;
  Int_t d_trig_bbc;

  int    m_run_target;

 public:
  TestSubsysReco(
     const int run, const char* ofilename, const char *name = "TESTRECO");
  virtual ~TestSubsysReco() {;}

  int End(PHCompositeNode *topNode); // called at EndRun
  int Init(PHCompositeNode *topNode); // Initialization at startup - create histos here
  int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
  int process_event(PHCompositeNode *topNode); // your analysis code goes here
  int Reset(PHCompositeNode *topNode); // called when new run opened (experimental)
  int ResetEvent(PHCompositeNode *topNode); // called at end of each event (for cleanup)
  void Print(const std::string&) const { return; }

 protected:
//  TTree* m_tree;
  TFile* m_file;
  TH1D* m_h1_ert_check; //!
  TH2D* m_h2_gl1_check; //!
  TString m_ofilename;

};

#endif
