#ifndef __TESTANALYSIS_H__
#define __TESTANALYSIS_H__

#include "EmcAnaCommon.h"

#include <fstream>
#include <vector>

class TObject;
class TFile;
class TH1D;
class TH2D;
class TFile;
class TTree;
class TChain;

class Warnmap;
class RecalEcore;

class TestAnalysis
{
   TH1D* m_h1_ert_check; //!
   TH2D* m_h2_gl1_check; //!
   
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain     
   
   //   Int_t   m_run;
   //   Int_t   m_evt;
   //   Float_t m_bbcz;
   //   Float_t m_bbct0;

 public:
   TestAnalysis();
   virtual ~TestAnalysis();
 protected:
   Int_t SetTree(TTree* tree);
   Int_t GetEntry(Long64_t entry);
   Long64_t LoadTree(Long64_t entry);
 public:   
   void ProcessInput(char* ifname);
   void Analysis();

};
      
#endif // __PI0ANALYSIS_H__
