//-----------------------------------------------------------------------------
//
//  Declaration of class TreeToHist
//
//  Purpose: Make Histgram from trktree (TofTrkTree)
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#ifndef __TREETOHIST_HH__
#define __TREETOHIST_HH__

#if !defined(__CINT__) || defined(__MAKECINT__)
#include <TROOT.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TTree.h>
#include <TChain.h>
#include <TFile.h>
#include <TNtuple.h>
#endif

#include "TofTrkTree.h"

#include "TofAddressObject.hh"
#include "TofGeometryObject.hh"
#include "TofCalibObject.hh"

class TreeToHist : public TofTrkTree {
public:
  TreeToHist(TofAddressObject *, TofGeometryObject *, TofCalibObject *);
  ~TreeToHist();
public:
  void Loop();
  void Loop_a_file(char *ntfile);
  void Write(char *ofile);
  void setTofGlobalT(char *txtFile);

public:
  Float_t getMass2Tof(Float_t trueTof, Float_t pltof, Float_t mom);
  Float_t getTofGlobalT(Int_t run);

private:
  void make_hist(void);

  TofAddressObject*   TofAddress;
  TofGeometryObject*  TofGeometry;
  TofCalibObject*     TofCalib;

  // Run dependence
  Int_t   runsetnum;
  Int_t   run0[100];
  Float_t globalT0[100];

  // Track Counter
  Int_t trk_counter_all;
  Int_t trk_counter_time;
  Int_t trk_counter_diff;

  //+------------+
  //| Histograms |
  //+------------+
  // Slat ID
  TH1F *tofSlat;
  TH1F *tofSlatD;
  TH1F *tofSlatT;
  // Position
  TH1F *tofDiff;
  TH2F *tofDiffY;
  TH2F *tofDiff2d;
  TH3F *tofDiff3d; // pass1
  TH2F *tofYpos2d;
  TH2F *tofZpos2d;
  // Timing
  TH1F *tofTime;
  TH2F *tofTime2d; // pass2
  TH2F *tofT2d;    // pass4
  TH3F *tofTft3d;  // pass4 pre
  TH3F *tofTbb3d;  // pass4 pre

  TH3F *tofQvcL3d; // pass3
  TH3F *tofQvcU3d; // pass3
  TH2F *tofSlew;

  // rundep
  TH1F *run2;
  TH1F *run4;
  TH1F *run6;
  TH1F *run8;
  TH2F *tofTrun2;
  TH2F *tofTrun4;
  TH2F *tofTrun6;
  TH2F *tofTrun8;

  // checking plot
  TH2F *pt_mass2;

  TH2F *t0pt_pos;
  TH2F *t0pt_neg;
  TH2F *t0npc1_pos;
  TH2F *t0npc1_neg;
  TH2F *t0npc1_pos_10pt15;
  TH2F *t0npc1_neg_10pt15;
  TH2F *t0npc1_pos_15pt20;
  TH2F *t0npc1_neg_15pt20;
  TH3F *t0npc1pt_pos;
  TH3F *t0npc1pt_neg;

};

#endif /* __TREETOHIST_HH__ */

#ifdef __TREETOHIST_CC__
TreeToHist::TreeToHist(TofAddressObject  *address,
		       TofGeometryObject *geom, 
		       TofCalibObject    *calib){
  // Setup TofGeometryObject
  TofAddress  = address;
  TofGeometry = geom;
  TofCalib    = calib;
  make_hist();
  trk_counter_all  = 0;
  trk_counter_time = 0;
  trk_counter_diff = 0;
}

TreeToHist::~TreeToHist(){
  if (!fChain) return;
  delete fChain->GetCurrentFile();
  //if (!fTree) return;
  //delete fTree->GetCurrentFile();
}
#endif /* __TREETOHIST_CC__ */
