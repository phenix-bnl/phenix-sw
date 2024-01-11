//-----------------------------------------------------------------------------
//
//  Declaration of class Slewing
//
//  Purpose: Make Histgram from trktree (TofTrkTree)
//
//  Description:
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#ifndef __SLEWING_HH__
#define __SLEWING_HH__

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

#include "Tof.hh"
#include "TofAddressObject.hh"
#include "TofGeometryObject.hh"
#include "TofCalibObject.hh"

class Slewing : public TofTrkTree {
public:
  Slewing(TofAddressObject *, TofGeometryObject *, TofCalibObject *);
  ~Slewing();
public:
  void Loop();
  void Loop_a_file(char *ntfile);
  void Write(char *ofile);
  void setTofGlobalT(char *txtFile);
  void setTofPmtGain(char *txtFile);

public:
  Float_t getMass2Tof(Float_t trueTof, Float_t pltof, Float_t mom);
  Float_t getTofGlobalT(Int_t run);
  Float_t getTofPmtGain(Int_t slatid, Int_t lu);
  Float_t getTofPmtFactor(Int_t slatid, Int_t lu);
  //

private:
  void make_hist(void);

  // PMT gain
  Float_t mip[960][2], pmtfactor[960][2];

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
  //|   Ntuple   |
  //+------------+
  TNtuple *nt_tof;
  TNtuple *nt_raw;

  //+------------+
  //| Histograms |
  //+------------+
  // Slat ID
  TH1F *tofSlat;
  TH1F *tofSlatD;
  TH1F *tofSlatT;
  // Timing
  TH2F *tofT2d;    // pass4 # Pre timing tuning
  TH3F *tofQvcL3d; // pass3
  TH3F *tofQvcU3d; // pass3

  TH2F *tofQvcL2d; // pass2
  TH2F *tofQvcU2d; // pass2

  TH2F *tofSlew;
  TH2F *tofSlew2d[TOF_NPANEL_ALL][8]; // [panel][slatset]  # Time:QVC
  TH2F *tofSqvc2d[TOF_NPANEL_ALL][8]; // [panel][slatset]  # TOF:sqrt(Q0*Q1)
  TH2F *tofEloss2d[TOF_NPANEL_ALL][8]; // [panel][slatset] # TOF:ELOSS

  // checking histgram
  TH2F *tofSlewPos2d[TOF_NPANEL_ALL][8][6]; // [panel][slatset][position]
  TH2F *tofSlewT2d[TOF_NPANEL_ALL][8]; // [panel][slatset]  # TOF:QVC
  TH2F *tofSlewS2d[TOF_NPANEL_ALL][8]; // [panel][slatset]  # Time:sqrt(Q0*Q1)
  TH3F *tofSlewPt;
  TH3F *tofSlew3d[TOF_NPANEL_ALL][8]; // [panel][slatset]
  TH3F *tofSqvc3d[TOF_NPANEL_ALL][8]; // [panel][slatset]
  TH3F *tofEloss3d[TOF_NPANEL_ALL][8]; // [panel][slatset]

};

#endif /* __SLEWING_HH__ */

#ifdef __SLEWING_CC__
Slewing::Slewing(TofAddressObject  *address,
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

Slewing::~Slewing(){
  if (!fChain) return;
  delete fChain->GetCurrentFile();
  //if (!fTree) return;
  //delete fTree->GetCurrentFile();
}

#endif /* __SLEWING_CC__ */
