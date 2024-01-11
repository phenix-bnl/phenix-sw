//-----------------------------------------------------------------------------
//
//  Declaration of class TreeToNtpl
//
//  Purpose: Make Histgram from trktree (TofTrkTree)
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#ifndef __TREETONTPL_HH__
#define __TREETONTPL_HH__

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

class TreeToNtpl : public TofTrkTree {
public:
  TreeToNtpl(TofAddressObject *, TofGeometryObject *, TofCalibObject *);
  ~TreeToNtpl();
public:
  void Loop();
  void Loop_a_file(char *ntfile);
  void Write(char *ofile);
  void setTofGlobalT(char *txtFile);

public:
  Float_t getMass2Tof(Float_t trueTof, Float_t pltof, Float_t mom);
  Float_t getTofGlobalT(Int_t run);
  //-------------------------------------------------------------------------
  // Function -- copy from $CVS/offline/analysis/microdst/classMicroTrack.hh
  //   following functions work only Year-1 Analysis
  //-------------------------------------------------------------------------
  Float_t getBbcT0(); // for Year1 only
  Float_t getToF();
  //
  // Matching cuts 
  Float_t surviveMatchTof(Float_t sigma,Int_t type,Float_t& nsP, Float_t& nsZ);
  // Centrality selection
  Int_t centBin(Float_t& ncoll, Float_t& npart);
  void LoadCentAngles(Int_t run = -1); // contains the parametrized angle cut
  // ------------------------------------------------
  //   track by track
  // centrality selection based on ZDC vs BBC angular cuts
  //   
  // Int_t centBin return value:
  // 0     1    2     3     4     5     6     7     8     9    10    11  etc
  // centrality between :
  // 0-5 5-10 10-15 15-20 20-25 25-30 30-35 35-40 40-45 45-50 50-55 55-60 etc
  //
  // NOTE: since our trigger is not efficient for the most peripheral 10%
  // the top value of centBin is 17, which means centrality > 85% 
  // example:  centBin = 10  - the track was in centrality bin 50-55%
  // if you need all the tracks in centrality bin 40-55%  
  // select tracks with centBin= 8||centBin=9||centBin=10
  // centBin<0 - error
  // centBin=-1 - the run is not parametrized
  // centBin=-2 - zdce0 = 0 for this event  
  // ----------------------------------------------

private:
  void make_ntpl(void);

  TofAddressObject*   TofAddress;
  TofGeometryObject*  TofGeometry;
  TofCalibObject*     TofCalib;

  // Run dependence
  Int_t   runsetnum;
  Int_t   run0[400];
  Float_t globalT0[400];

  // Track Counter
  Int_t trk_counter_all;
  Int_t trk_counter_time;
  Int_t trk_counter_diff;

  // JV parameters for centrality calculation
  Float_t bbcMax;
  Float_t zdcMax;
  Float_t phiCut[20];
  // JV parameters for multiplicity and centrality dependent efficiency
  // these are loaded in the function LoadCentAngles
  Float_t p0x1orx2;
  Float_t p1x1orx2;
  Float_t p2x1orx2;
  Float_t p0x1andx2;
  Float_t p1x1andx2;
  Float_t p2x1andx2;

  //+--------+
  //| Ntuple |
  //+--------+
  TNtuple *nt_tof;
  //+----------+
  //| Histgram |
  //+----------+
  // Slat ID
  TH1F *tofSlat;
  TH1F *tofSlatD;
  TH1F *tofSlatT;
  // Timing
  TH2F *tofT2d;    // pass4
  // checking plot
  TH2F *pt_mass2;
};

#endif /* __TREETONTPL_HH__ */

#ifdef __TREETONTPL_CC__
TreeToNtpl::TreeToNtpl(TofAddressObject  *address,
		       TofGeometryObject *geom, 
		       TofCalibObject    *calib){
  // Setup TofGeometryObject
  TofAddress  = address;
  TofGeometry = geom;
  TofCalib    = calib;
  LoadCentAngles();  // centrality for Year-1
  make_ntpl();
  trk_counter_all  = 0;
  trk_counter_time = 0;
  trk_counter_diff = 0;
}

TreeToNtpl::~TreeToNtpl(){
  if (!fChain) return;
  delete fChain->GetCurrentFile();
  //if (!fTree) return;
  //delete fTree->GetCurrentFile();
}
#endif /* __TREETONTPL_CC__ */
