#ifndef __HBDGAINANALYZER_H__
#define __HBDGAINANALYZER_H__

#include <map>
#include <vector>
#include <iostream>
#include "SubsysReco.h"
#include <TH1F.h>

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;
class HbdCell;
class HbdCellList;
class HbdMiniCellList;
class hbdDetectorGeo;
class HbdFinalSimSupport;
class TTree;
class TFile;
class TH1F;
class TH2;
//class hbdAdcCalib;

const Int_t Narms  =2;//number of Hbd arms
const Int_t Nsides =2;//number of Hbd sides (modules)
const Int_t Nsect  =6;//number of Hbd sectors

const Int_t kMaxCells=2050;
const Int_t kMaxTracks =5000;
//
//  Hello Gain Fans:
//    This is a simple Analyzer Module that is used as an example for coding.
//  It presumes to read objects from one or more input streams,
//  make a few histograms of interesting variables, and save these to a file.
//
//                                                TKH
//                                                8-25-2003
//

class HbdGainAnalyzer: public SubsysReco
{
public:
  HbdGainAnalyzer(const char *outfile = "dstout.root");
  virtual ~HbdGainAnalyzer();
  
  char outfilename[1000];
  char f_name[1000];

  //  For this analysis we only use Init, process_event, and End;
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);// {return 0;}
  int Reset(PHCompositeNode *topNode) {return 0;}
  int End(PHCompositeNode *topNode);
  void Print(const char *what) const {return;}


protected:
  //  Utility to get pointers to data objects...
  void GetNodes(PHCompositeNode *topNode);
  
  /****************
   Tree Definitions
  ****************/
  void fill_RunInfo(PHCompositeNode *topNode);
  void fill_HBDCell(PHCompositeNode *topNode);  
  void fill_TrackProj(PHCompositeNode *topNode);  

  TTree *fHBDTree;
  TFile* tmpf;
  TTree *fHBDTree_fr;
  TFile* tmpf_fr;
   
  Bool_t fWriteFullTree;
  Bool_t fWriteShortTree;
  
  /****************
   Histogram Definitions
  ****************/

  TH1F *h_mod_gain[Narms][Nsides][Nsect];
  TH1F *h_counter;

  int ncalls;

  //DataNodes...
  PHCentralTrack  *t;
  PHGlobal        *g;
  HbdCellList *CellList;
  HbdMiniCellList *MiniCellList;

  HbdFinalSimSupport *simsupport;
  hbdDetectorGeo *hbdgeo;
  //hbdAdcCalib *calib;

  //Tree Variables: RunInfo
  Int_t   f_RunInfo_Run;
  Int_t   f_RunInfo_Seg;
  Int_t   f_RunInfo_Event;
  Float_t f_RunInfo_ZVtx;
  Float_t f_RunInfo_bbcS;
  Float_t f_RunInfo_bbcN;
  Int_t   f_RunInfo_DCTracks;

  //Tree Variables: HBDCell
  
  Int_t   f_HBDCell_ncells;//dynamic for n_cells
  Float_t f_HBDCell_charge[kMaxCells];
  Int_t   f_HBDCell_sector[kMaxCells];
  Int_t   f_HBDCell_arm[kMaxCells];
  Int_t   f_HBDCell_module[kMaxCells];
  Int_t   f_HBDCell_padnum[kMaxCells];
  Int_t   f_HBDCell_adcch[kMaxCells];
  Int_t   f_HBDCell_flag[kMaxCells];
  Float_t f_HBDCell_locy[kMaxCells];
  Float_t f_HBDCell_locz[kMaxCells];
  Float_t f_HBDCell_globx[kMaxCells];
  Float_t f_HBDCell_globy[kMaxCells];
  Float_t f_HBDCell_globz[kMaxCells];
  Float_t f_HBDCell_globphi[kMaxCells];

  //Tree Variables: TrackProj
  Int_t   f_TrackProj;//dynamic for n_tracks
  Float_t f_TrackProj_x[kMaxTracks];
  Float_t f_TrackProj_y[kMaxTracks];
  Float_t f_TrackProj_z[kMaxTracks];
  Float_t f_TrackProj_phi[kMaxTracks];

private:
  int PadId[2304];
  int SeqSec[2304];

};


#endif /* __HBDGAINANALYZER_H__ */
