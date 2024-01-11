#ifndef __MPCMIP_H__
#define __MPCMIP_H__

#include "SubsysReco.h"
#include <string>
//#include <TString.h>

class Fun4AllHistoManager;
class PHCompositeNode;
class TH1;
class TH2;
class TCanvas;
//class PHGlobal;
//class EventHeader;
class TriggerHelper;
class MpcMap;
class MpcCalib;
//class BbcOut;
class BbcGeo;
class TNtuple;
class TFile;
class TTree;
class TString;

class MpcMip: public SubsysReco
{
public:
  MpcMip(const char* outhistfile = "mpcmip_hist.root",const char* outrootfile="mpcmip_tree.root");
  virtual ~MpcMip();

  void SetTreeFlag(const bool t) { treeflag = t; }

  //  For this analysis we only use Init, process_event;
  //int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

  int Reset        (PHCompositeNode *topNode) { return 1; }

protected:
  std::string OutHistFile;
  std::string OutTreeFile;
  
  Fun4AllHistoManager *HistoManager;

  TCanvas *DisplayCanvas;
  TH2 *AlignmentDisplay;
  TH1 *htdc[576];
  TH1 *hloadc[576];
  TH1 *hhiadc[576];
  TH1 *hlo[6][576];	// [NTRIG][NFEECH]
  TH1 *hhi[6][576];
//  TH1 *hlopost[576];
//  TH1 *hlopre[576];
//  TH1 *hhipost[576];
//  TH1 *hhipre[576];

  TH1 *hadc[576];
  TH1 *htrig;		// trigger counts

  //TH2 *TrigCounts;

  bool treeflag;		// whether to write out a tree
  TFile *ttreefile;
  TTree *ttree;
  TriggerHelper *trighelp;

  MpcMap   *mpcmap;     // channel mapping class
  MpcCalib *mpccalib;   // calibrations class (returns calib values)

  BbcGeo  *bbcgeom;

  // miptree variables
  UInt_t   event;
  Float_t  zvtx;
  UInt_t   scaledtrig;	// scaled trigger word
  Float_t  bbcqn;
  Float_t  bbcqs;
  Float_t  bbchit;
  Float_t  mpchit;
  Short_t  pmt;
  UShort_t adc;
  UShort_t tdc0;
  UShort_t tdc1;
  Short_t  arm;
  Short_t  gridx_value;
  Short_t  gridy_value;
  Short_t  hienergybitsum;
  Float_t  loadc_cor;
  Float_t  hiadc_cor;
  Int_t    tdc;
  Float_t  maxsamp;
  Int_t    post_amu;
  Int_t    pre_amu;
  Float_t  dx;
  Float_t  dy;

  int npmt_hit;

  float DR_MAXIMUM_CUT;
  float HIT_TOWER_MIN_HIADC;

  static const int nbit=3;
  static const int nrad=4;
  int lo_bit_cut[nbit];
  int hi_bit_cut[nbit];
  double lo_rad_cut[nrad];
  double hi_rad_cut[nrad];
  TH1 *hhiadc_cut[576][nrad][nbit];

//  PHGlobal *global;
//  EventHeader *evtheader;
};

#endif /* __MPCMIP_H__ */



