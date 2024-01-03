#ifndef __Run16dAuPi0Photon_H__
#define __Run16dAuPi0Photon_H__

#include <math.h>

#include "SubsysReco.h"

// Define BOOK_GG_NTUPLE if you want the diagnostic gamma-gamma
// Ntuple to be defined in the output.
// NOTE NOTE NOTE: this is now defined with --enable-ntuple!!
#define BOOK_GG_NTUPLE

// Define SIMPLE_MIXING if you need to debug the event mixing
// bookkeeping.
//
//#define SIMPLE_MIXING

#ifdef SIMPLE_MIXING
#warning SIMPLE_MIXING is set!
#define MAX_EVBUF_DEPTH 5
#define NUM_CENT_CLASSES 1
#define NUM_VTX_CLASSES 1
#define NUM_RP_CLASSES 12

#else
#define MAX_EVBUF_DEPTH 40
//#define MAX_EVBUF_DEPTH 5
//#define NUM_CENT_CLASSES 4
#define NUM_CENT_CLASSES 7
#define NUM_VTX_CLASSES 4
#define NUM_RP_CLASSES 24

#endif

// #define NUM_ASYM_BINS 20 // see below 
// #define ASYM_MIN 0.0
// #define ASYM_MAX 0.95

// BBC Vertex Z cut +- 30 or 10 cm
#define VTX_Z_CUT 20.0

#include <string>
#include <deque>

#include "KEventCent.h"
#include "Combination.h"

// If the EvB timer/averaging routines are available, use them
//
#ifdef HAVE_EVBTIMER_H
#include "EvBTimer.h"
#endif
#ifdef HAVE_EVBAVERAGER_H
#include "EvBAverager.h"
#endif

class TH1F;
class TH2F;
class THmulf;
class TNtuple;
class TTree;

class Fun4AllServer;
class CentralityReco;
class recoConsts;
class BunchCross;
class PHGlobal;
class emcClusterContainer;
class PHCentralTrack;
class TrigLvl1;
class ErtOut;
class KEvent;
class KTofCutter;
class emCalibMaster;


#ifndef __CINT__
#include "boost/shared_ptr.hpp"
typedef boost::shared_ptr<KEvent> KEVENT_PTR;
#else
typedef KEvent* KEVENT_PTR;
#endif

class Run16dAuPi0Photon : public SubsysReco {


public:

  Run16dAuPi0Photon(const char* outfile="EXAMPLE.root", const int Vorder=2);
  virtual ~Run16dAuPi0Photon();

  int End(PHCompositeNode *topNode);

  int Init(PHCompositeNode *topNode); // called during intialization
  int InitRun(PHCompositeNode *topNode); // called for first event when run number is known
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) { return 0; }
  int ResetEvent(PHCompositeNode *topNode) { return 0; }
  void SetOutputfile(const char *name) { OutFileName = name; }
	

  const char *Name() const { return "Run16dAuPi0Photon"; }
  
  static const int _ASYMBINS;// = 12;             /// ASYMMETRY BINNING
  static double _ASYMLOWEDGES[]; 

  static const int _ASYM2BINS;// = 12;             /// ASYMMETRY BINNING
  static double _ASYM2LOWEDGES[]; 

private:

  bool getNodes(PHCompositeNode *topNode);
  void checkSegNumber();
  TNtuple* bookGGNtuple() const; 
  TNtuple* bookGammaNtuple() const; 

protected:
  
  // Store the names of the nodes. This is useful since 
  // some nodes have one name in the micro- and nano-DSTs, but
  // different names in the (Hard) pico-DSTs
  //
  std::string _TrigNodeName;
  std::string _ErtNodeName;
  std::string _phGlobalNodeName;
  std::string _reactionPlaneObjectNodeName;
  std::string _emcClusterContainerNodeName;
  std::string _phCentralTrackNodeName;
   
  // The nodes themselves (not used quite yet)
  //
  PHGlobal* _phGlobal_ptr;
  emcClusterContainer* _emcClusterContainer_ptr;
  PHCentralTrack* _phCentTrack_ptr;
  TrigLvl1* _Trig_ptr;
  ErtOut* _Ert_ptr;
  emCalibMaster *_CP_Calib;  


  Int_t     xing_shift; // Usually 5. If not, need xcheck from other way (local pol asymetry, or so).
  Int_t		xing;
  int 	 shuffle;
  
  int mixed;
  int _nclus;
  int _prevevent;
  int _nevents;
  int _neventsGammaCluster;
  int _currentSegNumber;
  int _numMissingSegments;
  int runnumber;
  int debug;
  int is_pp;
  int is_patag;
  int is_ert;
  int is_bookNt;
  enum TrigSel {TT_BBC, TT_ERT};
  TrigSel TriggerBit;
  float pTcut;
  float bbcz;
  
  static const double pi;

  mEmcGeometryModule* EmcGeo;
  Combination cmb;

  void cmbReal(KEVENT_PTR kevent_ptr, int ERTb);
  void cmbMixedERT(int cent, int ivtx, int rpBin, int ERTb);
  void readBadTowers();
  void summary();
 
  Fun4AllServer *se;
  CentralityReco* centReco;
  recoConsts *rc;

  TH1F *event_counter;
  TH1F *cluster_counter;
  TH1F *event_live_erta;
  TH1F *event_live_ertb;
  TH1F *event_live_ertc;
  TH2F *htrigger;
  TH1F *hlumi;
	TH1F *hpatternBlue;
  TH1F *hpatternYellow;
  TH2F *hAcc;
  TH1D *hRelative;
  TH2F *pair_timing_sec[8];
  TH1D *centrality;
  TH2F *bbcqs_ecoreMax;
  TH2F *bbcqs_ecoreTotal;
  TH2F *bbcqs_ecoreAverage;
  TH2F *bbcqn_ecoreMax;
  TH2F *bbcqn_ecoreTotal;
  TH2F *bbcqn_ecoreAverage;

  THmulf *evts_cent;  // 500x500x102 =  25 500 000
  THmulf *evts_mult;  // 3x500x102   =     153 000
  THmulf *evts_ert;   // 2x2x2x2x102   =     153 000
  THmulf *gamma1;     // 120*2*5*8*2*7*2*3*4 = 3 225 600
  THmulf *gammarap;     // 120*2*5*8*2*7*2*3*4 = 3 225 600
  THmulf *tower;      //  30*8*48*96*2*4    = 8 847 360
  THmulf *towertof;      //  50*8*48*96*2*4    = 8 847 360
  THmulf *towertof_3ns;      //  50*8*48*96*2*4    = 8 847 360
  THmulf *tower_gghasym;
  THmulf *gghrp;        // 160*110*2*2*5*2*3*2*4 = 16 896 000
  THmulf *bbczdc;     // 30*30*5*30*30*4       = 16 200 000
  THmulf *reacpl;
  TH1D *reacpl2;
  THmulf *gghasym;
  THmulf *ggh_sysErr_Asym;
  THmulf *ggh_sysErr_Chi2;
  THmulf *gghspecial;
  THmulf *gghrap;
  THmulf *gghshuffle;
  THmulf *gghcheck;
  THmulf *Asym;
//  THmulf *dZOverZ_ele;
  THmulf *gghboxes;
  THmulf *cent_fw_corr;

  std::string _diagfilename;
  TFile*      _diagfile;
  TNtuple*    _ggDiagNtuple;
  TNtuple*    _gammaDiagNtuple;
  TTree*      _evRateTree;
  UInt_t      _evtN, _runN;
  ULong64_t   _evTime;

  std::deque<KEVENT_PTR> evarray[NUM_CENT_CLASSES][NUM_VTX_CLASSES][NUM_RP_CLASSES];

  KEventCent evCent;
  KTofCutter *tofcutter;
  
  std::string OutFileName;

  int inputFlag;  // 0 for PWG nanoDST, 1 for picoDST
  int rpflag;
  int vorder;
  int trigsel;

  int _status_array[2][4][48][96];
  float _low_limit_array[2][4][48][96];
  float _high_limit_array[2][4][48][96];


#ifdef HAVE_EVBTIMER_H
  EvBTimer _eventTimer;
#endif
#ifdef HAVE_EVBAVERAGER_H
  EvBAverager<double> _eventTimeAvg;
  EvBAverager<double> _mixTimeAvg;
#endif

//  Warnmap *TwrVeto; // Run8
//  HotTwrList *TwrVeto; Run7
//  TowerHelper *TwrHlp;
  ClassDef( Run16dAuPi0Photon, 0 )

};

#endif /* __Run16dAuPi0Photon_H__ */
