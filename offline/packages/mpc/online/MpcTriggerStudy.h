#ifndef __MPCTRIGGERSTUDY_H__
#define __MPCTRIGGERSTUDY_H__

#include <SubsysReco.h>
#include <string>
#include <vector>
#include <map>
#include <TH1.h>
#include <TH2.h>

class Fun4AllHistoManager;
class PHCompositeNode;
//class PHGlobal;
//class EventHeader;
class TriggerHelper;
class MpcMap;
class MpcCalib;
class mpcRawContainer;
class TTree;
class TFile;
class MpcTrigEmulator;

class MpcTriggerStudy: public SubsysReco
{
public:
  MpcTriggerStudy(const char* outfile = "mpctrigstudy.root");
  virtual ~MpcTriggerStudy() {}

  //  For this analysis we only use Init, process_event;
  //int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

  int Reset        (PHCompositeNode *topNode) { return 1; }

  int IsSingleTower(const int fee576_ch, float& e9, float& ecent, float& erat);
  int Fill_Tile_Arrays(mpcRawContainer *mraw);

protected:
  int GetTriggerNameList( const int RunNumber );

  std::string OutFileName;
  Fun4AllHistoManager *HistoManager;

  // List of all the triggers we care about
  std::map <int, std::string> trig_list;

  static const int MAXTRIG = 12;
  static const int MAX_RBITS = 16;

  static const int TRIG_MBIAS = 0;
  static const int TRIG_MPCANY = 1;

  // Run12 p+p 200 Triggers
  static const int TRIG_MPC_A = 2;
  static const int TRIG_MPC_B = 3;
  static const int TRIG_MPC_C_C = 4;		// (C&C) 
  static const int TRIG_MPC_C_ERT2 = 5;		// (NC&ERT) in BBC events
  //static const int TRIG_MPC_SC_ERT2 = 6;	// (SC&ERT) in BBC events

  static const int TRIG_MPC_A_BBC = 7;		// Trig A in BBC events
  static const int TRIG_MPC_B_BBC = 8;		// Trig B in BBC events
  static const int TRIG_MPC_C_C_BBC = 9;	// (C&C) in BBC events
  static const int TRIG_MPC_C_ERT2_BBC = 10;	// (NC&ERT) in BBC events
  //static const int TRIG_MPC_SC_ERT2_BBC = 11;	// (SC&ERT) in BBC events

  // Run11 p+p Triggers (no 2x2 though)
/*
  static const int TRIG_MPC_A = 2;
  static const int TRIG_MPC_B = 3;		// In run11, this is the 4x4B
  static const int TRIG_MPC_C_ERT2 = 4;
  static const int TRIG_MPC_A_BBC = 5;	// 4x4A in BBC events
  static const int TRIG_MPC_B_BBC = 6;		// 4x4B in BBC events
  static const int TRIG_MPC_C_CERT2_BBC = 7;	// (4x4&ERT) in BBC events
*/

  // Run09 p+p Triggers
/*
  static const int TRIG_MPC4X4A = 2;
  static const int TRIG_MPC2X2 = 3;
  static const int TRIG_MPC4X4CERT2 = 4;
  static const int TRIG_MPC4X4A_BBC = 5;		// 4x4 in BBC events
  static const int TRIG_MPC2X2_BBC = 6;		// 2x2 in BBC events
  static const int TRIG_MPC4X4CERT2_BBC = 7;	// (4x4&ERT) in BBC events
*/

  // Run08 d+Au Triggers
/*
  static const int TRIG_MPC4X4 = 2;
  static const int TRIG_MPC4X4BBC = 3;
  static const int TRIG_MPC2X2 = 5;
  static const int TRIG_MPC4X4_BBC = 7;		// 4x4 in BBC events
  static const int TRIG_MPC4X4BBC_BBC = 8;	// (4x4&BBC) in BBC events
  static const int TRIG_MPC2X2_BBC = 10;	// 2x2 in BBC events
*/

  // Run08 p+p Triggers
/*
  static const int TRIG_MPC4X4A = 2;
  static const int TRIG_MPC4X4B = 3;
  static const int TRIG_MPC4X4C_ERTC = 4;
  static const int TRIG_MPC4X4A_AND_BBC = 5;	// 4x4A&&BBC in MPC events
  static const int TRIG_MPC4X4B_AND_BBC = 6;	// 4x4B&&BBC in MPC events
  static const int TRIG_MPC4X4C_ERTC_AND_BBC = 7;	// 4x4C&&ERTC&&BBC in MPC events
  static const int TRIG_MPC4X4A_BBC = 8;	// 4x4A in BBC events
  static const int TRIG_MPC4X4B_BBC = 9;	// 4x4B in BBC events
  static const int TRIG_MPC4X4C_ERTC_BBC = 10;	// 4x4C_ERTC in BBC events
*/

  // Run12 p+p 510 RBITS
  static const int RBIT_A_N = 0;
  static const int RBIT_A_S = 1;
  static const int RBIT_B_N = 2;
  static const int RBIT_B_S = 3;
  static const int RBIT_C0_S = 4;
  static const int RBIT_C1_S = 5;
  static const int RBIT_C2_S = 6;
  static const int RBIT_C3_S = 7;
  static const int RBIT_C4_S = 8;
  static const int RBIT_C5_S = 9;
  static const int RBIT_C0_N = 10;
  static const int RBIT_C1_N = 11;
  static const int RBIT_C2_N = 12;
  static const int RBIT_C3_N = 13;
  static const int RBIT_C4_N = 14;
  static const int RBIT_C5_N = 15;

  // Run12 p+p 200 RBITS
/*
  static const int RBIT_A_N = 0;
  static const int RBIT_A_S = 1;
  static const int RBIT_B_N = 2;
  static const int RBIT_B_S = 3;
  static const int RBIT_C_N = 4;
  static const int RBIT_C0_S = 5;
  static const int RBIT_C1_S = 6;
  static const int RBIT_C2_S = 7;
  static const int RBIT_C3_S = 8;
  static const int RBIT_C4_S = 9;
  static const int RBIT_C5_S = 10;
*/

  // Run11 p+p RBITS
/*
  static const int RBIT_4x4A_N = 0;
  static const int RBIT_4x4A_S = 1;
  static const int RBIT_4x4B_N = 2;
  static const int RBIT_4x4B_S = 3;
  static const int RBIT_4x4C_N = 4;
  static const int RBIT_4x4C_S = 5;
  static const int RBIT_2x2_N = 6;
  static const int RBIT_2x2_S = 7;
*/

  // Run08 p+p RBITS
/*
  static const int RBIT_4x4A = 0;
  static const int RBIT_4x4B_N = 1;
  static const int RBIT_4x4C_N = 2;
  static const int RBIT_4x4B_S = 3;	// 2x2 in run09
  static const int RBIT_4x4C_S = 4;	// 2x2 in run09
*/

  // Run06 Triggers
/*
  static const int TRIG_MPC4X4A = 2;
  static const int TRIG_MPC4X4B = 3;
  static const int TRIG_MPC4X4C = 4;
  //static const int TRIG_MPC2X2 = 5;
  static const int TRIG_MPCANY_BBC = 6;		// with BBC Coincidence
  //static const int TRIG_MPC4X4A_BBC = 7;
  static const int TRIG_MPC4X4B_BBC = 8;
  static const int TRIG_MPC4X4C_BBC = 9;
  //static const int TRIG_MPC2X2_BBC = 10;
  static const int TRIG_ERT4X4B = 11;		// MPCANY with ERT Coincidence
*/

  // Tree
  TFile *savefile;
  TFile *treefile;
  TTree *ttree;
  UInt_t  f_run;
  UInt_t  f_evt;
  Int_t   f_cross;
  UInt_t  f_trig;
  Float_t f_zvtx;

  TH1 *h_ntrig;
  TH1 *h_ntrigcorr;
  TH1 *h_prescale;
  TH1 *h_nfiles;
  TH2 *h2_ntrig;

  TH2 *h2_ecent[2];
  TH2 *h2_e9[2];
  TH1 *h_erat[2];
  TH1 *h_e9rat[2];
  TH1 *h_dispx;
  TH1 *h_dispy;
  TH1 *h_corrdispx;
  TH1 *h_corrdispy;

  std::vector<TH1*> h_cross;	// crossing pattern for each trigger
  std::vector<TH1*> h_ncross;	// crossing pattern for each trig, north if possible
  std::vector<TH1*> h_scross;	// crossing pattern for each trig, north if possible
  std::vector<TH1*> h_rbits;	// rbits pattern for each trigger

  // These are after cleanup cuts
  std::vector<TH1*> hpt;	// pt dist, both mpc
  std::vector<TH1*> hn_pt;	// pt dist, mpc.n && north bit, if possible
  std::vector<TH1*> hs_pt;	// pt dist, mpc.s && south bit, if possible
  std::vector<TH1*> henergy;	// energy dist, both mpc
  std::vector<TH1*> hn_energy;	// energy dist, mpc.n && north bit, if possible
  std::vector<TH1*> hs_energy;	// energy dist, mpc.s && south bit, if possible

  // This is for all clusters
  std::vector<TH1*> hpt_all;		// pt dist, both mpc
  std::vector<TH1*> hn_pt_all;		// pt dist, mpc.n && north bit, if possible
  std::vector<TH1*> hs_pt_all;		// pt dist, mpc.s && south bit, if possible
  std::vector<TH1*> henergy_all;	// energy dist, both mpc
  std::vector<TH1*> hn_energy_all;	// energy dist, mpc.n && north bit, if possible
  std::vector<TH1*> hs_energy_all;	// energy dist, mpc.s && south bit, if possible

  // This is for the adc, to check where the threshold is set
  std::vector<TH2*> h2_adc;	

  std::vector<TH1*> hesum;

  // Trigger Tiles
  std::vector<TH1*> h2x2;
  std::vector<TH1*> hn_2x2;
  std::vector<TH1*> hs_2x2;
  std::vector<TH1*> h4x4;
  std::vector<TH1*> hn_4x4;
  std::vector<TH1*> hs_4x4;

  std::vector<TH1*> h2x2max;
  std::vector<TH1*> hn_2x2max;
  std::vector<TH1*> hs_2x2max;
  std::vector<TH1*> h4x4max;
  std::vector<TH1*> hn_4x4max;
  std::vector<TH1*> hs_4x4max;

  // Hit Tile distributions
  std::vector<TH2*> h2_2x2max;
  std::vector<TH2*> h2_4x4max;

  TH1* hch_pt[MAXTRIG][576];		// with high pt background cuts
  TH1* hch_energy[MAXTRIG][576];
  //TH1* hch_all_pt[MAXTRIG][576];		// without cuts
  //TH1* hch_all_energy[MAXTRIG][576];
  std::vector<TH2*> h2_energy;	// integrated energy in a tower
  std::vector<TH2*> h2_nhit;	// number of triggers in a tower

  float max2x2;
  float max4x4;
  float max2x2n;
  float max4x4n;
  float max2x2s;
  float max4x4s;
  int   max2x2n_xpos;	// xpos of max hit 2x2 tile
  int   max2x2n_ypos;
  int   max2x2s_xpos;
  int   max2x2s_ypos;
  int   max4x4n_xpos;
  int   max4x4n_ypos;
  int   max4x4s_xpos;
  int   max4x4s_ypos;
  float sums_2x2[2][10][10];		// 2x2 sums (forms a 10x10 grid, but use 12 for 4x4 summing)
  float sums_4x4[2][9][9];		// 4x4 sums (forms a 9x9 grid), we don't consider the edge tiles

  TriggerHelper *trighelp;
//  PHGlobal *global;
//  EventHeader *evtheader;
  MpcMap *mpcmap;
  MpcCalib *mpccalib;
  MpcTrigEmulator *mpctrigemulator;
};

#endif /* __MPCTRIGGERSTUDY_H__ */

