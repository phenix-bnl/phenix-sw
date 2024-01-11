#ifndef __MPCEMCTRIGEMULATOR_H__
#define __MPCEMCTRIGEMULATOR_H__

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

class MpcEmcTrigEmulator: public SubsysReco
{
public:
  MpcEmcTrigEmulator(const char* name = "mpcemctrig.root");
  virtual ~MpcEmcTrigEmulator() {}

  //  For this analysis we only use Init, process_event;
  //int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

  int Fill_Tile_Arrays(mpcTowerContainer *towers);

protected:

  std::string OutFileName;
  MpcMap *mpcmap;
  MpcCalib *mpccalib;

  TFile *savefile;

  static const int MAXTRIG = 4; // A,B,C,2

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

  // Evaluation histograms
  std::vector<TH2*> h2_energy;  // integrated energy in a tower
  std::vector<TH2*> h2_nhit;    // number of triggers in a tower

};

#endif /* __MPCEMCTRIGEMULATOR_H__ */

