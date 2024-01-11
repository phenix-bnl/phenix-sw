#ifndef __MPCCHECK_H__
#define __MPCCHECK_H__

#include <SubsysReco.h>
#include <string>

class Fun4AllHistoManager;
class PHCompositeNode;
class TH1;
class TH2;
class TCanvas;
class TFile;
//class PHGlobal;
class MpcMap;
class MpcCalib;
class TriggerHelper;


class MpcCheck: public SubsysReco
{
public:
  MpcCheck(const char* outfile = "mpctrigstudy.root");
  virtual ~MpcCheck() {}

  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

  int Reset        (PHCompositeNode *topNode) { return 1; }

protected:
  std::string OutFileName;
  //Fun4AllHistoManager *HistoManager;
  TFile *savefile;

  MpcMap *mpcmap;
  MpcCalib *mpccalib;

  int EventCounter;

  //TCanvas *DisplayCanvas;
  TH2 *h2s_bbcvsmpc;
  TH2 *h2n_bbcvsmpc;
  TH2 *h2_amu;
  TH2 *h2_tdc_cross;

  TH1 *htdc[576];
  TH1 *hlopost[576];
  TH1 *hlopre[576];
  TH1 *hadc[576];
  TH2 *hadc_amu[576];
  TH1 *hadc_driver[20];
  TH1 *hhipost[576];
  TH1 *hhipre[576];
  TH1 *hhi[576];
  TH1 *hlobits[576];
  TH1 *hhibits[576];
  TH1 *hx[2][18][18];
  TH1 *hy[2][18][18];
  //TH2 *TrigCounts;

  TH1 *h_ntrig;

  TriggerHelper *trighelp;
//  PHGlobal *global;
//  EventHeader *evtheader;
};

#endif /* __MPCCHECK_H__ */



