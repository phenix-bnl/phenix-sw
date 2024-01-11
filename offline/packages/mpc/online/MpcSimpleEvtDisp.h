#ifndef __MPCSIMPLEEVTDISP_H__
#define __MPCSIMPLEEVTDISP_H__

#include "SubsysReco.h"
#include <string>

class PHCompositeNode;
class TH1;
class TH2;
class TCanvas;
class TriggerHelper;
class MpcMap;
class mpcRawContainer;
class mpcTowerContainer;
class mpcClusterContainer;

class MpcSimpleEvtDisp: public SubsysReco
{
 public:
  MpcSimpleEvtDisp(const char* outfile = "mpcdisplay.root");
  virtual ~MpcSimpleEvtDisp() {}

  //  For this analysis we only use Init, process_event;
  //int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);

  int Reset        (PHCompositeNode *topNode) { return 1; }

protected:

  int InArea(const float x, const float y);
  void FillRawDisplay();
  void FillTowerDisplay();

  int runnumber;
  mpcRawContainer *mpcraw;
  mpcTowerContainer *mpctowers;
  mpcClusterContainer *mpcclus;

  std::string OutFileName;

  TCanvas *DisplayCanvas;
  TH2 *MpcDisplay[2];
  TH1 *h_ndriver[20];	// number of towers in each driver
  TH1 *h_e[20];		// for event by event studies
  TH1 *h_emean[20];	// Tower E mean for each event in a driver
  TH1 *h_erms[20];	// Tower RMS for each event in a driver

  TriggerHelper *trighelp;
  MpcMap *mpcmap;

};

#endif /* __MPCSIMPLEEVTDISP_H__ */



