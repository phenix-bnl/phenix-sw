#ifndef __MPC_GAIN_CORR_RECO_H__
#define __MPC_GAIN_CORR_RECO_H__

#include "SubsysReco.h"
#include <iostream>
#include <fstream>
#include <string>
#include <TString.h>
class Fun4AllHistoManager;
class PHCompositeNode;
class TH1;
class TH2;
class TCanvas;
class TriggerHelper;
class TFile;
class TVector3;
class TLorentzVector;
class mpcTowerContainer;
class mpcTowerContent;
class RunHeader;
class MpcMap;

class MpcGainCorrReco: public SubsysReco
{
 public:
  MpcGainCorrReco(const char* outfile = "GoldenRun_zvtx.txt");
  virtual ~MpcGainCorrReco();
  
  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  int InitGainParam(int type, float time);
  int SetGainCorr  (float time); // 0 => pp, 1 => da
  float GetTime    (int run);
 protected:
  TriggerHelper *trighelp;
  RunHeader     *runheader;

  mpcTowerContainer* mpctower;
  int runnum;
  bool daflag;

  float scaleparamN[2];
  float scaleparamS[2];
  float gainparam[2][576];  // 2 is for slope/intercept
  MpcMap* mpcmap;
  float gaincorr[576];
};

#endif /* __MPC_BACK_CORR_RECO_H__ */
