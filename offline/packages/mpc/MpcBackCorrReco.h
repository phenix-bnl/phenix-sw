#ifndef __MPC_BACK_CORR_RECO_H__
#define __MPC_BACK_CORR_RECO_H__

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
class PHGlobal;
class TriggerHelper;
class TFile;
class TVector3;
class TLorentzVector;
class mpcClusterContainer;
class mpcClusterContent;
class RunHeader;
class MpcMap;

class MpcBackCorrReco: public SubsysReco
{
 public:
  MpcBackCorrReco(const char* outfile = "GoldenRun_zvtx.txt");
  virtual ~MpcBackCorrReco();
  
  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  
  float GetEta(int ich);
  float GetEta(mpcClusterContent* clus);
  float GetOffset(float eta, float cent);
 protected:
  
  TriggerHelper *trighelp;
  RunHeader     *runheader;
  
  PHGlobal *global;
  mpcClusterContainer* mpcclus;
  int runnum;
  bool daflag;
  
  MpcMap* mpcmap;
  float offset[4][9];

  
};

#endif /* __MPC_BACK_CORR_RECO_H__ */



