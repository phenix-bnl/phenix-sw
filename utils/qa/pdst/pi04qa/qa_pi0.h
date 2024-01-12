#ifndef __QA_PI0_H__
#define __QA_PI0_H__

#include "SubsysReco.h"

#define MAX_EVBUF_DEPTH 7
#define NUM_CENT_CLASSES 9
#define NUM_VTX_CLASSES 12

#define QAROOT

#include <string>
#include <KEvent.h>
#include <KEventCent.h>

class Fun4AllServer;
class TH1F;
class THmulf;
class CentralityReco;
class recoConsts;
class BunchCross;

class QA_pi0 : public SubsysReco {

public:

  QA_pi0(const char* outfile = "EXAMPLE.root");
  virtual ~QA_pi0();

#ifndef QAROOT
  int End(PHCompositeNode *topNode);
#endif

  int InitRun(PHCompositeNode *topNode); // called for first event when run number is known
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode) {return 0;}
  void SetOutputfile(const char *name) {outputfile = name;}
  void SetIgnoreCentrality(bool b=true){ignore_centrality=b;}

  const char *Name() const
    {
      return "QA_pi0";
    }

 protected:

  int buf_add[NUM_CENT_CLASSES][NUM_VTX_CLASSES]; 

  int mixed;
  int nclus;
  int nevents;

  bool ignore_centrality;

  std::string outputfile;

  mEmcGeometryModule *EmcGeo;
  Combination cmb;

  void cmbReal(int nc, int index, int cent, int ivtx);
  void cmbMixed(int cent, int ivtx);
  void readBadTowers();

  Fun4AllServer *se;
  CentralityReco * centReco;
  recoConsts *rc;

  TH1F *event_counter;
  TH1F *evts_cent;

  // stochastic cut variables

  THmulf *hcluster;
  THmulf *ecompactness;
  THmulf *padisp_ratio;

  // pi0 QA
  THmulf *elecheck;

  THmulf *evts_mult;
  THmulf *tower;
  THmulf *toftower;
  THmulf *gghs;
  THmulf *tofc;
  THmulf *bbczdc;

  KEvent evarray[MAX_EVBUF_DEPTH][NUM_CENT_CLASSES][NUM_VTX_CLASSES];

  KEventCent evCent;

  std::string OutFileName;

};

#endif /* __QA_PI0_H__ */
