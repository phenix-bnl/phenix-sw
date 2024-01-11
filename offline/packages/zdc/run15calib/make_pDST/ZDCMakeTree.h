#ifndef __ZDCMAKETREE_H__
#define __ZDCMAKETREE_H__

#include "SubsysReco.h"
#include "PHCompositeNode.h"

#include <string>

class PHCompositeNode;
class SyncObject;
class TrigLvl1;
class ZdcRaw;
class BbcOut;


class TFile;
class TTree;
class TH1F;

class ZDCMakeTree: public SubsysReco
{

 public:
  ZDCMakeTree(const char *name="ZDCMakeTree");
  ~ZDCMakeTree(void);
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  void SetOutputName(char* fname){outfilename=fname;}

  int ReInit(void);


 private:

  int nevt;
  std::string outfilename;

  // SyncObject
  int runnumber;
  int eventnumber;

  // TrigLvl1
  unsigned int trigscaled;
  unsigned int cross;

  // BbcOut
  float zvtx;
  float zvtxerror;
 
  // ZdcRaw
  short zdc_adc[40];
  short zdc_tdc0[40];
  short zdc_tdc1[40];

  TFile *fout;
  TTree *tree;

};

#endif /* __MAKEZDCTREE_H__ */
