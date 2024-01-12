#ifndef __EVASIMRECO_H__
#define __EVASIMRECO_H__

#include "SubsysReco.h"
#include "TFile.h"
#include "TNtuple.h"

class PHCompositeNode;

class EvaSimreco: public SubsysReco
{
 public:
  EvaSimreco(const char *name = "EVA", const int evaMode = 0);
  virtual ~EvaSimreco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}
  int getMode () {return thisMode;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

  const int              NTPL_PARAMPAD;
  const int              NTPL_PARAMCGL;
  const int              NTPL_PARAMPAIR;
  const int              NPAIR_DATA;
  const int              NTPL_PARAMEMC;
  const int              NTPL_REACPLANE;
  int                    thisMode;
  TFile                  *evaluateFile;
  TNtuple                *evaluatePadNtuple;
  TNtuple                *evaluateEmcNtuple;
  TNtuple                *evaluateCglNtuple;
  TNtuple                *evaluatePairNtuple;
  TNtuple                *evaluateReacPlaneNtuple;


};

#endif /* __EVASIMRECO_H__ */
