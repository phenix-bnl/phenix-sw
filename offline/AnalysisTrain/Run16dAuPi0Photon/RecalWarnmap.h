#ifndef __RecalWarnmap_H__
#define __RecalWarnmap_H__

#include <math.h>
#include "SubsysReco.h"

class Fun4AllServer;
class recoConsts;
class BunchCross;
class PHGlobal;
class emcClusterContainer;
class emcClusterContent;
class Run16WMap;

class RecalWarnmap : public SubsysReco {

public:
  RecalWarnmap(int input_flag=0, int debug_flag=0);
  virtual ~RecalWarnmap();

  void updateCluster(emcClusterContent * c); // update cluster's dead/warn map
  int Init(PHCompositeNode *topNode); // called during intialization
  int InitRun(PHCompositeNode *topNode); // called for first event when run number is known
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) { return 0; }
  int ResetEvent(PHCompositeNode *topNode) { return 0; }
  int End(PHCompositeNode *topNode);
  const char *Name() const { return "RecalWarnmap"; }
  Run16WMap *TwrVeto;
  int runnumber;

private:
  bool getNodes(PHCompositeNode *topNode);
  static const double pi;
  int debug;

protected:
// o1 std::string _phGlobalNodeName;
  std::string _emcClusterContainerNodeName;
  
// o1  PHGlobal* _phGlobal_ptr;
  emcClusterContainer* _emcClusterContainer_ptr;

  Fun4AllServer *se;
  recoConsts *rc;
};

#endif /* __RecalWarnmap_H__ */
