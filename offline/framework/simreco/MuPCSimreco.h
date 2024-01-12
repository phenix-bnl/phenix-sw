#ifndef __MUPCRECO_H__
#define __MUPCRECO_H__

#include "SubsysReco.h"

class uIDLL1Road;
class MuPCCluster;
class MuPCSnglCluster;
class MuPCAnalysis;
class uIDLL1Analysis;
class mupcghitWrapper;
class MuPCSimreco: public SubsysReco
{
 public:
  MuPCSimreco(int version=1);
  virtual ~MuPCSimreco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

 protected:
  int version;

  uIDLL1Road *uidll1road;
  MuPCCluster* mupc1cluster;
  MuPCCluster* mupc2cluster;
  MuPCCluster* mupc3cluster;
  uIDLL1Analysis * uidll1analysis;
  MuPCAnalysis * mupcAnalysis;

  mupcghitWrapper* mupc1ghit;
  mupcghitWrapper* mupc2ghit;
  mupcghitWrapper* mupc3ghit;

  bool CreateNodeTree(PHCompositeNode *topNode);
};

#endif /* __MUPCRECO_H__ */
