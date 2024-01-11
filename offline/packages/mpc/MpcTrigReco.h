#ifndef __MPCTRIGRECO_H__
#define __MPCTRIGRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;
class MpcMap;
class MpcTriggerMapping;
class MpcCalib;
class mpcTowerContent;
class mpcTowerContainer;
class mpc2x2Content;
class mpc2x2Container;
class mpc4x4Content;
class mpc4x4Container;


class MpcTrigReco: public SubsysReco
{
public:
  MpcTrigReco(const std::string &name = "MpcTrigReco");
  virtual ~MpcTrigReco() {}

  //int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);
  //void Print() const;
  
  void PrintEvent(PHCompositeNode* topNode) const;
  
  //adds mpc2x2Container to node tree
  int CreateNodeTree(PHCompositeNode *topNode);

private:
  MpcMap *mpcmap;
  MpcCalib *mpccalib;
  MpcTriggerMapping *mpctrigmap;

  mpc2x2Container *mpc2x2;
  mpc4x4Container *mpc4x4;
  mpcTowerContainer *mpctow;
};

#endif /* __MPCTRIGRECO_H__ */

