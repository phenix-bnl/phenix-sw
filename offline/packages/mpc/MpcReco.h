#ifndef __MPCRECO_H__
#define __MPCRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class MpcEvent;
class MpcMap;
class MpcCalib;

class MpcReco: public SubsysReco
{
 public:
  MpcReco(const std::string &name = "MPC");
  virtual ~MpcReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int EndRun(const int runno);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  MpcEvent *mMpcEvent;
  MpcMap   *mpcmap;
  MpcCalib *mpccalib;
};

#endif /* __MPCRECO_H__ */

