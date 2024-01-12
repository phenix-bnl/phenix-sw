#ifndef __FCALRECO_H__
#define __FCALRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;
class FclEvent;

class FcalReco: public SubsysReco
{
 public:
  FcalReco(const char *name = "FCAL");
  virtual ~FcalReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  void Print(const std::string&) const {}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  FclEvent *mFclEvent;

};

#endif /* __FCALRECO_H__ */
