#ifndef __ZDCRECO_H__
#define __ZDCRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class ZdcEvent;

class ZdcReco: public SubsysReco
{
 public:
  ZdcReco(const std::string &name = "ZDC");
  virtual ~ZdcReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  ZdcEvent *mZdcEvent;
};

#endif /* __ZDCRECO_H__ */
