#ifndef __CRKRECO_H__
#define __CRKRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;
class CrkDAO;

class CrkReco: public SubsysReco
{
 public:
  CrkReco(const std::string &name = "CRK");
  virtual ~CrkReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  int copyWrapper(PHCompositeNode *topNode);
  CrkDAO *crkdao;
};

#endif /* __CRKRECO_H__ */
