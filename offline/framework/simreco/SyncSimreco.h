#ifndef __SYNCSIMRECO_H__
#define __SYNCSIMRECO_H__

#include "SubsysReco.h"
#include <string>

class PHCompositeNode;
class recoConsts;

class SyncSimreco: public SubsysReco
{
 public:
  SyncSimreco(const std::string &name = "SYNCSIM");
  virtual ~SyncSimreco() {}

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

protected:
  int CreateNodeTree(PHCompositeNode *topNode);

private:
  recoConsts *rc;

};

#endif /* __SYNCSIMRECO_H__ */
