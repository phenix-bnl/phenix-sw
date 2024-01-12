#ifndef SYNCRECO_H__
#define SYNCRECO_H__

#include <SubsysReco.h>
#include <string>

class PHCompositeNode;

class SyncReco: public SubsysReco
{
 public:
  SyncReco(const std::string &name = "SYNC");
  virtual ~SyncReco() {}

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __SYNCRECO_H__ */
