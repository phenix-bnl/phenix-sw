#ifndef T0RECO_H__
#define T0RECO_H__

#include <SubsysReco.h>

class PHCompositeNode;

class T0Reco: public SubsysReco
{
 public:
  T0Reco(const char *name = "T0");
  virtual ~T0Reco() {}

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __T0RECO_H__ */
