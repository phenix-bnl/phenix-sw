#ifndef __TRIGRECO_H__
#define __TRIGRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;

class TrigReco: public SubsysReco
{
 public:
  TrigReco(const char *name = "TRIG");
  virtual ~TrigReco() {}

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __TRIGRECO_H__ */
