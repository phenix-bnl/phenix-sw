#ifndef __PREVIOUSEVENTRECO_H__
#define __PREVIOUSEVENTRECO_H__

#include "SubsysReco.h"
#include <string>

class PHCompositeNode;

class PreviousEventReco: public SubsysReco
{
 public:
  PreviousEventReco(const std::string &name = "PREVIOUSEVENT");
  virtual ~PreviousEventReco() {}

  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __PREVIOUSEVENTRECO_H__ */
