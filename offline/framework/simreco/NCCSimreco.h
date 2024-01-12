#ifndef __NCCSIMRECO_H__
#define __NCCSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;

class NCCSimreco: public SubsysReco
{
 public:
  NCCSimreco(const char *name = "NCC");
  virtual ~NCCSimreco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

};

#endif /* __NCCSIMRECO_H__ */
