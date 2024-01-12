#ifndef __RPSUMXYRECO_H__
#define __RPSUMXYRECO_H__

#include "SubsysReco.h"

class RpSumXYObject;

class RpSumXYReco : public SubsysReco
{
 public:
  
  RpSumXYReco(const std::string &name ="RPSUMXY");
  
  virtual ~RpSumXYReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:
  bool CreateNodeTree(PHCompositeNode* topNode);

  RpSumXYObject* d_rpsumxy;
  int myeve;
  int myrun;
};

#endif
