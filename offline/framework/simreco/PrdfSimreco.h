#ifndef __PRDFSIMRECO_H__
#define __PRDFSIMRECO_H__

#include "SubsysReco.h"

class PHRawOManager;
class PHCompositeNode;

class PrdfSimreco: public SubsysReco
{
 public:
  PrdfSimreco(const std::string &name = "PRDF", const char *prdfOutputFile = "sim.prdf", int runNumb = -1);
  virtual ~PrdfSimreco();

  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

 protected:

  PHRawOManager *prdfOut;
  PHCompositeNode *prdfNode;
};

#endif /* __PRDFSIMRECO_H__ */
