#ifndef _HISTEMC_H
#define _HISTEMC_H

#include "SubsysReco.h"

class PHCompositeNode;

class QAEmc: public SubsysReco
{
 public:
  QAEmc(const char *name = "QAEmc"): SubsysReco(name) {}
  virtual ~QAEmc() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int emcHistFill_cluscont(PHCompositeNode *topNode);
  int emcHistFill_staftable(PHCompositeNode *topNode);
};

#endif  /*_HISTEMC_H */

//EOL
