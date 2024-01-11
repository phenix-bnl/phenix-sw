#ifndef __RECOVERTOFEHITS_H__
#define __RECOVERTOFEHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverTofeHits: public SubsysReco
{
 public:
  RecoverTofeHits(const std::string &name = "RECOVERTOFEHITS");
  virtual ~RecoverTofeHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
