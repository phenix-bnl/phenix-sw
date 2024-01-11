#ifndef __RECOVERHBDHITS_H__
#define __RECOVERHBDHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverHbdHits: public SubsysReco
{
 public:
  RecoverHbdHits(const std::string &name = "RECOVERHBDHITS");
  virtual ~RecoverHbdHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
