#ifndef __RECOVEREMCHITS_H__
#define __RECOVEREMCHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverEmcHits: public SubsysReco
{
 public:
  RecoverEmcHits(const std::string &name = "RECOVEREMCHITS");
  virtual ~RecoverEmcHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
