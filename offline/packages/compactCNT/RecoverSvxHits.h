#ifndef __RECOVERSVXHITS_H__
#define __RECOVERSVXHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverSvxHits: public SubsysReco
{
 public:
  RecoverSvxHits(const std::string &name = "RECOVERSVXHITS");
  virtual ~RecoverSvxHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif



