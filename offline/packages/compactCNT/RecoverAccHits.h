#ifndef __RECOVERACCHITS_H__
#define __RECOVERACCHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverAccHits: public SubsysReco
{
 public:
  RecoverAccHits(const std::string &name = "RECOVERCRKHITS");
  virtual ~RecoverAccHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
