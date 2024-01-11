#ifndef __RECOVERPADHITS_H__
#define __RECOVERPADHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverPadHits: public SubsysReco
{
 public:
  RecoverPadHits(const std::string &name = "RECOVERPADHITS");
  virtual ~RecoverPadHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover[3];
#endif


};

#endif
