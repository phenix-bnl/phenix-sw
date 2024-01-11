#ifndef __RECOVERDCHHITS_H__
#define __RECOVERDCHHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverDchHits: public SubsysReco
{
 public:
  RecoverDchHits(const std::string &name = "RECOVERDCHHITS");
  virtual ~RecoverDchHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
