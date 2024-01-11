#ifndef __FILLTRACKHITS_H__
#define __FILLTRACKHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>
#include <string>

class FillTrackHits: public SubsysReco
{
 public:
  FillTrackHits(const std::string &name = "FILLTRACKHITS");
  virtual ~FillTrackHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumpfile[2];
#endif

  std::string outputnodename[2];
};

#endif
