#ifndef __RECOVERTRACKHITS_H__
#define __RECOVERTRACKHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverTrackHits: public SubsysReco
{
 public:
  RecoverTrackHits(const std::string &name = "RECOVERTRACKHITS");
  virtual ~RecoverTrackHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover[2];
#endif

  std::string inputnodename[2];
  std::string outputnodename[2];
};

#endif
