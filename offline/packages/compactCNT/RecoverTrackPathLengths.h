#ifndef __RECOVERTRACKPATHLENGTHS_H__
#define __RECOVERTRACKPATHLENGTHS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverTrackPathLengths: public SubsysReco
{
 public:
  RecoverTrackPathLengths(const std::string &name = "RECOVERTRACKPATHLENGTHS");
  virtual ~RecoverTrackPathLengths() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
