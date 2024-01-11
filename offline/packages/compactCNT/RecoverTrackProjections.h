#ifndef __RECOVERTRACKPROJECTIONS_H__
#define __RECOVERTRACKPROJECTIONS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>


class RecoverTrackProjections: public SubsysReco
{
 public:
  RecoverTrackProjections(const std::string &name = "RECOVERTRACKPROJECTIONS");
  virtual ~RecoverTrackProjections() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:


#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
