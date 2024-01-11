#ifndef __RECOVERTRACKLINEPROJECTIONS_H__
#define __RECOVERTRACKLINEPROJECTIONS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>


class RecoverTrackLineProjections: public SubsysReco
{
 public:
  RecoverTrackLineProjections(const std::string &name = "RECOVERTRACKPROJECTIONS");
  virtual ~RecoverTrackLineProjections() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:
  float GetFloat(const int ival) const;
  float GetFloat(const short int ival) const;

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
