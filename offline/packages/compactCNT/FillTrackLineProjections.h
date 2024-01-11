#ifndef __FILLTRACKLINEPROJECTIONS_H__
#define __FILLTRACKLINEPROJECTIONS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillTrackLineProjections: public SubsysReco
{
 public:
  FillTrackLineProjections(const std::string &name = "FILLTRACKLINEPROJECTIONS");
  virtual ~FillTrackLineProjections() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef useIntflag_lineproj
  int FloatToInt(const float rval) const;
#else
  short int FloatToInt(const float rval) const;
#endif

#ifdef DUMP
  std::ofstream dumpfile;
#endif
};

#endif
