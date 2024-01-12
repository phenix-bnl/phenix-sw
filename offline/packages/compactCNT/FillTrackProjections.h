#ifndef __FILLTRACKPROJECTIONS_H__
#define __FILLTRACKPROJECTIONS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillTrackProjections: public SubsysReco
{
 public:
  FillTrackProjections(const std::string &name = "FILLTRACKPROJECTIONS");
  virtual ~FillTrackProjections() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef useIntflag
  int FloatToInt(const float rval) const;
#else
  short int FloatToInt(const float rval) const;
#endif

#ifdef DUMP
  std::ofstream dumpfile;
#endif

};

#endif