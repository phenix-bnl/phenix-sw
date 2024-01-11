#ifndef __FILLTRACKPATHLENGTHS_H__
#define __FILLTRACKPATHLENGTHS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillTrackPathLengths: public SubsysReco
{
 public:
  FillTrackPathLengths(const std::string &name = "FILLTRACKPATHLENGTHS");
  virtual ~FillTrackPathLengths() {}

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
