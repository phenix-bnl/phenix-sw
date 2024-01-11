#ifndef __FILLTOFEHITS_H__
#define __FILLTOFEHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillTofeHits: public SubsysReco
{
 public:
  FillTofeHits(const std::string &name = "FILLTOFEHITS");
  virtual ~FillTofeHits() {}

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
