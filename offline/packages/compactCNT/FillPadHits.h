#ifndef __FILLPADHITS_H__
#define __FILLPADHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillPadHits: public SubsysReco
{
 public:
  FillPadHits(const std::string &name = "FILLPADHITS");
  virtual ~FillPadHits() {}

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
  std::ofstream dumpfile[3];
#endif

};

#endif
