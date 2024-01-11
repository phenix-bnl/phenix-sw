#ifndef __FILLTECEHITS_H__
#define __FILLTECEHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillTecHits: public SubsysReco
{
 public:
  FillTecHits(const std::string &name = "FILLTECHITS");
  virtual ~FillTecHits() {}

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
