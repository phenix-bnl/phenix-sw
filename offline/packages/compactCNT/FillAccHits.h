#ifndef __FILLACCHITS_H__
#define __FILLACCHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <PHLine.h>
#include <fstream>


class FillAccHits: public SubsysReco
{
 public:
  FillAccHits(const std::string &name = "FILLACCHITS");
  virtual ~FillAccHits() {}

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
