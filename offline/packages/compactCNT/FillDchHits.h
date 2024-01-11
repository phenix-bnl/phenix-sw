#ifndef __FILLDCHHITS_H__
#define __FILLDCHHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillDchHits: public SubsysReco
{
 public:
  FillDchHits(const std::string &name = "FILLDCHHITS");
  virtual ~FillDchHits() {}

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
