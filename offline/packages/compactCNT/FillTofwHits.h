#ifndef __FILLTOFWHITS_H__
#define __FILLTOFWHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>

class FillTofwHits: public SubsysReco
{
 public:
  FillTofwHits(const std::string &name = "FILLTOFWHITS");
  virtual ~FillTofwHits() {}

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
