#ifndef __RECOVERTOFWHITS_H__
#define __RECOVERTOFWHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverTofwHits: public SubsysReco
{
 public:
  RecoverTofwHits(const std::string &name = "RECOVERTOFWHITS");
  virtual ~RecoverTofwHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
