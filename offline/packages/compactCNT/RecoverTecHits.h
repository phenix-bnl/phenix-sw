#ifndef __RECOVERTECHITS_H__
#define __RECOVERTECHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class RecoverTecHits: public SubsysReco
{
 public:
  RecoverTecHits(const std::string &name = "RECOVERTECHITS");
  virtual ~RecoverTecHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

  float GetFloat(const int ival) const;
  float GetFloat(const short int ival) const;

#ifdef DUMP
  std::ofstream dumpfile;
#endif

};

#endif
