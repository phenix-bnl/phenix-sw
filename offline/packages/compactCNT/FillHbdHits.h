#ifndef __FILLHBDHITS_H__
#define __FILLHBDHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>


class FillHbdHits: public SubsysReco
{
 public:
  FillHbdHits(const std::string &name = "FILLHBDHITS");
  virtual ~FillHbdHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumpfile;
#endif

};

#endif
