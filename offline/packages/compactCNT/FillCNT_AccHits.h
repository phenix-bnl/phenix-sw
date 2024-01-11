#ifndef __FILLCNT_ACCHITS_H__
#define __FILLCNT_ACCHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>


class PHCentralTrack;

class FillCNT_AccHits: public SubsysReco
{
 public:
  FillCNT_AccHits(const std::string &name = "FILLCNT_ACCHITS");
  virtual ~FillCNT_AccHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

#ifdef DUMP
  std::ofstream dumpfile;
#endif
  
};

#endif
