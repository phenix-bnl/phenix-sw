#ifndef __FILLCNT_TRACKHITS_H__
#define __FILLCNT_TRACKHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class PHCentralTrack;

class FillCNT_TrackHits: public SubsysReco
{
 public:
  FillCNT_TrackHits(const std::string &name = "FILLCNT_TRACKHITS");
  virtual ~FillCNT_TrackHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  std::string inputnodename[2];

#ifdef DUMP
  std::ofstream dumprecover;
#endif


};

#endif
