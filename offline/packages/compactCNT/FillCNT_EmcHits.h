#ifndef __FILLCNT_EMCHITS_H__
#define __FILLCNT_EMCHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class PHCentralTrack;

class FillCNT_EmcHits: public SubsysReco
{
 public:
  FillCNT_EmcHits(const std::string &name = "FILLCNT_EMCHITS");
  virtual ~FillCNT_EmcHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  float get_dphi(const float x1, const float y1, const float x2, const float y2) const;
  float get_dz(const float z1, const float z2) const;

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
