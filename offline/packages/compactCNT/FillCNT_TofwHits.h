#ifndef __FILLCNT_TOFWHITS_H__
#define __FILLCNT_TOFWHITS_H__

#include <SubsysReco.h>

class PHCentralTrack;

class FillCNT_TofwHits: public SubsysReco
{
 public:
  FillCNT_TofwHits(const std::string &name = "FILLCNT_TOFWHITS");
  virtual ~FillCNT_TofwHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  float get_dphi(const float x1, const float y1, const float x2, const float y2) const;
  float get_dz(const float z1, const float z2) const;

};

#endif
