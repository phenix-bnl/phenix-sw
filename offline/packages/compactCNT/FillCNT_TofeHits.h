#ifndef __FILLCNT_TOFEHITS_H__
#define __FILLCNT_TOFEHITS_H__

#include <SubsysReco.h>

class PHCentralTrack;

class FillCNT_TofeHits: public SubsysReco
{
 public:
  FillCNT_TofeHits(const std::string &name = "FILLCNT_TOFEHITS");
  virtual ~FillCNT_TofeHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  float get_dphi(const float x1, const float y1, const float x2, const float y2) const;
  float get_dz(const float z1, const float z2) const;

};

#endif
