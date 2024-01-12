#ifndef __FILLCNT_PADHITS_H__
#define __FILLCNT_PADHITS_H__

#include <SubsysReco.h>

class PHCentralPad;

class FillCNT_PadHits: public SubsysReco
{
 public:
  FillCNT_PadHits(const std::string &name = "FILLCNT_PADHITS");
  virtual ~FillCNT_PadHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  float get_dphi(const float x1, const float y1, const float x2, const float y2) const;
  float get_dz(const float z1, const float z2) const;

};

#endif