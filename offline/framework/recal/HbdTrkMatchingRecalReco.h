#ifndef __HBDTRKMATCHINGRECALRECO_H__
#define __HBDTRKMATCHINGRECALRECO_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;
class hbdAdcCalib;

class HbdTrkMatchingRecalReco : public Recalibrator
{
 public:
  HbdTrkMatchingRecalReco(const std::string &name = "HbdTrkMatchingRecalReco");
  virtual ~HbdTrkMatchingRecalReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  int isValidRun(const int runno) const;

 private:
  float get_sdz   (float hbddz, float mom);
  float get_sdphi (float hbddphi, float mom);
  double get_sdz_Run10(float hbddz, float mom);
  double get_sdphi_Run10(float hbddphi, float mom, int charge);

  hbdAdcCalib *calib;

  int skip_flag;
  int clst_flag;
  int mc_flag;
  int runnum;
};

#endif /*  __HBDTRKMATCHINGRECALRECO_H__ */

