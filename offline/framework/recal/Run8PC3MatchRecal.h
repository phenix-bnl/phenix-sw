#ifndef __RUN8PC3MATCHRECAL_H__
#define __RUN8PC3MATCHRECAL_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class Run8PC3MatchRecal : public Recalibrator
{
 public:
  Run8PC3MatchRecal(const char*name="Run8PC3MatchRecal");
  virtual ~Run8PC3MatchRecal() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  int runNumber;

  PHCentralTrack *d_cnt;
  PHGlobal *d_global;

  int Calibrate_Run8dAu();
  int Calibrate_Run8pp();

  float calculate_pc3sdphi_pp (const short charge, const float zed, const float phi, const float pt, const float dphi);
  float calculate_pc3sdphi_dAu(const float alpha , const float zed, const float phi, const float pt, const float dphi);
  float calculate_pc3sdz_pp   (const short charge, const float zed, const float phi, const float pt, const float dz);
  float calculate_pc3sdz_dAu  (const float alpha , const float zed, const float phi, const float pt, const float dz);

 protected:

};

#endif /* __RUN8PC3MATCHRECAL_H__ */
