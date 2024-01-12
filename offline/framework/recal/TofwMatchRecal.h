#ifndef __TOFWMATCHRECAL_H__
#define __TOFWMATCHRECAL_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class TofwMatchRecal : public Recalibrator
{

 public:

  TofwMatchRecal(const char*name="TofwMatchRecal");
  virtual ~TofwMatchRecal() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  int InitRun12pp510();

  int runNumber;

  PHCentralTrack *d_cnt;
  PHGlobal *d_global;

  float run8_caltofwsdz(const int run, const int striptofw, const int charge, const float zed, const float mom, const float tofwdz);
  float run8_caltofwsdphi(const int run, const int striptofw, const int charge, const float zed, const float mom, const float tofwdphi);
  float run8_tunetofwsdz(const int run, const int charge, const float mom, const float cent, const float tofwsdz);
  float run8_tunetofwsdphi(const int run, const int charge, const float mom, const float cent, const float tofwsdphi);

  float run12pp510_caltofwsdz(const int charge, const int ized, const float pt, const float tofwdz);
  float run12pp510_caltofwsdphi(const int charge, const int ized, const float pt, const float tofwdphi);

  private:
  float dphi_pos_sigma[10][10][10];
  float dphi_pos_mean[10][10][10];
  float dphi_neg_sigma[10][10][10];
  float dphi_neg_mean[10][10][10];

  float dz_pos_sigma[10][10][10];
  float dz_pos_mean[10][10][10];
  float dz_neg_sigma[10][10][10];
  float dz_neg_mean[10][10][10];

};

#endif /* __TOFWMATCHRECAL_H__ */
