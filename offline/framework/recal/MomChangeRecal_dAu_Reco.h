#ifndef __momchangeRecal_dAu_RECO_H__
#define __momchangeRecal_dAu_RECO_H__

#include "Recalibrator.h"
#include <string>

class PHCompositeNode;
class TH1;
class TProfile;

class MomChangeRecal_dAu_Reco : public Recalibrator
{
 public:
  // This code is a recalibrator for the momentum correction for run3 dAu
  // it is based on the code mom_change written by Felix Matathias, Sean Leckey & Tom Hemmick
  // which is checked into CVS in offline/analysis/momdau
  //  --SCC 1/25/06

  /// ctor.
  MomChangeRecal_dAu_Reco(const char* name = "MomChangeRecal_dAu_Reco");
  virtual ~MomChangeRecal_dAu_Reco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  void SetScaleFactor(const float val) { ScaleFactor=val; return;}

 private:

  float ScaleFactor;
  float refRadius;              //dch reference radius (cm)
  float dx[2];   //  X offsets in cm
  float dy[2];   //  Y offsets in cm

  float new_alpha (float alpha, float phi, int dcarm);
  float delta_phi0(float del_alpha);

  //these sub programs are taken from mom_changer
  float momentum_changer  (float mom, float alpha, float zed, float the0, float phi, int dcarm);
  float momentum_predictor(float mom, float alpha, float zed, float the0);

  TH1 *dAumomold;
  TH1 *dAumomnew;
  TH1 *dAumomdiff;
  TProfile *dAumomprof;
};

#endif /* __MOMCHANGERECAL_DAU_RECO_H__ */
