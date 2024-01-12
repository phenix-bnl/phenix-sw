#ifndef __PIDRECAL_DAU_RECO_H__
#define __PIDRECAL_DAU_RECO_H__

#include <Recalibrator.h>


class PHCentralTrack;
class PHCompositeNode;
class TH2;

class Pidrecal_dAu_Reco : public Recalibrator
{
  //     This code is a recalibrator for the TOF time-of-flight for run3 dAu
  // it is based on the code tofHelper written by Felix Matathias
  // which is checked into CVS in offline/analysis/tofHelper
  //     Much of the implementation of the tofHelper subprograms 
  // is taken from spectraMaker.C by Felix Matathias
  // it's located at offline/analysis/hadronSelect/SpectraMaker/
  // or workarea/felice/run03/pid/offline/analysis/hadronSelect/spectraMaker
  //  --SCC 1/25/06

public:
  Pidrecal_dAu_Reco(const std::string &name = "Pidrecal_dAu_Reco");
  virtual ~Pidrecal_dAu_Reco() {}

  //  Standard methods
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void Print(const std::string &what = "ALL") const;
  int isValidRun(const int runno) const;

protected:
  TH2 *TOFmomtof;
  TH2 *TOFmomtofP;
  TH2 *TOFmomtofK;
  TH2 *TOFmomtofPi;

  float this_globalTzero;
  float t0_offset[768];
  float t0_offset_pos[768];
  float t0_offset_neg[768];
  int dead_slat[800];

  //  Nodes we need while running...
  PHCentralTrack      *d_cnt;

  // set parameters -- these values are from tofHelper
  void InitializeDeadSlats();
  void SetTimingOffsets();

  // these sub programs are all from tofHelper in offline/analysis/tofHelper
  float calcMeasM2(float t, float L, float p);
  float d_TOF_z_match_new(float p, float dv, float zed);
  float d_TOF_phi_match_new(float p, float dv);

  double IsPion  (double m2, double p);
  double IsKaon  (double m2, double p);
  double IsProton(double m2, double p);

  double sigmaM2_analytical(double m2, double p);
  double meanPi2(double mom);
  double meanK2 (double mom);
  double meanP2 (double mom);

};

#endif /* __PIDRECAL_DAU_RECO_H__ */
