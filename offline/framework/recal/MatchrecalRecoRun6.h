#ifndef MATCHRECALRECORUN6_H
#define MATCHRECALRECORUN6_H

#include <Recalibrator.h>

class PHCompositeNode;
class PHCentralTrack;

class MatchrecalRecoRun6 : public Recalibrator
{
 public:
  MatchrecalRecoRun6(const std::string &name="MatchrecalRecoRun6");
  virtual ~MatchrecalRecoRun6() {}
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  void initParameters();
  float GetPC3sdphi(const int iarm, const int ialpha, const int ized, 
		    const float pt, const float pc3dphi);
  float GetPC3sdz(const int iarm, const int ialpha, const int ized, 
		  const float pt, const float pc3dz);
  float GetExp(const double* par, const float pt) const;
  float GetDphiDz(const float dphidz, const float mean, const float sigma) const;
  float GetSigma(const double* par, const float pt) const;
  float GetSigmaParameter(const double* par, const double cent) const;
  int dcArm(const float phi) const;
  int Alpha(const float alpha) const;
  int Zed(const float zed) const;

  enum {NARM=2, NCH=2, NZED=4, NCENT=1};

 private:

  // Straight from MatchingParameters.h
  double PC3_dphi_mean[NARM][NCH][NZED][3];
  double PC3_dphi_sigma[NARM][3];
  double PC3_dphi_sigma_scale[NARM][NZED][3];
  double PC3_dz_mean_highpt[NARM][NCH][NZED][2];
  double PC3_dz_mean_lowpt[NARM][NCH][NZED][2];
  double PC3_dz_mean_offset_highpt[NARM][NCH][NZED][NCENT];
  double PC3_dz_mean_offset_lowpt[NARM][NCH][NZED][NCENT];
  double PC3_dz_sigma_offset[NARM][5];
  double PC3_dz_sigma_scale[NARM][NZED][3];
  double PC3_dz_sigma_slope[NARM][NCH];
};

#endif // MINIRECAL_H
