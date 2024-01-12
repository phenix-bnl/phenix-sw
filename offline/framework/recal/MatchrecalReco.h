#ifndef __MatchRECALRECO_H__
#define __MatchRECALRECO_H__

#include <string>
#include "Recalibrator.h"

class PHCentralTrack;
class PHCompositeNode;

class MatchrecalReco : public Recalibrator
{
 public:
  MatchrecalReco(const char*name="MatchrecalReco");
  virtual ~MatchrecalReco() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  enum {
    NARM = 2,
    NCH  = 2,
    NZED = 4,
    NCENT = 9
  };

  int runNumber;
  PHCentralTrack *d_cnt;

  // Run4
  int InitRun4();
  int Burn_match_Run4(PHCompositeNode* topNode);
  int run_Burn_Run4_match;

  // Ids
  // alpha id
  // 0 --> alpha<0
  // 1 --> alpha>0
  int Alpha(const float alpha) const;

  // zed id
  // 0 --> -80 <= zed < -40 cm
  // 1 --> -40 <= zed <   0 cm
  // 2 -->   0 <= zed <  40 cm
  // 3 -->  40 <= zed <  80 cm
  int Zed(const float zed) const;

  // centrality id
  // 0 -->  0 - 10 %
  // 1 --> 10 - 20 %
  // 2 --> 20 - 30 %
  // 3 --> 30 - 40 %
  // 4 --> 40 - 50 %
  // 5 --> 50 - 60 %
  // 6 --> 60 - 70 %
  // 7 --> 70 - 80 %
  // 8 --> 80 - 93 %
  int Centrality(const int cent) const;

  // Box matching cuts
  // |sdphi| < Nsigma && |sdz| < Nsigma
  // Default Nsigma = 2
  bool IsPC2BoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);
  bool IsPC3BoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);
  bool IsTOFBoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);
  bool IsEMCBoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);

  // Radial matching cuts
  // sqrt(sdphi^2 + sdz^2) < Nsigma
  // Default Nsigma = 2
  bool IsPC2RadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);
  bool IsPC3RadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);
  bool IsTOFRadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);
  bool IsEMCRadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma = 2.0);

  // Delta phi
  float GetPC3sdphi(const int iarm, const int ialpha, const int ized, const float pt, const float pc3dphi);
  float GetPC2sdphi(const int iarm, const int ialpha, const int ized, const float pt, const float pc2dphi);
  float GetTOFsdphi(const int iarm, const int ialpha, const int ized, const float pt, const float tofdphi);
  float GetEMCsdphi(const int iarm, const int ialpha, const int ized, const float pt, const float emcdphi);

  // Delta z
  float GetPC3sdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float pc3dz);
  float GetPC2sdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float pc2dz);
  float GetTOFsdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float tofdz);
  float GetEMCsdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float emcdz);

  float GetExp(const double* par, const float pt) const;
  float GetSigma(const double* par, const float pt) const;
  float GetSigmaParameter(const double* par, const double cent) const;
  float GetDphiDz(const float dphidz, const float mean, const float sigma) const;

  // Run4 62.4 GeV
  int run_Burn_pc2_match;
  int Burn_pc2_match();
  int run_Burn_pc3_match;
  int Burn_pc3_match();
  int run_Burn_tof_match;
  int Burn_tof_match();

 protected:

  //______________________________________________________________________________
  // Matching parameters
  // NARM : 0=EAST,     1=WEST
  // NCH  : 0=alpha<0,  1=alpha>0
  // NZED : 
  //   0=  -75 < zed < -40 cm
  //   1=  -40 < zed <   0 cm
  //   2=    0 < zed <  40 cm
  //   3=   40 < zed <  75 cm
  //______________________________________________________________________________
  //

  // Delta phi
  // mean
  double PC2_dphi_mean[NCH][NZED][3];
  double PC3_dphi_mean[NARM][NCH][NZED][3];
  double TOF_dphi_mean[NCH][NZED][3];
  double EMC_dphi_mean[NARM][NCH][NZED][3];

  // sigma
  double PC2_dphi_sigma[3];
  double PC3_dphi_sigma[NARM][3];
  double TOF_dphi_sigma[3];
  double EMC_dphi_sigma[NARM][3];

  // Delta z
  // mean
  double PC2_dz_mean_lowpt[NCH][NZED][2];
  double PC3_dz_mean_lowpt[NARM][NCH][NZED][2];
  double TOF_dz_mean_lowpt[NCH][NZED][2];
  double EMC_dz_mean_lowpt[NARM][NCH][NZED][2];

  double PC2_dz_mean_highpt[NCH][NZED][2];
  double PC3_dz_mean_highpt[NARM][NCH][NZED][2];
  double TOF_dz_mean_highpt[NCH][NZED][2];
  double EMC_dz_mean_highpt[NARM][NCH][NZED][2];

  double PC2_dz_mean_offset_lowpt[NCH][NZED][NCENT];
  double PC3_dz_mean_offset_lowpt[NARM][NCH][NZED][NCENT];
  double TOF_dz_mean_offset_lowpt[NCH][NZED][NCENT];
  double EMC_dz_mean_offset_lowpt[NARM][NCH][NZED][NCENT];

  double PC2_dz_mean_offset_highpt[NCH][NZED][NCENT];
  double PC3_dz_mean_offset_highpt[NARM][NCH][NZED][NCENT];
  double TOF_dz_mean_offset_highpt[NCH][NZED][NCENT];
  double EMC_dz_mean_offset_highpt[NARM][NCH][NZED][NCENT];

  // sigma
  double PC2_dz_sigma_slope[NCH];
  double PC3_dz_sigma_slope[NARM][NCH];
  double TOF_dz_sigma_slope[NCH];
  double EMC_dz_sigma_slope[NARM][NCH][2];

  double PC2_dz_sigma_offset[5];
  double PC3_dz_sigma_offset[NARM][5];
  double TOF_dz_sigma_offset[5];
  double EMC_dz_sigma_offset[NARM][2][5];

  // pT dependent scale factor for sigma
  double PC2_dphi_sigma_scale[NZED][3];
  double PC3_dphi_sigma_scale[NARM][NZED][3];
  double TOF_dphi_sigma_scale[NZED][3];
  double EMC_dphi_sigma_scale[NARM][NZED][3];
  double PC2_dz_sigma_scale[NZED][3];
  double PC3_dz_sigma_scale[NARM][NZED][3];
  double TOF_dz_sigma_scale[NZED][3];
  double EMC_dz_sigma_scale[NARM][NZED][3];

};

#endif /* __MATCHRECALRECO_H__ */
