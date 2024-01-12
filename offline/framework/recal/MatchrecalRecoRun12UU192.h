#ifndef MATCHRECALRECORUN12UU192_H
#define MATCHRECALRECORUN12UU192_H

#include <Recalibrator.h>
#include "TF1.h"

class PHCompositeNode;
class PHCentralTrack;

class MatchrecalRecoRun12UU192 : public Recalibrator
{
 public:
  MatchrecalRecoRun12UU192(const std::string &name="MatchrecalRecoRun12UU192");
  virtual ~MatchrecalRecoRun12UU192() {}
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

protected:
  int get_ZED_bin(float ized);
  float get_tofsdphi(float ipt, int icharge, int ized, float itofdphi);
  float get_tofsdz(float ipt, int icharge, int ized, float itofdz, int icentrality);
  void initParameters();

  TF1 *fit_UU_z_sig, *fit_UU_phi_sig, *fit_UU_z_mean, *fit_UU_phi_mean;
  TF1 *fit_UU_sz_sig, *fit_UU_sphi_sig, *fit_UU_sz_mean, *fit_UU_sphi_mean;

  float zed_bin_UU[20];
  double par_UU_mean0[10][5], par_UU_sigma0[10][7], par_UU_mean1[10][5], par_UU_sigma1[10][7],
         par_UU_mean2[10][5], par_UU_sigma2[10][7], par_UU_mean3[10][5], par_UU_sigma3[10][7],
         par_UU_smean0[10][5], par_UU_ssigma0[10][7], par_UU_smean1[10][5], par_UU_ssigma1[10][7],
         par_UU_smean2[10][5], par_UU_ssigma2[10][7], par_UU_smean3[10][5], par_UU_ssigma3[10][7];
  double Keff_UU_central_sigma[5],Keff_UU_central_mean[5];
  static const int MINRUN = 369327;
  static const int MAXRUN = 371908;
};


#endif // MINIRECAL_H
