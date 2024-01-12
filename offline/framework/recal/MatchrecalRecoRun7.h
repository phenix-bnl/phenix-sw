#ifndef __MatchrecalRecoRun7_H__
#define __MatchrecalRecoRun7_H__

#include <string>
#include <Recalibrator.h>

class PHCompositeNode;

class MatchrecalRecoRun7: public Recalibrator
{
 public:

  MatchrecalRecoRun7(const std::string &name="MatchrecalRecoRun7");

  virtual ~MatchrecalRecoRun7() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

 private:

  int runNumber;

  void InitPars();//to initialize the parameters;
  void InitGroup1();
  void InitGroup2();
  
  enum {
    NDET = 8,
    NCH = 2,
    NZED = 16,
    NZED2 = 4,
  };


  //put tuning function here
  float  tofsdzE(float tofsdz, float pt, float cent);
  float  tofsdzW(float tofsdz, float pt, float cent);
  float  pc3sdzE(float pc3sdz, float pt, float cent);
  float  pc3sdzW(float pc3sdz, float pt, float cent);

  // Ids
  // alpha id
  // 0 --> alpha<0
  // 1 --> alpha>0
  int Alpha(float alpha);
  
  // zed id
  //from -80 to 80, step: 10cm
  int Zed(float zed);

  float Getsigma(int idet, int id, int ized, float pt);
  float Getmean(int idet, int id, int ialpha, int ized, float pt);

  float GetsigmaBurn(int idet, int id, int ialpha, int ized, float pt);
  float GetmeanBurn(int idet, int id, int ialpha, int ized, float pt);

  bool goodStrip(int i);
  float CalTofwSigmaPhi(int igrp, int charge, float tofwdphi, float mom);
  float CalTofwSigmaZ(int igrp, int charge, float tofwdz, float mom);
  int Group(int i);

  double phi_sigma[8][2];
  double phi_mean[8][2][16][3];
  double z_sigma[8][4][2];
  double z_mean_lowpt[8][2][16][3];
  double z_mean_highpt[8][2][16][3];

  double phi_sigma_lowpt_burn[8][2][16][3];
  double phi_sigma_highpt_burn[8][2][16][3];
  double phi_mean_lowpt_burn[8][2][16][3];
  double phi_mean_highpt_burn[8][2][16][3];
  double z_sigma_lowpt_burn[8][2][16][3];
  double z_sigma_highpt_burn[8][2][16][3];
  double z_mean_lowpt_burn[8][2][16][3];
  double z_mean_highpt_burn[8][2][16][3];

  float tfw_dphimeanpos[512];
  float tfw_dphimeanneg[512];
  float tfw_dzmeanpos[512];
  float tfw_dzmeanneg[512];

  float tfw_dphi_pos_sigma_par[12][4];
  float tfw_dphi_neg_sigma_par[12][4];
  
  float tfw_dz_pos_sigma_par[12][4];
  float tfw_dz_neg_sigma_par[12][4];
  
  float tfw_dphi_pos_mean_par[12][4];
  float tfw_dphi_neg_mean_par[12][4];
  
  float tfw_dz_pos_mean_par[12][4];
  float tfw_dz_neg_mean_par[12][4];
};

#endif /* __MatchrecalRecoRun7_H__ */
