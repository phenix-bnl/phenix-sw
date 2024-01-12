#ifndef MATCHRECALRECO39GEVRUN10_H
#define MATCHRECALRECO39GEVRUN10_H

#include <string>
#include <Recalibrator.h>

class MatchrecalReco39GeVRun10: public Recalibrator
{
 public:
  
  MatchrecalReco39GeVRun10(const std::string &name="MatchrecalReco39GeVRun10");
  virtual ~MatchrecalReco39GeVRun10();
  
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;


 private:

  int InitPars(void);
  int Zed(float fzed);

  float GetSval(int itype,int idet,int isub,int ized,int ich,float fpt,float fdval);      
  float GetFineTSval(int isub,int cent,float fpt,float sdz);
  
  enum{NZBN=10, NARM=2, NEMC=3, NCH=2};

  //! Calibration params
  //! PC2
  float p0_pc2_m_dp[2][NZBN][NCH], p1_pc2_m_dp[2][NZBN][NCH], p2_pc2_m_dp[2][NZBN][NCH], p3_pc2_m_dp[2][NZBN][NCH], p4_pc2_m_dp[2][NZBN][NCH];
  float p0_pc2_s_dp[NZBN][NCH]   , p1_pc2_s_dp[NZBN][NCH]   , p2_pc2_s_dp[NZBN][NCH]   , p3_pc2_s_dp[NZBN][NCH]   , p4_pc2_s_dp[NZBN][NCH];
  float p0_pc2_m_dz[2][NZBN][NCH], p1_pc2_m_dz[2][NZBN][NCH], p2_pc2_m_dz[2][NZBN][NCH], p3_pc2_m_dz[2][NZBN][NCH], p4_pc2_m_dz[2][NZBN][NCH];
  float p0_pc2_s_dz[NZBN][NCH]   , p1_pc2_s_dz[NZBN][NCH]   , p2_pc2_s_dz[NZBN][NCH]   , p3_pc2_s_dz[NZBN][NCH]   , p4_pc2_s_dz[NZBN][NCH];

  //! PC3
  float p0_pc3_m_dp[2][NARM][NZBN][NCH], p1_pc3_m_dp[2][NARM][NZBN][NCH], p2_pc3_m_dp[2][NARM][NZBN][NCH], p3_pc3_m_dp[2][NARM][NZBN][NCH], p4_pc3_m_dp[2][NARM][NZBN][NCH];
  float p0_pc3_s_dp[NARM][NZBN][NCH]   , p1_pc3_s_dp[NARM][NZBN][NCH]   , p2_pc3_s_dp[NARM][NZBN][NCH]   , p3_pc3_s_dp[NARM][NZBN][NCH]   , p4_pc3_s_dp[NARM][NZBN][NCH];
  float p0_pc3_m_dz[2][NARM][NZBN][NCH], p1_pc3_m_dz[2][NARM][NZBN][NCH], p2_pc3_m_dz[2][NARM][NZBN][NCH], p3_pc3_m_dz[2][NARM][NZBN][NCH], p4_pc3_m_dz[2][NARM][NZBN][NCH];
  float p0_pc3_s_dz[NARM][NZBN][NCH]   , p1_pc3_s_dz[NARM][NZBN][NCH]   , p2_pc3_s_dz[NARM][NZBN][NCH]   , p3_pc3_s_dz[NARM][NZBN][NCH]   , p4_pc3_s_dz[NARM][NZBN][NCH];
  
  //! EMC
  float p0_emc_m_dp[NEMC][NZBN][NCH], p1_emc_m_dp[NEMC][NZBN][NCH], p2_emc_m_dp[NEMC][NZBN][NCH], p3_emc_m_dp[NEMC][NZBN][NCH], p4_emc_m_dp[NEMC][NZBN][NCH];
  float p0_emc_s_dp[NEMC][NZBN][NCH], p1_emc_s_dp[NEMC][NZBN][NCH], p2_emc_s_dp[NEMC][NZBN][NCH], p3_emc_s_dp[NEMC][NZBN][NCH], p4_emc_s_dp[NEMC][NZBN][NCH];
  float p0_emc_m_dz[NEMC][NZBN][NCH], p1_emc_m_dz[NEMC][NZBN][NCH], p2_emc_m_dz[NEMC][NZBN][NCH], p3_emc_m_dz[NEMC][NZBN][NCH], p4_emc_m_dz[NEMC][NZBN][NCH];
  float p0_emc_s_dz[NEMC][NZBN][NCH], p1_emc_s_dz[NEMC][NZBN][NCH], p2_emc_s_dz[NEMC][NZBN][NCH], p3_emc_s_dz[NEMC][NZBN][NCH], p4_emc_s_dz[NEMC][NZBN][NCH];
  
  //! ToF
  float p0_tof_m_dp[2][NARM][NZBN][NCH], p1_tof_m_dp[2][NARM][NZBN][NCH], p2_tof_m_dp[2][NARM][NZBN][NCH], p3_tof_m_dp[2][NARM][NZBN][NCH], p4_tof_m_dp[2][NARM][NZBN][NCH];
  float p0_tof_s_dp[NARM][NZBN][NCH]   , p1_tof_s_dp[NARM][NZBN][NCH]   , p2_tof_s_dp[NARM][NZBN][NCH]   , p3_tof_s_dp[NARM][NZBN][NCH]   , p4_tof_s_dp[NARM][NZBN][NCH];
  float p0_tof_m_dz[2][NARM][NZBN][NCH], p1_tof_m_dz[2][NARM][NZBN][NCH], p2_tof_m_dz[2][NARM][NZBN][NCH], p3_tof_m_dz[2][NARM][NZBN][NCH], p4_tof_m_dz[2][NARM][NZBN][NCH];
  float p0_tof_s_dz[NARM][NZBN][NCH]   , p1_tof_s_dz[NARM][NZBN][NCH]   , p2_tof_s_dz[NARM][NZBN][NCH]   , p3_tof_s_dz[NARM][NZBN][NCH]   , p4_tof_s_dz[NARM][NZBN][NCH];

  //! Afterburn
  //! PC2
  float p0_pc2_m_sdp[2][NZBN][NCH], p1_pc2_m_sdp[2][NZBN][NCH], p2_pc2_m_sdp[2][NZBN][NCH], p3_pc2_m_sdp[2][NZBN][NCH], p4_pc2_m_sdp[2][NZBN][NCH];
  float p0_pc2_s_sdp[2][NZBN][NCH], p1_pc2_s_sdp[2][NZBN][NCH], p2_pc2_s_sdp[2][NZBN][NCH], p3_pc2_s_sdp[2][NZBN][NCH], p4_pc2_s_sdp[2][NZBN][NCH];
  float p0_pc2_m_sdz[2][NZBN][NCH], p1_pc2_m_sdz[2][NZBN][NCH], p2_pc2_m_sdz[2][NZBN][NCH], p3_pc2_m_sdz[2][NZBN][NCH], p4_pc2_m_sdz[2][NZBN][NCH];
  float p0_pc2_s_sdz[2][NZBN][NCH], p1_pc2_s_sdz[2][NZBN][NCH], p2_pc2_s_sdz[2][NZBN][NCH], p3_pc2_s_sdz[2][NZBN][NCH], p4_pc2_s_sdz[2][NZBN][NCH];
  
  //! PC3
  float p0_pc3_m_sdp[2][NARM][NZBN][NCH], p1_pc3_m_sdp[2][NARM][NZBN][NCH], p2_pc3_m_sdp[2][NARM][NZBN][NCH], p3_pc3_m_sdp[2][NARM][NZBN][NCH], p4_pc3_m_sdp[2][NARM][NZBN][NCH];
  float p0_pc3_s_sdp[2][NARM][NZBN][NCH], p1_pc3_s_sdp[2][NARM][NZBN][NCH], p2_pc3_s_sdp[2][NARM][NZBN][NCH], p3_pc3_s_sdp[2][NARM][NZBN][NCH], p4_pc3_s_sdp[2][NARM][NZBN][NCH];
  float p0_pc3_m_sdz[2][NARM][NZBN][NCH], p1_pc3_m_sdz[2][NARM][NZBN][NCH], p2_pc3_m_sdz[2][NARM][NZBN][NCH], p3_pc3_m_sdz[2][NARM][NZBN][NCH], p4_pc3_m_sdz[2][NARM][NZBN][NCH];
  float p0_pc3_s_sdz[2][NARM][NZBN][NCH], p1_pc3_s_sdz[2][NARM][NZBN][NCH], p2_pc3_s_sdz[2][NARM][NZBN][NCH], p3_pc3_s_sdz[2][NARM][NZBN][NCH], p4_pc3_s_sdz[2][NARM][NZBN][NCH];

  //! EMC
  float p0_emc_m_sdp[2][NEMC][NZBN][NCH], p1_emc_m_sdp[2][NEMC][NZBN][NCH], p2_emc_m_sdp[2][NEMC][NZBN][NCH],p3_emc_m_sdp[2][NEMC][NZBN][NCH],p4_emc_m_sdp[2][NEMC][NZBN][NCH];
  float p0_emc_s_sdp[2][NEMC][NZBN][NCH], p1_emc_s_sdp[2][NEMC][NZBN][NCH], p2_emc_s_sdp[2][NEMC][NZBN][NCH],p3_emc_s_sdp[2][NEMC][NZBN][NCH],p4_emc_s_sdp[2][NEMC][NZBN][NCH];
  float p0_emc_m_sdz[2][NEMC][NZBN][NCH], p1_emc_m_sdz[2][NEMC][NZBN][NCH], p2_emc_m_sdz[2][NEMC][NZBN][NCH],p3_emc_m_sdz[2][NEMC][NZBN][NCH],p4_emc_m_sdz[2][NEMC][NZBN][NCH];
  float p0_emc_s_sdz[2][NEMC][NZBN][NCH], p1_emc_s_sdz[2][NEMC][NZBN][NCH], p2_emc_s_sdz[2][NEMC][NZBN][NCH],p3_emc_s_sdz[2][NEMC][NZBN][NCH],p4_emc_s_sdz[2][NEMC][NZBN][NCH];

  //! ToF
  float p0_tof_m_sdp[2][NARM][NZBN][NCH], p1_tof_m_sdp[2][NARM][NZBN][NCH], p2_tof_m_sdp[2][NARM][NZBN][NCH], p3_tof_m_sdp[2][NARM][NZBN][NCH], p4_tof_m_sdp[2][NARM][NZBN][NCH];
  float p0_tof_s_sdp[2][NARM][NZBN][NCH], p1_tof_s_sdp[2][NARM][NZBN][NCH], p2_tof_s_sdp[2][NARM][NZBN][NCH], p3_tof_s_sdp[2][NARM][NZBN][NCH], p4_tof_s_sdp[2][NARM][NZBN][NCH];
  float p0_tof_m_sdz[2][NARM][NZBN][NCH], p1_tof_m_sdz[2][NARM][NZBN][NCH], p2_tof_m_sdz[2][NARM][NZBN][NCH], p3_tof_m_sdz[2][NARM][NZBN][NCH], p4_tof_m_sdz[2][NARM][NZBN][NCH];
  float p0_tof_s_sdz[2][NARM][NZBN][NCH], p1_tof_s_sdz[2][NARM][NZBN][NCH], p2_tof_s_sdz[2][NARM][NZBN][NCH], p3_tof_s_sdz[2][NARM][NZBN][NCH], p4_tof_s_sdz[2][NARM][NZBN][NCH];

  int RunNumber;
};
#endif /*MATCHRECALRECO39GEVRUN10_H*/
