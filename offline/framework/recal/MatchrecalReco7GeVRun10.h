#ifndef MATCHRECALRECO7GEVRUN10_H
#define MATCHRECALRECO7GEVRUN10_H

#include "Recalibrator.h"
#include <string>

class MatchrecalReco7GeVRun10: public Recalibrator
{
 public:
  
  MatchrecalReco7GeVRun10(const std::string &name="MatchrecalReco7GeVRun10");
  virtual ~MatchrecalReco7GeVRun10() {}
  
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;


 private:

  int InitPars();
  int Zed(const float fzed) const;

  float GetSval(const int itype,const int idet,const int isub,const int ized,const int ich,const float fpt,const float fdval) const;      
  
  enum{NZBN=10, NARM=2, NEMC=3, NCH=2};

  //! Calibration params
  //! PC2
  float p0_pc2_m_dp[NZBN][NCH], p1_pc2_m_dp[NZBN][NCH], p2_pc2_m_dp[NZBN][NCH];
  float p0_pc2_s_dp[NZBN][NCH], p1_pc2_s_dp[NZBN][NCH];
  float p0_pc2_m_dz[NZBN][NCH], p1_pc2_m_dz[NZBN][NCH], p2_pc2_m_dz[NZBN][NCH];
  float p0_pc2_s_dz[NZBN][NCH], p1_pc2_s_dz[NZBN][NCH];

  //! PC3
  float p0_pc3_m_dp[NARM][NZBN][NCH], p1_pc3_m_dp[NARM][NZBN][NCH], p2_pc3_m_dp[NARM][NZBN][NCH];
  float p0_pc3_s_dp[NARM][NZBN][NCH], p1_pc3_s_dp[NARM][NZBN][NCH];
  float p0_pc3_m_dz[NARM][NZBN][NCH], p1_pc3_m_dz[NARM][NZBN][NCH], p2_pc3_m_dz[NARM][NZBN][NCH];
  float p0_pc3_s_dz[NARM][NZBN][NCH], p1_pc3_s_dz[NARM][NZBN][NCH];

  //! EMC
  float p0_emc_m_dp[NEMC][NZBN][NCH], p1_emc_m_dp[NEMC][NZBN][NCH], p2_emc_m_dp[NEMC][NZBN][NCH];
  float p0_emc_s_dp[NEMC][NZBN][NCH], p1_emc_s_dp[NEMC][NZBN][NCH];
  float p0_emc_m_dz[NEMC][NZBN][NCH], p1_emc_m_dz[NEMC][NZBN][NCH], p2_emc_m_dz[NEMC][NZBN][NCH];
  float p0_emc_s_dz[NEMC][NZBN][NCH], p1_emc_s_dz[NEMC][NZBN][NCH];
  
/*   //! ToF */
/*   float p0_tof_m_dp[NARM][NZBN][NCH], p1_tof_m_dp[NARM][NZBN][NCH], p2_tof_m_dp[NARM][NZBN][NCH]; */
/*   float p0_tof_s_dp[NARM][NZBN][NCH]   , p1_tof_s_dp[NARM][NZBN][NCH]; */
/*   float p0_tof_m_dz[NARM][NZBN][NCH], p1_tof_m_dz[NARM][NZBN][NCH], p2_tof_m_dz[NARM][NZBN][NCH]; */
/*   float p0_tof_s_dz[NARM][NZBN][NCH]   , p1_tof_s_dz[NARM][NZBN][NCH] ; */

};
#endif /*MATCHRECALRECO7GEVRUN10_H*/
