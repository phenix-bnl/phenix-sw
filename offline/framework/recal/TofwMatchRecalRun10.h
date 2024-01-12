#ifndef TOFWMATCHRECALRUN10_H
#define TOFWMATCHRECALRUN10_H

#include <Recalibrator.h>
#include <string>

class TofwMatchRecalRun10: public Recalibrator
{
 public:
  
  TofwMatchRecalRun10(const std::string &name="TofwMatchRecalRun10");
  virtual ~TofwMatchRecalRun10() {}
  
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;


 private:

  int InitPars(void);
  int Zed(const float fzed);

  float GetSval(const int itype,const int idet,const int isub,const int ized,const int ich,const float fpt,const float fdval);      

  enum{NZBN=10, NARM=2, NEMC=3, NCH=2};


  //! Calibration params
  //! ToF
  float p0_tof_m_dp[2][NARM][NZBN][NCH];
  float p1_tof_m_dp[2][NARM][NZBN][NCH];
  float p2_tof_m_dp[2][NARM][NZBN][NCH];
  float p3_tof_m_dp[2][NARM][NZBN][NCH];
  float p4_tof_m_dp[2][NARM][NZBN][NCH];
  float p0_tof_s_dp[NARM][NZBN][NCH];
  float p1_tof_s_dp[NARM][NZBN][NCH];
  float p2_tof_s_dp[NARM][NZBN][NCH];
  float p3_tof_s_dp[NARM][NZBN][NCH];
  float p4_tof_s_dp[NARM][NZBN][NCH];
  float p0_tof_m_dz[2][NARM][NZBN][NCH];
  float p1_tof_m_dz[2][NARM][NZBN][NCH];
  float p2_tof_m_dz[2][NARM][NZBN][NCH];
  float p3_tof_m_dz[2][NARM][NZBN][NCH];
  float p4_tof_m_dz[2][NARM][NZBN][NCH];
  float p0_tof_s_dz[NARM][NZBN][NCH];
  float p1_tof_s_dz[NARM][NZBN][NCH];
  float p2_tof_s_dz[NARM][NZBN][NCH];
  float p3_tof_s_dz[NARM][NZBN][NCH];
  float p4_tof_s_dz[NARM][NZBN][NCH];

  //! Afterburn
  //! ToF
  float p0_tof_m_sdp[2][NARM][NZBN][NCH];
  float p1_tof_m_sdp[2][NARM][NZBN][NCH];
  float p2_tof_m_sdp[2][NARM][NZBN][NCH];
  float p3_tof_m_sdp[2][NARM][NZBN][NCH];
  float p4_tof_m_sdp[2][NARM][NZBN][NCH];
  float p0_tof_s_sdp[2][NARM][NZBN][NCH];
  float p1_tof_s_sdp[2][NARM][NZBN][NCH];
  float p2_tof_s_sdp[2][NARM][NZBN][NCH];
  float p3_tof_s_sdp[2][NARM][NZBN][NCH];
  float p4_tof_s_sdp[2][NARM][NZBN][NCH];
  float p0_tof_m_sdz[2][NARM][NZBN][NCH];
  float p1_tof_m_sdz[2][NARM][NZBN][NCH];
  float p2_tof_m_sdz[2][NARM][NZBN][NCH];
  float p3_tof_m_sdz[2][NARM][NZBN][NCH];
  float p4_tof_m_sdz[2][NARM][NZBN][NCH];
  float p0_tof_s_sdz[2][NARM][NZBN][NCH];
  float p1_tof_s_sdz[2][NARM][NZBN][NCH];
  float p2_tof_s_sdz[2][NARM][NZBN][NCH];
  float p3_tof_s_sdz[2][NARM][NZBN][NCH];
  float p4_tof_s_sdz[2][NARM][NZBN][NCH];
};
#endif /*TOFWMATCHRECALRUN10_H*/
