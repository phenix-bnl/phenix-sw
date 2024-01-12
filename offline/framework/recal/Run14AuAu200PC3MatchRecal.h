#ifndef __RUN14AUAU200PC3MATCHRECAL_H__
#define __RUN14AUAU200PC3MATCHRECAL_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class TF1;
class TGraphErrors;

class Run14AuAu200PC3MatchRecal : public Recalibrator
{
 public:
  Run14AuAu200PC3MatchRecal(const char* name="Run14AuAu200PC3MatchRecal");
  virtual ~Run14AuAu200PC3MatchRecal();

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  void help();

 private:
  void  InitParameters();
  void  readParameters();

  void  initParMatchFunc();
  void  initParMatchGraph();

  double Calc(double raw, double pt, int icharge, int iarm, int ized, int DPHI_DZ);
  double CalcMeanDZ( double pt, int icharge, int iarm, int ized);
  double CalcSigmaDZ(double pt, int icharge, int iarm, int ized);
  double CalcMeanDPHI( double pt, int icharge, int iarm, int ized);
  double CalcSigmaDPHI(double pt, int icharge, int iarm, int ized);
  
  bool IsChargeRangeOK(unsigned int icharge){ return (icharge<2); }
  bool IsArmRangeOK(   unsigned int iarm)   { return (iarm<2); }
  bool IsZedRangeOK(   unsigned int ized)   { return (ized<10); }

 private:
  int             m_runNumber;
  PHCentralTrack *m_cnt;
  PHGlobal       *m_global;

  int             Calibrate();
  
  
  //Parameters
  TF1*          m_fn_dphi_mean[2][2][10];   //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TF1*          m_fn_dphi_sigma[2][2][10];  //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TF1*          m_fn_dz_mean[2][2][10];     //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TF1*          m_fn_dz_sigma[2][2][10];    //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TGraphErrors* m_g_dphi_mean[2][2][10];   //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TGraphErrors* m_g_dphi_sigma[2][2][10];  //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TGraphErrors* m_g_dz_mean[2][2][10];     //[NCHARGE][NARMS][NZED], number of charge, arms, zed
  TGraphErrors* m_g_dz_sigma[2][2][10];    //[NCHARGE][NARMS][NZED], number of charge, arms, zed

};

#endif /* __RUN14AUAU200PC3MATCHRECAL_H__ */
