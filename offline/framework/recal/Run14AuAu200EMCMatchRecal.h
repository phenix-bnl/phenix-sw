#ifndef __RUN14AUAU200EMCMATCHRECAL_H__
#define __RUN14AUAU200EMCMATCHRECAL_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class TF1;
class TGraphErrors;

class Run14AuAu200EMCMatchRecal : public Recalibrator
{
 public:
  Run14AuAu200EMCMatchRecal(const char*name="Run14AuAu200EMCMatchRecal");
  virtual ~Run14AuAu200EMCMatchRecal();

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

 private:
  void  InitParameters();
  void  readParameters();

  void  initParMatchFunc();
  void  initParMatchGraph();

  double Calc(double raw, double pt, int icharge, int iarm, int ized, int DPHI_DZ);
  double CalcMeanDPHI( double pt, int icharge, int iarm, int ized);
  double CalcSigmaDPHI(double pt, int icharge, int iarm, int ized);
  double CalcMeanDZ( double pt, int icharge, int iarm, int ized);
  double CalcSigmaDZ(double pt, int icharge, int iarm, int ized);

  double correctEmcdzByZed(float emcdz, int icharge, int iarm, 
                           short emcsect, float ecore, float zed);
  
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

#endif /* __RUN14AUAU200EMCMATCHRECALSGAUSS_H__ */
