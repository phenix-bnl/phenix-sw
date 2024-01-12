#ifndef __RUN12PP200PC3MATCHRECAL_H__
#define __RUN12PP200PC3MATCHRECAL_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHCentralTrack;
class PHGlobal;

class Run12pp200PC3MatchRecal : public Recalibrator
{
 public:
  Run12pp200PC3MatchRecal(const char*name="Run12pp200PC3MatchRecal");
  virtual ~Run12pp200PC3MatchRecal() {}

  int process_event(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
	int Init(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  int runNumber;

  PHCentralTrack *d_cnt;
  PHGlobal *d_global;

	 int Calibrate_Run12pp200();
	 
	 int GetPtBin(float pT);
	 float GetPtBinCenter(int pt_bin);

	 float CalcMean(float pt, int icharge, int iarm, int ized, int icent, int STAGE, int DPHI_DZ);
	 float CalcSigma(float pt, int icharge, int iarm, int ized, int icent, int STAGE, int DPHI_DZ);
	 float Calc(float pt, int icharge, int iarm, int ized, int icent, float dphi_dz, int STAGE, int DPHI_DZ);
	 
	 void InitializeFunctionFitParameters();
	
	 //Parameters
		float mean_points[2][2][10][1][21][3][2]; //[NCHARGE][NARMS][NZED][NCENT][NPT][NSTAGES][DPHI or DZ], number of charge, arms, zed, centrality, and pt bins
		float sigma_points[2][2][10][1][21][3][2];

 protected:

};

#endif /* __RUN12PP200PC3MATCHRECAL_H__ */
