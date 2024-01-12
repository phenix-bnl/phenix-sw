#ifndef RUN15PAUEMCMATCHING_H
#define RUN15PAUEMCMATCHING_H

#include <Recalibrator.h>

class PHCompositeNode;
class PHCentralTrack;

class Run15pAuEMCMatching : public Recalibrator
{
 public:
  Run15pAuEMCMatching(const std::string &name="Run15pAuEMCMatching");
  virtual ~Run15pAuEMCMatching() {}
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;
  void initParameters();
  void help() const;
  float GetEMCsdphi(int armsect, int charge, int ized,
		    float pt, float emcdphi);
  float CalculateInitialEmcsdPhi(int charge, int sector, int zed, float emcdphi, float pt);
  float CalculateFinalEmcsdPhi(int charge, int sector, int zed, float emcsdphi, float pt);


  float GetEMCsdz(int armsect, int charge, int ized,
		  float pt, float emcdz, float beta);
  float CalculateCorrectedEmcdZ(float beta, float pt, float emcdz, int sector);
  float CalculateInitialEmcsdZ(int charge, int sector, int zed, float emcdz, float pt);
  float CalculateFinalEmcsdZ(int charge, int sector, int zed, float emcsdz, float pt);
  

  int getDCZedBin(float zedDC);
  int GetPtBin(float);
  
  enum {NSECT=8, NPT=14, NCHARGE=2, NZED=10, NCENT=1};
 private:
  float mean0[NSECT][NPT];
  float mean1[NSECT][NPT];
  float mean2[NSECT][NPT];

  float sigma0[NSECT][NPT];
  float sigma1[NSECT][NPT];
  float sigma2[NSECT][NPT];

  float initialdPhiMeanParam_0[NCHARGE][NSECT][NZED];
  float initialdPhiMeanParam_1[NCHARGE][NSECT][NZED];
  float initialdPhiMeanParam_2[NCHARGE][NSECT][NZED];
  float initialdPhiMeanParam_3[NCHARGE][NSECT][NZED];
  float initialdPhiMeanParam_4[NCHARGE][NSECT][NZED];

  float initialdPhiSigmaParam_0[NCHARGE][NSECT][NZED];
  float initialdPhiSigmaParam_1[NCHARGE][NSECT][NZED];
  float initialdPhiSigmaParam_2[NCHARGE][NSECT][NZED];
  float initialdPhiSigmaParam_3[NCHARGE][NSECT][NZED];
  float initialdPhiSigmaParam_4[NCHARGE][NSECT][NZED];

  float finaldPhiMeanParam_0[NCHARGE][NSECT][NZED];
  float finaldPhiMeanParam_1[NCHARGE][NSECT][NZED];
  float finaldPhiMeanParam_2[NCHARGE][NSECT][NZED];
  float finaldPhiMeanParam_3[NCHARGE][NSECT][NZED];
	
  float finaldPhiSigmaParam_0[NCHARGE][NSECT][NZED];

  float initialdZMeanParam_0[NCHARGE][NSECT][NZED];

  float initialdZSigmaParam_0[NCHARGE][NSECT][NZED];
  float initialdZSigmaParam_1[NCHARGE][NSECT][NZED];
  float initialdZSigmaParam_2[NCHARGE][NSECT][NZED];
  float initialdZSigmaParam_3[NCHARGE][NSECT][NZED];
  float initialdZSigmaParam_4[NCHARGE][NSECT][NZED];

  float finaldZMeanParam_0[NCHARGE][NSECT][NZED];
	
  float finaldZSigmaParam_0[NCHARGE][NSECT][NZED];
  float finaldZSigmaParam_1[NCHARGE][NSECT][NZED];
  float finaldZSigmaParam_2[NCHARGE][NSECT][NZED];

};

#endif // RUN15PAUEMCMATCHING_H
