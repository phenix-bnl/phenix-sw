#ifndef __TOFWRECALRECO_H__
#define __TOFWRECALRECO_H__

#include <Recalibrator.h>
#include <TofwPar.h>
#include <string>

class PHCompositeNode;
class TofwCalib;

class TofwrecalReco : public Recalibrator
{
 public:
  TofwrecalReco(const std::string &name="TofwrecalReco");
  virtual ~TofwrecalReco();

  //  Standard methods
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int isValidRun(const int runno) const;

  // calibration constans from file
  void fetchrunFromFile(const char * filename);  // constants from file
  void fetchFromFile(const char* filename);

  // DB access fetch
  //void fetchrun();            //run-by-run constants from database
  //void fetch();// constant from database

  // set/get
  void SetDeltaT(const float val) {DeltaTRun = val;}
  void setDeltaT(const int istrip, const float val) {DeltaT[istrip] = val;}
  void setSlewing_A(const int istrip, const float val) {Slewing_A[istrip]=val;}
  void setSlewing_B(const int istrip, const float val) {Slewing_B[istrip]=val;}
  void setMean_Dz_Plus(const int istrip, const float val) {Mean_Dz_Plus[istrip]=val;}
  void setMean_Dz_Minus(const int istrip, const float val) {Mean_Dz_Minus[istrip]=val;}

  void setMean_Dphi_Plus(const int istrip, const float val) {Mean_Dphi_Plus[istrip]=val;}
  void setMean_Dphi_Minus(const int istrip, const float val) {Mean_Dphi_Minus[istrip]=val;}

  float getDeltaTRun() const {return DeltaTRun;}
  float getDeltaT(const int istrip) const {return DeltaT[istrip];}
  float getSlewing_A(const int istrip) const {return Slewing_A[istrip];}
  float getSlewing_B(const int istrip) const {return Slewing_B[istrip];}

  float getMean_Dz_Plus(const int istrip) const {return Mean_Dz_Plus[istrip];}
  float getMean_Dz_Minus(const int istrip) const {return Mean_Dz_Minus[istrip];}

  float getMean_Dphi_Plus(const int istrip) const {return Mean_Dphi_Plus[istrip];}
  float getMean_Dphi_Minus(const int istrip) const {return Mean_Dphi_Minus[istrip];}

  float CalSigmaZ(float mom);
  float CalSigmaPhi(float mom);
  float CalSlewing(int strip, float adc);

 protected:
  TofwCalib *tofwcalib;

  //TF1 *fun_dz;
  //TF1 *fun_dphi;
  //TF1 *fun_slewing;

  bool fetchstripoffflag;
  int nrun;
  int runNumber;
  float run_deltat[2000][2];
  float DeltaTRun;

  float sigma_dphi_par[4];
  float sigma_dz_par[4];

  float DeltaT[TOFW_NSTRIP_TOTAL];
  float Slewing_A[TOFW_NSTRIP_TOTAL];
  float Slewing_B[TOFW_NSTRIP_TOTAL];
  float Mean_Dz_Plus[TOFW_NSTRIP_TOTAL];
  float Mean_Dz_Minus[TOFW_NSTRIP_TOTAL];
  float Mean_Dphi_Plus[TOFW_NSTRIP_TOTAL];
  float Mean_Dphi_Minus[TOFW_NSTRIP_TOTAL];

};

#endif /* __TOFWRECALRECO_H__ */
