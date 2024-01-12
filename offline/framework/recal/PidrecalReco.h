#ifndef __PIDRECALRECO_H__
#define __PIDRECALRECO_H__

#include <Recalibrator.h>
#include <PidrecalRecoPar.h>

class PHCentralTrack;
class PHCompositeNode;

class PidrecalReco : public Recalibrator
{
 public:
  PidrecalReco(const char *name = "PidrecalReco");
  virtual ~PidrecalReco() {}

  //  Standard methods
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void Print(const std::string &what = "ALL") const;
  int isValidRun(const int runno) const;

  // get constants from database
  void fetchPidParPlus(const int runnumber);
  void fetchPidParMinus(const int runnumber);
  void fetchMeanPp(const int runnumber);
  void fetchMeanPm(const int runnumber);
  void fetchMeanKp(const int runnumber);
  void fetchMeanKm(const int runnumber);
  void fetchMeanPr(const int runnumber);
  void fetchMeanPb(const int runnumber);
  
  // get constants from file
  void fetchPidParPlusFromFile(const char * filename);
  void fetchPidParMinusFromFile(const char * filename);
  void fetchMeanPpFromFile(const char * filename);
  void fetchMeanPmFromFile(const char * filename);
  void fetchMeanKpFromFile(const char * filename);
  void fetchMeanKmFromFile(const char * filename);
  void fetchMeanPrFromFile(const char * filename);
  void fetchMeanPbFromFile(const char * filename);

  // store constants in database
  void updatePidParPlus(const int beginrun, const int endrun=-1);
  void updatePidParMinus(const int beginrun, const int endrun=-1);
  void updateMeanPp(const int beginrun, const int endrun=-1);
  void updateMeanPm(const int beginrun, const int endrun=-1);
  void updateMeanKp(const int beginrun, const int endrun=-1);
  void updateMeanKm(const int beginrun, const int endrun=-1);
  void updateMeanPr(const int beginrun, const int endrun=-1);
  void updateMeanPb(const int beginrun, const int endrun=-1);

 protected:
  int calibration_ok;
  float PidPar_plus[NPAR_PID];
  float PidPar_minus[NPAR_PID];
  float MeanParPp[NPAR_MEAN_PION];
  float MeanParPm[NPAR_MEAN_PION];
  float MeanParKp[NPAR_MEAN_KAON];
  float MeanParKm[NPAR_MEAN_KAON];
  float MeanParPr[NPAR_MEAN_PROTON];
  float MeanParPb[NPAR_MEAN_PROTON];

  void applyDelta();

  const char *dataName_ParPlus; 
  const char *dataName_ParMinus;
  const char *dataName_MeanPp;
  const char *dataName_MeanPm;
  const char *dataName_MeanKp;
  const char *dataName_MeanKm;
  const char *dataName_MeanPr;
  const char *dataName_MeanPb;

  //  Nodes we need while running...
  PHCentralTrack      *d_cnt;

  float IsPion(const float m2tof, const float mom, const short charge);
  float IsKaon(const float m2tof, const float mom, const short charge);
  float IsProton(const float m2tof, const float mom, const short charge);
  float sigmaM2_analytical(const float m2tof, const float mom, const short charge);

};

#endif /* __PIDRECALRECO_H__ */
