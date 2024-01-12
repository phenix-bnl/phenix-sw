#ifndef __TOFRECALRECO_H__
#define __TOFRECALRECO_H__

#include <string>
#include "Recalibrator.h"
#include "Tof.hh"

class RunHeader;
class TofOut;
class dTofReconstructed;
class PHCentralTrack;
class PHCompositeNode;
class TofCalibObject;

class TofrecalReco : public Recalibrator
{
 public:
  TofrecalReco(const char *name="TofrecalReco");
  virtual ~TofrecalReco();

  //  Standard methods
  int InitRun(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void Print(const std::string& = "ALL") const;
  int isValidRun(const int runno) const;

  // calibration constans from file
  void fetchDeltaTFromFile(const char * filename);  // constants from file
  void fetchElossConvFromFile(const char* filename);
  void fetchMipPeakFromFile(const char* filename);
  void fetchSlewingFromFile(const char* filename);
  void fetchYoffsetFromFile(const char* filename);
  void fetchVelocityFromFile(const char* filename);

  // DB access fetch
  void fetchDeltaT(const int runnumber);            // constants from database
  void fetchElossConv(const int runnumber);
  void fetchMipPeak(const int runnumber);
  void fetchSlewing(const int runnumber);
  void fetchYoffset(const int runnumber);
  void fetchVelocity(const int runnumber);

  // update
  void updateDeltaT(const int beginrun, const int endrun=-1);
  void updateElossConv(const int beginrun, const int endrun=-1);
  void updateMipPeak(const int beginrun, const int endrun=-1);
  void updateSlewing(const int beginrun, const int endrun=-1);
  void updateYoffset(const int beginrun, const int endrun=-1);
  void updateVelocity(const int beginrun, const int endrun=-1);

  // set/get
  void setDeltaT(const int islat, const float val) {DeltaT[islat] = val;}
  void setElossConv(const int islat, const float val);
  void setMipPeak(const int islat, const int ipmt, const float val);
  void setSlewing(const int islat, const int ipmt, const float val_a, const float val_b);
  void setYoffset(const int islat, const float val);
  void setVelocity(const int islat, const float val);

  float getDeltaT(const int islat) const {return DeltaT[islat];}
  float getElossConv(const int islat) const;
  float getMipPeak(const int islat, const int ipmt) const;
  float getSlewPar_a(const int islat, const int ipmt) const;
  float getSlewPar_b(const int islat, const int ipmt) const;
  float getYoffset(const int islat) const;
  float getVelocity(const int islat) const;


 protected:
  int runNumber;
  int recalcnt;
  int recaltof;
  float DeltaT[TOF_NSLAT];

  //  Nodes we need while running...
  RunHeader           *d_runhdr;
  TofCalibObject* tofcalib;
};

#endif /* __TOFRECALRECO_H__ */
