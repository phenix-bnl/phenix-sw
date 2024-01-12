#ifndef __RUN4REACTIONPLANERECALRECO_H__
#define __RUN4REACTIONPLANERECALRECO_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;
class RpSumXY;
class ReactionPlaneCalib;

class Run4ReactionPlaneRecalReco : public Recalibrator
{
 public:
  Run4ReactionPlaneRecalReco();
  virtual ~Run4ReactionPlaneRecalReco();

  //  Standard methods
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode* topNode);

  int isValidRun(const int runno) const;

 private:

  int SetInvalid();

  PHGlobal* d_global;
  ReactionPlaneCalib *rpcalib;
  RpSumXY* rpsumxy;

  int runNumber;
  int calibration_ok;

  bool is62GeV;

};

#endif /* __RUN4REACTIONPLANERECALRECO_H__ */
