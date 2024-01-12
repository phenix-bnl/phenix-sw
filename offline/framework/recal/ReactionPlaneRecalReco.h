#ifndef __REACTIONPLANERECALRECO_H__
#define __REACTIONPLANERECALRECO_H__

#include <string>
#include "Recalibrator.h"

class PHCompositeNode;
class RpSumXYObject;
class ReactionPlaneCalib;

class ReactionPlaneRecalReco : public Recalibrator
{
 public:
  ReactionPlaneRecalReco();
  virtual ~ReactionPlaneRecalReco();

  //  Standard methods
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode* topNode);

  int isValidRun(const int runno) const;

 private:
  RpSumXYObject* d_rpsumxy;
  ReactionPlaneCalib* rpcalib;
  int SetInvalid();
  int runNumber;
  int calibration_ok;

};

#endif /* __REACTIONPLANERECALRECO_H__ */
