#ifndef __REACTIONPLANERECO_H__
#define __REACTIONPLANERECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class ReactionPlaneObject;
class ReactionPlaneCalib;

class ReactionPlaneReco : public Recalibrator
{
 public:
  ReactionPlaneReco();
  virtual ~ReactionPlaneReco();

  int InitRun(PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);

  int isValidRun(const int runno) const;

  bool CreateNodeTree(PHCompositeNode* topNode, const int runno);

  void Verbosity(const int v);

 private:

  ReactionPlaneObject* d_rp;
  ReactionPlaneCalib* rpcalib;
  int calibration_ok;
  int runNumber;
};

#endif
