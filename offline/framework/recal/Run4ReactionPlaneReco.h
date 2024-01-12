#ifndef __RUN4REACTIONPLANERECO_H__
#define __RUN4REACTIONPLANERECO_H__

#include <Recalibrator.h>

class PHCompositeNode;
class ReactionPlaneObject;
class ReactionPlaneCalib;

class Run4ReactionPlaneReco : public Recalibrator
{
 public:
  Run4ReactionPlaneReco(const int version=1);
  virtual ~Run4ReactionPlaneReco();

  int InitRun(PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);

  int isValidRun(const int runno) const;

  bool CreateNodeTree(PHCompositeNode* topNode);

  void Verbosity(const int v);

 private:

  ReactionPlaneObject* d_rp;
  ReactionPlaneCalib* rpcalib;
  int calibration_ok;
  int version;

};

#endif
