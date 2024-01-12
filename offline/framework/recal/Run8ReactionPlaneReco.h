#ifndef __RUN8REACTIONPLANERECO_H__
#define __RUN8REACTIONPLANERECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class ReactionPlaneObject;
class RpSumXYObject;
class ReactionPlaneCalibv2;
class BbcCalib;
class BbcGeo;
class TH1D;

class Run8ReactionPlaneReco : public Recalibrator
{
 public:
  Run8ReactionPlaneReco();
  virtual ~Run8ReactionPlaneReco();

  int InitRun(PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);

  int isValidRun(const int runno) const;

  bool CreateNodeTree(PHCompositeNode* topNode, const int runno);

  bool BadTower(int ich);

  void Verbosity(const int v);

 private:

  ReactionPlaneObject* d_rp;
  RpSumXYObject* d_rpsumxy;
  ReactionPlaneCalibv2* rpcalib;
  BbcCalib* bbccalib;
  BbcGeo* bbcgeo;
  PHCompositeNode *dst_node;
  PHCompositeNode *rxnp_node;

  int calibration_ok;
  int runNumber;
  int ievent;

  TH1D *h_MPCrp00;
  TH1D *h_MPCrp10;

  TH1D *h_MPCrp01;
  TH1D *h_MPCrp11;

  TH1D *h_BBCrp00;
  TH1D *h_BBCrp10;

  TH1D *h_BBCrp01;
  TH1D *h_BBCrp11;

  TH1D *h_RXNrp00;
  TH1D *h_RXNrp10;

  TH1D *h_RXNrp01;
  TH1D *h_RXNrp11;

  TH1D *h_RXNrp02;
  TH1D *h_RXNrp12;

  TH1D *h_RXNrp03;
  TH1D *h_RXNrp13;

  TH1D *h_RXNrp04;
  TH1D *h_RXNrp14;

  TH1D *h_RXNrp05;
  TH1D *h_RXNrp15;

  TH1D *h_SMDrp00;

  TH1D *h_CNTrp14;

};

#endif
