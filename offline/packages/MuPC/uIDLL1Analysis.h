#ifndef __UIDLL1ANALYSIS_H__
#define __UIDLL1ANALYSIS_H__

#include "MuPCPar.h"
#include "TMuiChannelId.hh"
#include "hash_vector.hh"
#include "PHIODataNode.h"

class PHCompositeNode;
class uIDLL1Road ;
class TMuiHitMapO;

class uIDLL1Analysis
{
 public:
  uIDLL1Analysis();
  virtual ~uIDLL1Analysis() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int Event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);

 protected:

  int arm_deep[TwoPacksPerGap];
  int orient_deep[TwoPacksPerGap];
  int symsetID_deep[TwoPacksPerGap];
  int nRoad_deep;

  struct muiSymset
  {
      float cos;  // Slope of 1D projection
      int depth;  // Tracked to gap 1-4
      int arm;
      int orient;
      int symsetID;
  };
  muiSymset uRoad_deep[TwoPacksPerGap];

  float idxXoZ[NARMS][NOrent][TwoPacksPerGap]; //..symset slope
  bool signal[NARMS][NOrent][TwoPacksPerGap][GapPerArm];
  bool* triggerSignal[NARMS][NOrent][TwoPacksPerGap][LogicalsPerSymset];

  void calculate(TMuiHitMapO* _hit_map);
  void clear();

  int GetNodes(PHCompositeNode *topNode);
  void SymsetAlgorithm(bool **p, float slope, int arm, int orient, int symsetID);
  void fillRoad();   

  uIDLL1Road* uidll1road;
};

#endif /* __UIDLL1ANALYSIS_H__ */
