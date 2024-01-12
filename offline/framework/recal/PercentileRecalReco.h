#ifndef __PERCENTILERECALRECO_H__
#define __PERCENTILERECALRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class PercentileRecalReco : public Recalibrator
{
 public:

  PercentileRecalReco();
  virtual ~PercentileRecalReco(); 

  int   Init(PHCompositeNode *topNode);
  int   InitRun(PHCompositeNode *topNode);
  int   isValidRun(const int runno) const;
  int   process_event(PHCompositeNode *topNode);
  void  InitArray();
  float getCentralityByBBC(PHCompositeNode *topNode);

 private:

  // Lookup Table (BBC Charge Sum Cut Values) 
  // for Run4 Au+Au 62 GeV Centrality
  float bbcCutZDep[85][16];

};

#endif /* __PERCENTILERECALRECO_H__ */
