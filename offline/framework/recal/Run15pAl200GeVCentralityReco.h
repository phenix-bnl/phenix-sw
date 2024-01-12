#ifndef RUN15PAL200GEVCENTRALITYRECO_H__
#define RUN15PAL200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run15pAl200GeVCentralityReco : public Recalibrator
{
 public:

  Run15pAl200GeVCentralityReco();
  virtual ~Run15pAl200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun15pAl(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun15pAl(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[73][60];  // centrality selections in 72+1 for p+Al @ 200 GeV centrality bins

  float BBCScaleFactor[5000]; // run-by-run correction scale factors
  int   QAstatus[5000];       // include list of runs that passed current QA 
};

#endif /* RUN15PAL200GEVCENTRALITYRECO_H__ */
