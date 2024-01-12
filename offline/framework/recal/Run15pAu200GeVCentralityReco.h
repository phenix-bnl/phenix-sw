#ifndef RUN15PAU200GEVCENTRALITYRECO_H__
#define RUN15PAU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run15pAu200GeVCentralityReco : public Recalibrator
{
 public:

  Run15pAu200GeVCentralityReco();
  virtual ~Run15pAu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun15pAu(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun15pAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[85][60];  // centrality selections in 84 for p+Au @ 200 GeV centrality bins

  float BBCScaleFactor[5000]; // run-by-run correction scale factors
  int   QAstatus[5000];       // include list of runs that passed current QA 
};

#endif /* RUN15PAU200GEVCENTRALITYRECO_H__ */
