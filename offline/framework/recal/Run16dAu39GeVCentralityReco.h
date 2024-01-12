#ifndef RUN16DAU39GEVCENTRALITYRECO_H__
#define RUN16DAU39GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run16dAu39GeVCentralityReco : public Recalibrator
{
 public:

  Run16dAu39GeVCentralityReco();
  virtual ~Run16dAu39GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun16dAu(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun16dAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[75][20];   // centrality selections in 74 (or 75 edges) for d+Au @ 39 GeV centrality bins

  float BBCScaleFactor[5000]; // run-by-run correction scale factors
  int   QAstatus[5000];       // include list of runs that passed current QA 

};

#endif /* RUN16DAU39GEVCENTRALITYRECO_H__ */
