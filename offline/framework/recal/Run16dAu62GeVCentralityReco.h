#ifndef RUN16DAU62GEVCENTRALITYRECO_H__
#define RUN16DAU62GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run16dAu62GeVCentralityReco : public Recalibrator
{
 public:

  Run16dAu62GeVCentralityReco();
  virtual ~Run16dAu62GeVCentralityReco() {}

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

  float bbcCutZDep[79][20];   // centrality selections in 78 for d+Au @ 62 GeV centrality bins

  float BBCScaleFactor[456283-455792+1]; // run-by-run correction scale factors
  int   QAstatus[456283-455792+1];       // include list of runs that passed current QA 

};

#endif /* RUN16DAU62GEVCENTRALITYRECO_H__ */
