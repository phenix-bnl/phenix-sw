#ifndef RUN14HE3AU200GEVCENTRALITYRECO_H__
#define RUN14HE3AU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run14He3Au200GeVCentralityReco : public Recalibrator
{
 public:

  Run14He3Au200GeVCentralityReco();
  virtual ~Run14He3Au200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun14He3Au(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun14He3Au(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[89][60];  // centrality selections in 88 centrality bins

  float BBCScaleFactor[1232]; // run-by-run correction scale factors
  int   QAstatus[1232];       // include list of runs that passed current QA 
};

#endif /* RUN14HE3AU200GEVCENTRALITYRECO_H__ */
