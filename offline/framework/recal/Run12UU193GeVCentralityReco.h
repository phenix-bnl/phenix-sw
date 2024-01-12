#ifndef RUN12UU193GEVCENTRALITYRECO_H__
#define RUN12UU193GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run12UU193GeVCentralityReco : public Recalibrator
{
 public:

  Run12UU193GeVCentralityReco();
  virtual ~Run12UU193GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun12UU(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun12UU(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(int runnumber) const;
  int   GetQAStatus(int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[94][60];  // centrality selections in 93 centrality bins (requiring 94 cuts) and 60 zvertex bins

  float BBCScaleFactor[2700]; // run-by-run correction scale factors
  int   QAstatus[2700];       // include list of runs that passed current QA 
                               // use enum here to categorize pass,fail,nocheck
};

#endif /* RUN12UU193GEVCENTRALITYRECO_H__ */
