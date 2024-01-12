#ifndef __RUN8DAU200GEVCENTRALITYRECO_H__
#define __RUN8DAU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Run8dAu200GeVCentralityReco : public Recalibrator
{
 public:

  Run8dAu200GeVCentralityReco();
  virtual ~Run8dAu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun8dAu(PHCompositeNode *topNode);
  int  getCentralityByBBCRun8dAu(float bbc1, float bbc2, float zdc1, float zdc2, float zvertex, int runno);
  void help();
  float GetScaleFactor(int runnumber);
  int   GetQAStatus(int runnumber);
  void  InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[89][12];  // centrality selections in 12 zvertex bins (-30:+30 cm)
                             // note that one needs one extra entry (88+1) for centrality ranges 

  float BBCScaleFactor[8000]; // run-by-run correction scale factors
  int   QAstatus[8000];       // include list of runs that passed current QA 
                              // use enum here to categorize pass,fail,nocheck
  enum  QAstatus_types {QAstatus_PASS=0, QAstatus_FAIL=1, QAstatus_NOCHECK=2};

};

#endif /* __RUN5CUCU200GEVCENTRALITYRECO_H__ */
