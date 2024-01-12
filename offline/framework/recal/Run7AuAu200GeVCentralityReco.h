#ifndef __RUN7AUAU200GEVCENTRALITYRECO_H__
#define __RUN7AUAU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Run7AuAu200GeVCentralityReco : public Recalibrator
{
 public:

  Run7AuAu200GeVCentralityReco();
  virtual ~Run7AuAu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun7AuAu(PHCompositeNode *topNode);
  int  getCentralityByBBCRun7AuAu(float bbc1, float bbc2, float zdc1, float zdc2, float zvertex, int runno);
  void help();
  float GetScaleFactor(int runnumber);
  int   GetQAStatus(int runnumber);
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[94][12];  // centrality selections in 93 centrality bins (requiring 94 cuts) and 12 zvertex bins

  float BBCScaleFactor[15000]; // run-by-run correction scale factors
  int   QAstatus[15000];       // include list of runs that passed current QA 
                               // use enum here to categorize pass,fail,nocheck
  enum  QAstatus_types {QAstatus_PASS=0, QAstatus_FAIL=1, QAstatus_NOCHECK=2};

};

#endif /* __RUN7AUAU200GEVCENTRALITYRECO_H__ */
