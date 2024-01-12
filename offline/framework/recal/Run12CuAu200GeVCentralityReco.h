#ifndef RUN12CUAU200GEVCENTRALITYRECO_H__
#define RUN12CUAU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run12CuAu200GeVCentralityReco : public Recalibrator
{
 public:

  Run12CuAu200GeVCentralityReco();
  virtual ~Run12CuAu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun12CuAu(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun12CuAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[94][60];  // centrality selections in 93 centrality bins (requiring 94 cuts) and 60 zvertex bins

  float BBCScaleFactor[5000]; // run-by-run correction scale factors
  int   QAstatus[5000];       // include list of runs that passed current QA 
                               // use enum here to categorize pass,fail,nocheck

};

#endif /* RUN12CUAU200GEVCENTRALITYRECO_H__ */
