#ifndef RUN11AUAU200GEVCENTRALITYRECO_H__
#define RUN11AUAU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run11AuAu200GeVCentralityReco : public Recalibrator
{
 public:

  Run11AuAu200GeVCentralityReco();
  virtual ~Run11AuAu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun11AuAu(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun11AuAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[94][12];  // centrality selections in 93 centrality bins (requiring 94 cuts) and 12 zvertex bins

  float BBCScaleFactor[6700]; // run-by-run correction scale factors
  int   QAstatus[6700];       // include list of runs that passed current QA 
                               // use enum here to categorize pass,fail,nocheck
};

#endif /* RUN11AUAU200GEVCENTRALITYRECO_H__ */
