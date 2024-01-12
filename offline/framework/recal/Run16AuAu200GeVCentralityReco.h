#ifndef RUN16AUAU200GEVCENTRALITYRECO_H__
#define RUN16AUAU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Run16AuAu200GeVCentralityReco : public Recalibrator
{
 public:

  Run16AuAu200GeVCentralityReco();
  virtual ~Run16AuAu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun16AuAu200(PHCompositeNode *topNode) const;
  int  getCentralityByBBCRun16AuAu200(const float bbc1, const float bbc2, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactorBBC(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactorBBC();
  void InitArrayBBC();

 private:

  int centSource;  // 0 = use BBC, only option. Set with CENTSOURCERUN16AUAU200
  float bbcCutZDep[94][60];  // centrality selections in 93 centrality bins and 60 zvertex bins
  float BBCScaleFactor[16000]; // run-by-run correction scale factors
  int   QAstatus[16000];       // include list of runs that passed current QA 
};

#endif /* RUN16AUAU200GEVCENTRALITYRECO_H__ */
