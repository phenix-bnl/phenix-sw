#ifndef RUN10AUAU007GEVCENTRALITYRECO_H__
#define RUN10AUAU007GEVCENTRALITYRECO_H__

/*
 * Run10 Au+Au 7.7 GeV centrality calibrator:
 *
 * J.T. Mitchell, 2/23/2011, (mitchell@bnl.gov)
 */

#include "Recalibrator.h"

class PHCompositeNode;

class Run10AuAu007GeVCentralityReco : public Recalibrator
{
 public:

  Run10AuAu007GeVCentralityReco();
  virtual ~Run10AuAu007GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByRxnpRun10AuAu007(PHCompositeNode *topNode);
  int  getCentralityByRxnpRun10AuAu007(const float rxnpoutq, const float zvertex, const int runno) const;
  void help();
  float GetScaleFactor(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactor();

  void InitArray1();

 protected:

  float rxnpCutZDep[76][12];  // centrality selections in 75 centrality bins (requiring 76 cuts) and 12 zvertex bins
  float RXNPScaleFactor[3500]; // run-by-run correction scale factors
  int QAstatus[3500];       // include list of runs that passed current QA
  // use enum here to categorize pass,fail,nocheck
};

#endif /* RUN10AUAU007GEVCENTRALITYRECO_H__ */
