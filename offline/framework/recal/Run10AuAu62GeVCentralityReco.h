#ifndef __RUN10AUAU62GEVCENTRALITYRECO_H__
#define __RUN10AUAU62GEVCENTRALITYRECO_H__

/* 
 * Run10 Au+Au 62GeV centrality calibrator:
 * adapted from Run7AuAu200GeVCentralityReco
 *
 * 3 run groups are used based on run-by-run
 * bbc mean charge distribution.
 *
 * x.gong, 11/24/2010, (xgong@rcf.rhic.bnl.gov)
 */

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Run10AuAu62GeVCentralityReco : public Recalibrator
{
 public:

  Run10AuAu62GeVCentralityReco();
  virtual ~Run10AuAu62GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun10AuAu62(PHCompositeNode *topNode);
  int  getCentralityByBBCRun10AuAu62(float bbc1, float bbc2, float zvertex, int runno);
  void help();
  float GetScaleFactor(int runnumber);
  int   GetQAStatus(int runnumber);
  void InitScaleFactor();

  void InitArray1();

 private:

  float bbcCutZDep[87][12][3];  // centrality selections in 86 centrality bins (requiring 87 cuts) and 12 zvertex bins
                                // and 3 run groups

  float BBCScaleFactor[3001]; // run-by-run correction scale factors
  int   QAstatus[3001];       // include list of runs that passed current QA 
                              // use enum here to categorize pass,fail,nocheck
  enum  QAstatus_types {QAstatus_PASS=0, QAstatus_FAIL=1, QAstatus_NOCHECK=2};

};

#endif /* __RUN10AUAU62GEVCENTRALITYRECO_H__ */
