#ifndef __RUN5CUCU62GEVCENTRALITYRECO_H__
#define __RUN5CUCU62GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Run5CuCu62GeVCentralityReco : public Recalibrator
{
 public:

  Run5CuCu62GeVCentralityReco();
  virtual ~Run5CuCu62GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun5CuCu62GeV(PHCompositeNode *topNode);
  int  getCentralityByBBCRun5CuCu62GeV(float bbc1, float bbc2, float zdc1, float zdc2, float zvertex, int runno);
  void help();
  void InitArray();

 private:

  float bbcCutZDep[89][16];  // centrality selections in 16 zvertex bins

};

#endif /* __RUN5CUCU62GEVCENTRALITYRECO_H__ */
