#ifndef __RUN5CUCU200GEVCENTRALITYRECO_H__
#define __RUN5CUCU200GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;
class TH1;

class Run5CuCu200GeVCentralityReco : public Recalibrator
{
 public:

  Run5CuCu200GeVCentralityReco();
  virtual ~Run5CuCu200GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  InitRun(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByBBCRun5CuCu(PHCompositeNode *topNode);
  int  getCentralityByBBCRun5CuCu(float bbc1, float bbc2, float zdc1, float zdc2, float zvertex, int runno);
  void help();

  void InitArray1();
  void InitArray2();
  void InitArray3();
  void InitArray4();
  void InitArray5();
  void InitArray6();
  void InitArray7();
  void InitArray8();
  void InitArray9();
  void InitArray10();

 private:

  float bbcCutZDep[95][16];  // centrality selections in 16 zvertex bins
  TH1 *centrality_histo;
};

#endif /* __RUN5CUCU200GEVCENTRALITYRECO_H__ */
