#ifndef __RUN11AUAU019GEVCENTRALITYRECO_H__
#define __RUN11AUAU019GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run11AuAu019GeVCentralityReco : public Recalibrator
{
 public:

  Run11AuAu019GeVCentralityReco();
  virtual ~Run11AuAu019GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByPC1Run11AuAu019(PHCompositeNode *topNode) const;
  int  getCentralityByPC1Run11AuAu019(const int npc1, const float zvertex, const int runno) const;
  int  getCentralityByEMCRun11AuAu019(PHCompositeNode *topNode) const;
  int  getCentralityByEMCRun11AuAu019(const float emcet, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactorPC1(const int runnumber) const;
  float GetScaleFactorEMC(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactorPC1();
  void InitScaleFactorEMC();
  void InitArrayPC1();
  void InitArrayEMC();

 private:

  int centSource;  // 0 = use PC1, 1 = use EMC. Set with CENTSOURCERUN11AUAU19
  float pc1CutZDep[87][60];  // centrality selections in 86 centrality bins (requiring 87 cuts) and 60 zvertex bins
  float emcCutZDep[87][60];  // centrality selections in 86 centrality bins (requiring 87 cuts) and 60 zvertex bins

  float PC1ScaleFactor[1100]; // run-by-run correction scale factors
  float EMCScaleFactor[1100]; // run-by-run correction scale factors
  int   QAstatus[1100];       // include list of runs that passed current QA 
};

#endif /* __RUN11AUAU019GEVCENTRALITYRECO_H__ */
