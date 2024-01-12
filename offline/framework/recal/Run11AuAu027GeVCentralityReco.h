#ifndef RUN11AUAU027GEVCENTRALITYRECO_H__
#define RUN11AUAU027GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;

class Run11AuAu027GeVCentralityReco : public Recalibrator
{
 public:

  Run11AuAu027GeVCentralityReco();
  virtual ~Run11AuAu027GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByPC1Run11AuAu027(PHCompositeNode *topNode) const;
  int  getCentralityByPC1Run11AuAu027(const int npc1, const float zvertex, const int runno) const;
  int  getCentralityByEMCRun11AuAu027(PHCompositeNode *topNode) const;
  int  getCentralityByEMCRun11AuAu027(const float emcet, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactorPC1(const int runnumber) const;
  float GetScaleFactorEMC(const int runnumber) const;
  int GetQAStatus(const int runnumber) const;
  void InitScaleFactorPC1();
  void InitScaleFactorEMC();
  void InitArrayPC1();
  void InitArrayEMC();

 private:
  
  int centSource;  // 0 = use PC1, 1 = use EMC. Set with CENTSOURCERUN11AUAU27
  float pc1CutZDep[87][60];  // centrality selections in 86 centrality bins (requiring 87 cuts) and 12 zvertex bins
  float emcCutZDep[87][60];  // centrality selections in 86 centrality bins (requiring 87 cuts) and 12 zvertex bins

  float PC1ScaleFactor[750]; // run-by-run correction scale factors
  float EMCScaleFactor[750]; // run-by-run correction scale factors
  int   QAstatus[750];       // include list of runs that passed current QA 
                               // use enum here to categorize pass,fail,nocheck
};

#endif /* RUN11AUAU027GEVCENTRALITYRECO_H__ */
