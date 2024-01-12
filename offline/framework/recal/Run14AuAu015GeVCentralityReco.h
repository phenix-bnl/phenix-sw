#ifndef RUN14AUAU015GEVCENTRALITYRECO_H__
#define RUN14AUAU015GEVCENTRALITYRECO_H__

#include "Recalibrator.h"

class PHCompositeNode;
class PHGlobal;

class Run14AuAu015GeVCentralityReco : public Recalibrator
{
 public:

  Run14AuAu015GeVCentralityReco();
  virtual ~Run14AuAu015GeVCentralityReco() {}

  int  process_event(PHCompositeNode *topNode);
  int  Init(PHCompositeNode *topNode);
  int  isValidRun(const int runno) const;
  int  getCentralityByPC1Run14AuAu015(PHCompositeNode *topNode) const;
  int  getCentralityByPC1Run14AuAu015(const int npc1, const float zvertex, const int runno) const;
  int  getCentralityByEMCRun14AuAu015(PHCompositeNode *topNode) const;
  int  getCentralityByEMCRun14AuAu015(const float emce, const float zvertex, const int runno) const;
  void help() const;
  float GetScaleFactorPC1(const int runnumber) const;
  float GetScaleFactorEMC(const int runnumber) const;
  int   GetQAStatus(const int runnumber) const;
  void InitScaleFactorPC1();
  void InitScaleFactorEMC();
  void InitArrayPC1();
  void InitArrayEMC();

 private:

  int centSource;  // 0 = use PC1, 1 = use EMC. Set with CENTSOURCERUN14AUAU15
  float pc1CutZDep[85][60];  // centrality selections in 84 centrality bins and 60 zvertex bins
  float emcCutZDep[85][60];  // centrality selections in 84 centrality bins and 60 zvertex bins

  float PC1ScaleFactor[3160]; // run-by-run correction scale factors
  float EMCScaleFactor[3160]; // run-by-run correction scale factors
  int   QAstatus[3160];       // include list of runs that passed current QA 
};

#endif /* RUN14AUAU015GEVCENTRALITYRECO_H__ */
