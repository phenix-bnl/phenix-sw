#ifndef __FCLCALIBANALYZER_H__
#define __FCLCALIBANALYZER_H__

//
//  FclCalibAnalyzier is a Fun4All module used to extract Fcl-Zdc
//  Correlations used for correcting the Zdc Xtalk in the FCAL in d+Au
//  data.
//
//  newby5@llnl.gov
//

#include "SubsysReco.h"

class PHCompositeNode;

class TH2;  //forward dec
class TH1;

class FclCalibAnalyzer: public SubsysReco
{
 public:
  FclCalibAnalyzer(const std::string &name = "FclCalibAnalyzer");
  virtual ~FclCalibAnalyzer() {}

  //  For this analysis we only use Init, process_event, and End;
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  TH2* hFcl2D[2][90] ;
  TH2* hFclZdc[2];
  TH2* hFclBbc;
  TH1* hFclGreyS;
  TH1* hFclTotlS;
  TH2* hFclSChan2D;

};

#endif /* __FCLCALIBANALYZER_H__ */



