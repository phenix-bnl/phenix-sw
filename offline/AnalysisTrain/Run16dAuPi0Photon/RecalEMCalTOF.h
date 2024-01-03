#ifndef __RecalEMCalTOF_H__
#define __RecalEMCalTOF_H__

#include <math.h>
#include "SubsysReco.h"
#include "emcDCProcessor.h"
#include "emcCalibrationDataHelper.h"
#include "TOAD.h"
#include "TF1.h"
#include "THnSparse.h"


class TH1F;
class THmulf;

class Fun4AllServer;
class recoConsts;
class BunchCross;
class PHGlobal;
class emcClusterContainer;
class emcClusterContent;
class emcTowerContainer;
class emcTowerContent;
class emcCalibrationData;
class emcCalibrationDataHelper;
//class Run16WMap;

class RecalEMCalTOF : public SubsysReco {

public:
  RecalEMCalTOF(int input_flag=0, int debug_flag=0);
  virtual ~RecalEMCalTOF();

  int Init(PHCompositeNode *topNode); // called during intialization
  int InitRun(PHCompositeNode *topNode); // called for first event when run number is known
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) { return 0; }
  int ResetEvent(PHCompositeNode *topNode) { return 0; }
  int End(PHCompositeNode *topNode);
  const char *Name() const { return "RecalEMCalTOF"; }
  //Run16WMap *TwrVeto;
  int runnumber;

private:
  bool getNodes(PHCompositeNode *topNode);
  static const double pi;
  int debug;

protected:
  
  double Walk[24768];
  double Walk2[24768];
  double T0Offset[24768];
  double T0OffsetSigma[24768];
  double SectorOffset[8];  
  TF1 *fafter;

  TH1F *NEW_histo;
    //THnSparse* tdc_adc_6;
  

	  
  TOAD *toad_time;
  
  emcCalibrationDataHelper* fCDH;
  PHGlobal * _phglobal_ptr;
  emcClusterContainer* _emcClusterContainer_ptr;
  emcTowerContainer* _emcTowerContainer_ptr;

  Fun4AllServer *se;
  recoConsts *rc;
};

#endif /* __RecalEMCalTOF_H__ */
