#ifndef __EMCSIMRECO_H__
#define __EMCSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;

class mEmcCalibratorModule     ;
class mEmcCalibTowerModule     ;
class mEmcClusterNewModule     ;
class mEmcDCMToRawModule       ;
class mEmcDefGeomModule        ;
class mEmcEventModule          ;
class mEmcFEMToDCMModule       ;
class mEmcGeaClusterEvalModule ;
class mEmcGeaEventModule       ;
class mEmcGeaMakeRawModule     ;
class mEmcGeaParamsModule      ;
class mEmcGeaTrackModule       ;
class mEmcGeometryModule       ;
class mEmcMIPCorrModule        ;
class mEmcMIPCorr3Module       ;
class mEmcRawToFEMModule       ;	// writes sim raw data to fem
// to be removed, mazsi@bnl.gov // class mEmcRawToLongModule      ;	// writes sim raw data to hit format IDPBSC_FPGA0SUP
class mEmcRealEventModule      ;
class mEmcTOFCorr2Module       ;
class mEmcTOFCorr4Module       ;
class mEmcToolsModule          ;
class EmcSimuRawDataReCal      ;


class EmcSimreco: public SubsysReco
{
 public:
  EmcSimreco(const std::string &name = "EMC");
  virtual ~EmcSimreco() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int copyWrapper(PHCompositeNode *);

  mEmcCalibratorModule     *mEmcCalibrator     ;
  mEmcCalibTowerModule     *mEmcCalibTower     ;
  mEmcClusterNewModule     *mEmcClusterNew     ;
  mEmcDCMToRawModule       *mEmcDCMToRaw       ;
  mEmcDefGeomModule        *mEmcDefGeom        ;
  mEmcEventModule          *mEmcEvent          ;
  mEmcFEMToDCMModule       *mEmcFEMToDCM       ;
  mEmcGeaClusterEvalModule *mEmcGeaClusterEval ;
  mEmcGeaEventModule       *mEmcGeaEvent       ;
  mEmcGeaMakeRawModule     *mEmcGeaMakeRaw     ;
  mEmcGeaParamsModule      *mEmcGeaParams      ;
  mEmcGeaTrackModule       *mEmcGeaTrack       ;
  mEmcGeometryModule       *mEmcGeometry       ;
  mEmcMIPCorrModule        *mEmcMIPCorr        ;
  mEmcMIPCorr3Module       *mEmcMIPCorr3       ;
  mEmcRawToFEMModule       *mEmcRawToFEM       ;	// writes sim raw data to fem
// to be removed, mazsi@bnl.gov //   mEmcRawToLongModule      *mEmcRawToLong      ;	// writes sim raw data to hit format IDPBSC_FPGA0SUP
  mEmcRealEventModule      *mEmcRealEvent      ;
  mEmcTOFCorr2Module       *mEmcTOFCorr        ;
  mEmcTOFCorr4Module       *mEmcTOFCorr4       ;
  mEmcToolsModule          *mEmcTools          ;
  EmcSimuRawDataReCal      *mEmcSimuRawReCal   ;


  int runnumber;
};

#endif /* __EMCSIMRECO_H__ */
