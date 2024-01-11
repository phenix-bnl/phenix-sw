#ifndef __mEmcRecoModuleSimulationYear3_h__
#define __mEmcRecoModuleSimulationYear3_h__

#include <SubsysRecoStack.h>
#include "PHTimeStamp.h"
#include "emcManageable.h"

class PHFlag;
class dEmcRespParWrapper;

/** SubsysReco for simulated data.

Valid for Run3 and beyond, unless you find a most recent one ;-)

It only works in the mode PRDF to DST (pisa_simulationflag=2).

It's basically a list of SubsysRecos called in sequence :

- mEmcGeaGetHits
- mEmcGeaParamsModule (first event only)
- mEmcGeaEventModule
- mEmcGeaMakeRawModule
- mEmcGeaTrackModule
- mEmcGeaMakeCalibTower
- mEmcApplyQAToSimu
- mEmcRecoModuleSimulationYear3 (additional dead towers ) Cesar
- mEmcClusterizerv0
- mEmcGeaPatchSimulatedCluster
- mEmcGeaMakeClusterEvaluation (if recoConsts flag EVALUATIONFLAG is present)

*/

class mEmcRecoModuleSimulationYear3 : public SubsysRecoStack
{

public:

  mEmcRecoModuleSimulationYear3(const PHFlag& flags);

  using SubsysRecoStack::process_event;
  using SubsysRecoStack::End;
  virtual const char* getName() const { return Name(); }
  virtual int InitRun(PHCompositeNode*);

private:

  void setupAna(PHCompositeNode*);  
  void setupBadModules(PHCompositeNode*);
  void setupClustering(PHCompositeNode*);
  void setupEvaluation(PHCompositeNode*);
  void setupResponse(dEmcRespParWrapper*);

private:

  bool fEvaluation;
  int fSimulationFlag;

  static const float fgTowerThresholdPbSc;
  static const float fgTowerThresholdPbGl;
  static const float fgMinClusterEnergyPbSc;
  static const float fgMinClusterEnergyPbGl;

  PHTimeStamp fTimeStamp;


  emcManageable::EStorage fDataSource;
};
#endif
