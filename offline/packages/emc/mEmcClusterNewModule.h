#ifndef __MEMCCLUSTERNEWMODULE_H__
#define __MEMCCLUSTERNEWMODULE_H__

//
// EMC basic clustering class (for PHOOL).
//
// Alexander Bazilevsky Sep-00
//

#include "phool.h"
#include <SubsysReco.h>
#include "EmcScSectorRec.h"
#include "EmcGlSectorRec.h"

class PHCompositeNode;
class mEmcGeometryModule;
class EmcModule;
/** (OLD) Module for clusterizing EMCAL. 
\deprecated With the removal of the usage of STAF tables, see now mEmcClusterizerv0

@ingroup deprecated
@ingroup clustering
*/

class mEmcClusterNewModule: public SubsysReco {

public:

  mEmcClusterNewModule(mEmcGeometryModule*, int runnumber=0);
  virtual ~mEmcClusterNewModule(){}
  void SetMinClusterEnergy(float eClMin){ fMinClusterEnergySc=eClMin; fMinClusterEnergyGl=eClMin; }
  void SetMinClusterEnergyPbSc(float eClMin){ fMinClusterEnergySc=eClMin; }
  void SetMinClusterEnergyPbGl(float eClMin){ fMinClusterEnergyGl=eClMin; }
  void SetTowerThreshold(float Thresh);
  void SetTowerThreshold(int is, float Thresh);
  void SetTowerThresholdPbSc(float Thresh);
  void SetTowerThresholdPbGl(float Thresh);
  void SetPeakThreshold(float Thresh);
  void SetPeakThreshold(int is, float Thresh);
  int process_event(PHCompositeNode * root);
  void ToF_Process( EmcModule* phit, float dist, EmcModule& hmax,
		    float* ptof,    float* petof,    float* ptofcorr,
		    float* pdtof,
		    float* ptofmin, float* petofmin, float* ptofmincorr,
		    float* ptofmax, float* petofmax, float* ptofmaxcorr );

private: 

  EmcScSectorRec ScSector[6];
  EmcGlSectorRec GlSector[2];
  float fMinClusterEnergySc;
  float fMinClusterEnergyGl;
  int fRunNumber;
};

#endif /*__MEMCCLUSTERNEWMODULE_H__*/
