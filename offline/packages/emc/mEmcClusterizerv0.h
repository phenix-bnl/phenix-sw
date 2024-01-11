#ifndef __mEmcClusterizerv0_h__
#define __mEmcClusterizerv0_h__

//
// EMC basic clustering class (for PHOOL).
//
// Alexander Bazilevsky Sep-00
// Adapted for dropping of staf table by L. Aphecetche Oct-02

#include "phool.h"
#include <SubsysReco.h>
#include <vector>

class PHCompositeNode;
class mEmcGeometryModule;
class EmcModule;
class emcTowerContainer;
class emcClusterContainer;
class EmcSectorRec;
class EmcPeakarea;
class EmcCluster;
class SecGeom;

/** Module for clusterizing EMCAL. 
@ingroup clustering
*/

class mEmcClusterizerv0: public SubsysReco
{
public:

  mEmcClusterizerv0(mEmcGeometryModule*);
  virtual ~mEmcClusterizerv0();

  void SetMinClusterEnergy(float eClMin)
  { fMinClusterEnergySc=eClMin; fMinClusterEnergyGl=eClMin; }
  void SetMinClusterEnergyPbSc(float eClMin) { fMinClusterEnergySc=eClMin; }
  void SetMinClusterEnergyPbGl(float eClMin) { fMinClusterEnergyGl=eClMin; }
  void SetTowerThreshold(float Thresh);
  void SetTowerThreshold(int is, float Thresh);
  void SetTowerThresholdPbSc(float Thresh);
  void SetTowerThresholdPbGl(float Thresh);
  void SetPeakThreshold(float Thresh);
  void SetPeakThreshold(int is, float Thresh);

  int process_event(PHCompositeNode* root);

protected:

  void ToF_Process( EmcModule* phit, size_t nhits,
		    float dist, EmcModule& hmax,
		    float* ptof,    float* petof,    float* ptofcorr,
		    float* pdtof,
		    float* ptofmin, float* petofmin, float* ptofmincorr,
		    float* ptofmax, float* petofmax, float* ptofmaxcorr,
		    float& tofdisp);

  void fillHitList(const emcTowerContainer&);

  void fillPeakArea(EmcPeakarea&, EmcCluster&, const emcTowerContainer&,
		    int arm, int sector);

protected: 

  std::vector<EmcSectorRec*> fScSector;
  std::vector<EmcSectorRec*> fGlSector;
  std::vector<SecGeom*> fSectorGeometries;
  float fMinClusterEnergySc;
  float fMinClusterEnergyGl;
  
  static const int MAX_SECTORS_PROCESS=8;
  static const int MAX_NUMBER_OF_PEAKS=10;
  static const int HITS_TO_TABLE=16;

  std::vector<EmcModule> HitList[MAX_SECTORS_PROCESS];
  static const int HVECTSIZE=4608;

  EmcModule* fHVect;
  float TowerThresh[MAX_SECTORS_PROCESS];
  int Nx[MAX_SECTORS_PROCESS];

  emcClusterContainer* fClusters;
  std::vector<float> fVertex;

  bool displaywarning;
};

#endif
