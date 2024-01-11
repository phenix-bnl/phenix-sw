#ifndef __mEmcClusterizerv1_h__
#define __mEmcClusterizerv1_h__

//
// EMC basic clustering class (for PHOOL).
//
// Alexander Bazilevsky Sep-00
// Adapted for dropping of staf table by L. Aphecetche Oct-02

#include "mEmcClusterizerv0.h"
#include "emcTowerContainerDST.h"

// the containers for the auxiliary info
#include "emcClusterAuxInfoContainerV1.h"
#include "emcClusterAuxInfoV1.h"


/** Module for clusterizing EMCAL. 
@ingroup clustering
*/

class mEmcClusterizerv1: public mEmcClusterizerv0
{
public:

  mEmcClusterizerv1(mEmcGeometryModule*);
  virtual ~mEmcClusterizerv1(){};


  int process_event(PHCompositeNode* root);
  void fillPeakArea(EmcPeakarea&, EmcCluster&, const emcTowerContainer&,
		    int arm, int sector);

protected: 

  unsigned int emc_cluster_id;
  unsigned int towers_per_event;
  emcTowerContainerDST* fHit;
  emcClusterAuxInfoContainerV1* fAuxInfoContainer;

};

#endif
