// $Author: purschke $
// $Date: 2010/05/07 03:33:54 $
// $Name:  $

// This recalibrator re-creates the original EmcClusterContainer
// structure found on the older-style PWGs. It reads the EmcTowerContainer,
// makes EmcClusters, and provides the user with a tree structure which
// existing analysis code expects.  

#ifndef __EMCCLUSTERCONTAINERRESURRECTOR_H__
#define __EMCCLUSTERCONTAINERRESURRECTOR_H__


#include "PHMatrix.h"
#include "SubsysReco.h"
#include <vector>

#define MAX_SECTORS_PROCESS 8

class emcTowerContainerDST;
class emcClusterAuxInfoContainerV1;
class emcClusterAuxInfo;
class emcTowerContent;
class EmcSectorRec;
class SecGeom;
class EmcModule;
class mEmcGeometryModule;
class PHCompositeNode;
class emcClusterContainer;


class EmcClusterContainerResurrector: public SubsysReco
{
 public:
  EmcClusterContainerResurrector();
  ~EmcClusterContainerResurrector();

  int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
  int process_event(PHCompositeNode *topNode); // your analysis code goes here


 protected:

#ifndef __CINT__


  int digest_cluster ( emcClusterContainer *clustercontainer, std::vector<emcTowerContent*>  &mlist, const int isector, const int current_module, const int n_modules, const float fVertex[3], emcClusterAuxInfo* aux);

  // the pointer to the "0th" tower container
  std::string _emcClusterAuxInfoContainerNodeName;
  //  emcClusterAuxInfoContainerV1* _emcClusterAuxInfoContainer_ptr;

  std::string _emcTowerContainerNodeName;
  emcTowerContainerDST* _emcTowerContainer_ptr;

 
  std::vector<EmcSectorRec*> fSector;
  std::vector<SecGeom*> fSectorGeometries;
  int Nx[MAX_SECTORS_PROCESS];

  mEmcGeometryModule* geom;
  PHMatrix emcrm;
  PHVector emctr;
  //  int already_issued_warning;

#endif


};

#endif /* __EMCCLUSTERCONTAINERRESURRECTOR_H__ */
