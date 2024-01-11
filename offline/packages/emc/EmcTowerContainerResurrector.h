// This recalibrator re-creates the original EmcTowerContainer
// structure found on the older-style PWGs. It reads the EmcTowerContainerDST,
// makes EmcTowers, and provides the user with a tree structure which
// existing analysis code expects.  

#ifndef __EMCTOWERCONTAINERRESURRECTOR_H__
#define __EMCTOWERCONTAINERRESURRECTOR_H__



#include <string>

#include <SubsysReco.h>




class EmcTowerContainerResurrector: public SubsysReco
{
 public:
  EmcTowerContainerResurrector();
  ~EmcTowerContainerResurrector();

  int InitRun(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);


 public:
  std::string _emcHitContainerNodeName;
  std::string _emcTowerContainerNodeName;

};



#endif /* __EMCTOWERCONTAINERRESURRECTOR_H__ */
