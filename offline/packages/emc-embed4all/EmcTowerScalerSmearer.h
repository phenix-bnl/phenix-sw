#ifndef __EMC_TOWERSCALERSMEARER_H__
#define __EMC_TOWERSCALERSMEARER_H__

#include <string>

#include <Rtypes.h>

#include <SubsysReco.h>


class EmcTowerScalerSmearer: public SubsysReco {
public:
  EmcTowerScalerSmearer(float scale, float smear);
  virtual ~EmcTowerScalerSmearer(){}
  

public:
  int Init(PHCompositeNode * root);
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);

  void SetScale(float s0, float s1, float s2, float s3, float s4, float s5, float s6, float s7);
  void SetScale(float pbsc, float pbgl); // shortcut for different scaling for pbsc and pbgl
  void SetScale(float scale); // shortcut for setting all sectors

  void SetSmear(float s0, float s1, float s2, float s3, float s4, float s5, float s6, float s7);
  void SetSmear(float pbsc, float pbgl); // shortcut for different smearing for pbsc and pbgl
  void SetSmear(float smear); // shortcut for setting all sectors

  void SetSmear2(float s0, float s1, float s2, float s3, float s4, float s5, float s6, float s7);
  void SetSmear2(float pbsc, float pbgl); // shortcut for different smearing for pbsc and pbgl
  void SetSmear2(float smear); // shortcut for setting all sectors

  double scale[8], smear[8]; /// scaling and smearing parameter for the 8 emc sectors
  double smear2[8]; // the intrinsic resolution term of EMCal
   

  ClassDef(EmcTowerScalerSmearer, 0)
};


#endif /* !__EMC_TOWERSCALERSMEARER_H__*/
