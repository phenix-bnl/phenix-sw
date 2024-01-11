#ifndef __EMC_RECOMODULESIMULATION_YEAR8_H__
#define __EMC_RECOMODULESIMULATION_YEAR8_H__


class PHFlags;


#include <Rtypes.h>

#include <SubsysRecoStack.h>




class EmcRecoModuleSimulationYear8: public SubsysRecoStack {
public:
  EmcRecoModuleSimulationYear8();
  ~EmcRecoModuleSimulationYear8();


  int Init(PHCompositeNode * root);


  ClassDef(EmcRecoModuleSimulationYear8, 0)
};





#endif /* ! __EMC_RECOMODULESIMULATION_YEAR8_H__ */

