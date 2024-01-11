#ifndef __EMC_GEA_CONTAINERIMPORTER_H__
#define __EMC_GEA_CONTAINERIMPORTER_H__





#include <Rtypes.h>

#include <SubsysRecoStack.h>
#include <PHCompositeNode.h>


class EmcGeaContainerImporter: public SubsysRecoStack {
public:
  EmcGeaContainerImporter(PHCompositeNode * root = NULL): SubsysRecoStack("EmcGeaContainerImporter", root){}

  virtual int process_event(PHCompositeNode * root);

  ClassDef(EmcGeaContainerImporter, 0)
};





#endif /* ! __EMC_GEA_CONTAINERIMPORTER_H__ */

