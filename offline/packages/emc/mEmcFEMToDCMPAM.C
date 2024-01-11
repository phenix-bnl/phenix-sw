/* Automatically generated.  Do not edit. */
#include "mEmcFEMToDCMModule.h"
#include "mEmcFEMToDCM.h"
#include "PHIODataNode.h"

#include "dEmcFEMDataWrapper.h"

typedef PHIODataNode<dEmcFEMDataWrapper> dEmcFEMDataNode_t;

#include "dEmcDCMDataWrapper.h"

typedef PHIODataNode<dEmcDCMDataWrapper> dEmcDCMDataNode_t;

PHBoolean
mEmcFEMToDCMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCFEMDATA_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCDCMDATA_ST *d2;

  dEmcFEMDataNode_t* n1 = static_cast<dEmcFEMDataNode_t*>(nl[0]);
  dEmcDCMDataNode_t* n2 = static_cast<dEmcDCMDataNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcfemtodcm_(
    &t1, d1,
    &t2, d2                              );

  n1->getData()->SetRowCount(t1.nok);
  n2->getData()->SetRowCount(t2.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
