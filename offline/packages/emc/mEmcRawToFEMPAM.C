/* Automatically generated.  Do not edit. */
#include "mEmcRawToFEMModule.h"
#include "mEmcRawToFEM.h"
#include "PHIODataNode.h"

#include "dEmcRawDataWrapper.h"

typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;

#include "dEmcFEMDataWrapper.h"

typedef PHIODataNode<dEmcFEMDataWrapper> dEmcFEMDataNode_t;

PHBoolean
mEmcRawToFEMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCRAWDATA_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCFEMDATA_ST *d2;

  dEmcRawDataNode_t* n1 = static_cast<dEmcRawDataNode_t*>(nl[0]);
  dEmcFEMDataNode_t* n2 = static_cast<dEmcFEMDataNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcrawtofem_(
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
