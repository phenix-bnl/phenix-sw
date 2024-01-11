/* Automatically generated.  Do not edit. */
#include "mEmcDCMinputModule.h"
#include "mEmcDCMinput.h"
#include "PHIODataNode.h"

#include "dEmcDCMDataWrapper.h"

typedef PHIODataNode<dEmcDCMDataWrapper> dEmcDCMDataNode_t;

PHBoolean
mEmcDCMinputModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCDCMDATA_ST *d1;

  dEmcDCMDataNode_t* n1 = static_cast<dEmcDCMDataNode_t*>(nl[0]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();

  result = memcdcminput_(
    &t1, d1                              );

  n1->getData()->SetRowCount(t1.nok);

  if (result == 1) {
    return True;
  } else {
    return False;
  }
}
