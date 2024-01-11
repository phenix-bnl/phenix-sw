/* Automatically generated.  Do not edit. */
#include "mEmcDCMToRawModule.h"
#include "mEmcDCMToRaw.h"
#include "PHIODataNode.h"

#include "dEmcDCMDataWrapper.h"

typedef PHIODataNode<dEmcDCMDataWrapper> dEmcDCMDataNode_t;

#include "dEmcRawDataWrapper.h"

typedef PHIODataNode<dEmcRawDataWrapper> dEmcRawDataNode_t;

PHBoolean
mEmcDCMToRawModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DEMCDCMDATA_ST *d1;
  TABLE_HEAD_ST t2;
  DEMCRAWDATA_ST *d2;

  dEmcDCMDataNode_t* n1 = static_cast<dEmcDCMDataNode_t*>(nl[0]);
  dEmcRawDataNode_t* n2 = static_cast<dEmcRawDataNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = memcdcmtoraw_(
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
