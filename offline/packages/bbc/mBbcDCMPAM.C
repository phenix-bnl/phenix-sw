/* Automatically generated.  Do not edit. */
#include "mBbcDCMModule.h"
#include "mBbcDCM.h"
#include "PHIODataNode.h"

#include "dBbcFEMWrapper.h"

typedef PHIODataNode<dBbcFEMWrapper> dBbcFEMNode_t;

#include "dBbcDCMWrapper.h"

typedef PHIODataNode<dBbcDCMWrapper> dBbcDCMNode_t;

PHBoolean
mBbcDCMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DBBCFEM_ST *d1;
  TABLE_HEAD_ST t2;
  DBBCDCM_ST *d2;

  dBbcFEMNode_t* n1 = static_cast<dBbcFEMNode_t*>(nl[0]);
  dBbcDCMNode_t* n2 = static_cast<dBbcDCMNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = mbbcdcm_(
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
