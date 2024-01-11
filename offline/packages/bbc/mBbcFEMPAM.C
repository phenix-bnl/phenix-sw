/* Automatically generated.  Do not edit. */
#include "mBbcFEMModule.h"
#include "mBbcFEM.h"
#include "PHIODataNode.h"

#include "dBbcRawWrapper.h"

typedef PHIODataNode<dBbcRawWrapper> dBbcRawNode_t;

#include "dBbcFEMWrapper.h"

typedef PHIODataNode<dBbcFEMWrapper> dBbcFEMNode_t;

PHBoolean
mBbcFEMModule::callPAM(PHPointerList<PHNode> &nl) {
  long result;

  TABLE_HEAD_ST t1;
  DBBCRAW_ST *d1;
  TABLE_HEAD_ST t2;
  DBBCFEM_ST *d2;

  dBbcRawNode_t* n1 = static_cast<dBbcRawNode_t*>(nl[0]);
  dBbcFEMNode_t* n2 = static_cast<dBbcFEMNode_t*>(nl[1]);

  t1 = n1->getData()->TableHeader();
  d1 = n1->getData()->TableData();
  t2 = n2->getData()->TableHeader();
  d2 = n2->getData()->TableData();

  result = mbbcfem_(
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
